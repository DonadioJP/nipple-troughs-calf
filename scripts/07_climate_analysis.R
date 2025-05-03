# 1. SETUP and DATA PREP----
source("~/nipple-troughs-calf/scripts/00_config.R")
source("~/nipple-troughs-calf/scripts/01_data_cleaning.R")

## 1.1 Climate data ----
climate <- cleaned_data$climate
view(climate)
names(climate)
str(climate)
climate <- climate %>%
  mutate(Date = as.Date(Date))

# Aggregate climate data to daily averages
daily_climate <- climate %>%
  group_by(Date) %>%
  summarise(
    Temp_mean = mean(Temp, na.rm = TRUE),
    Temp_max = max(Temp, na.rm = TRUE),
    Temp_min = min(Temp, na.rm = TRUE),
    RH_mean = mean(RH, na.rm = TRUE),
    Prec_sum = sum(Prec, na.rm = TRUE),
    WS_mean = mean(WS, na.rm = TRUE),
    ITU_Thom_mean = mean(`ITU - Thom`, na.rm = TRUE),
    ITU_Berman_mean = mean(`ITU - Berman`, na.rm = TRUE)
  )

## 1.2 Experimental days data ----
experimental_days <- cleaned_data$growth %>% 
  select(Group, id, Day_birth, Weaning_day)
names(experimental_days)
str(experimental_days)
experimental_days <- experimental_days %>%
  mutate(Day_birth = as.Date(Day_birth),
         Weaning_day = as.Date(Weaning_day))

# Calculate experimental period for each group
experimental_periods <- experimental_days %>%
  rowwise() %>%
  mutate(experimental_days = list(seq(Day_birth, Weaning_day, by = "day"))) %>%
  unnest(experimental_days) %>%
  select(Group, id, Date = experimental_days)

# Joining climate with experimental periods
experimental_climate <- experimental_periods %>%
  left_join(daily_climate, by = "Date")

# Missing values
missing_data <- experimental_climate %>%
  summarise(across(everything(), ~sum(is.na(.))))

print(missing_data)

# 2. EXPLORATORY ANALYSIS ----
## 2.1 Group-level summary statistics----
group_summary <- experimental_climate %>%
  group_by(Group) %>%
  summarise(
    across(where(is.numeric),
           list(mean = ~mean(., na.rm = TRUE),
                sd = ~sd(., na.rm = TRUE),
                min = ~min(., na.rm = TRUE),
                max = ~max(., na.rm = TRUE),
                median = ~median(., na.rm = TRUE),
                q25 = ~quantile(., 0.25, na.rm = TRUE),
                q75 = ~quantile(., 0.75, na.rm = TRUE)),
           .names = "{.col}_{.fn}"
    ),
    n_days = n(),
    .groups = 'drop'
  )

print(group_summary, width = Inf)

## 2.2 Monthly summary statistics -----
monthly_summary <- experimental_climate %>%
  mutate(YearMonth = format(Date, "%Y-%m")) %>%
  group_by(YearMonth) %>%
  summarise(
    across(c(Temp_mean, RH_mean, Prec_sum, WS_mean, ITU_Thom_mean),
           list(mean = mean, sd = sd, min = min, max = max),
           na.rm = TRUE),
    n_days = n(),
    heat_stress_days = sum(ITU_Thom_mean > 72, na.rm = TRUE),
    .groups = 'drop'
  )

print(monthly_summary)

## 2.3 Extreme weather ----
# Define thresholds 
thi_threshold <- 78 # Calf heat stress threshold

# Extreme days analysis
extreme_days <- experimental_climate %>%
  mutate(
    heat_stress_day = ITU_Thom_mean > thi_threshold,
    rainy_day = Prec_sum > 10,  # >10mm considered a rainy day
    windy_day = WS_mean > 5  # >5m/s considered a windy day
  ) %>%
  group_by(Group) %>%
  summarise(
    total_days = n(),
    heat_stress_days = sum(heat_stress_day, na.rm = TRUE),
    rainy_days = sum(rainy_day, na.rm = TRUE),
    windy_days = sum(windy_day, na.rm = TRUE),
    .groups = 'drop'
  ) %>%
  mutate(
    heat_stress_prop = heat_stress_days / total_days  )

print(extreme_days)

# Detailed extreme days listing
extreme_days_detail <- experimental_climate %>%
  filter(ITU_Thom_mean > thi_threshold) %>%
  select(Date, Group, Temp_mean, Temp_min, ITU_Thom_mean, Prec_sum, WS_mean) %>%
  arrange(desc(ITU_Thom_mean))

print(extreme_days_detail)

## 2.4 Correlation analysis ----
# matrix
correlation_matrix <- experimental_climate %>%
  select(Temp_mean, RH_mean, Prec_sum, WS_mean, ITU_Thom_mean, ITU_Berman_mean) %>%
  cor(use = "complete.obs")

print(correlation_matrix)

# Visual correlation plot
corrplot(correlation_matrix, method = "color", type = "upper",
         tl.col = "black", tl.srt = 45, addCoef.col = "black")

## 2.5 Heat waves ----
# Identify heat waves (3+ consecutive days above threshold)
heat_waves <- experimental_climate %>%
  mutate(heat_day = ITU_Thom_mean > thi_threshold) %>%
  group_by(Group, grp = cumsum(heat_day != lag(heat_day, default = first(heat_day)))) %>%
  filter(heat_day) %>%
  group_by(Group, grp) %>%
  filter(n() >= 3) %>%  # At least 3 consecutive days
  summarise(
    start_date = min(Date),
    end_date = max(Date),
    duration = n(),
    mean_thi = mean(ITU_Thom_mean),
    max_thi = max(ITU_Thom_mean),
    .groups = 'drop'
  )

print(heat_waves)

# 4. VISUALIZATION ----
# 4.1 Daily temperature, humidity, precipitation... ----
# Daily temperature during experimental periods
ggplot(experimental_climate, aes(x = Date, y = Temp_mean, color = Group)) +
  geom_line(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Daily Temperature During Experimental Periods",
       y = "Temperature (°C)", x = "Date") +
  theme_minimal() +
  scale_x_date(labels = scales::date_format("%b %Y"))

# Boxplot of temperature by group
ggplot(experimental_climate, aes(x = Group, y = Temp_mean, fill = Group)) +
  geom_boxplot() +
  labs(title = "Temperature Distribution by Experimental Group",
       y = "Temperature (°C)") +
  theme_minimal()

# RH plot
ggplot(experimental_climate, aes(x = Date, y = RH_mean, color = Group)) +
  geom_line(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE) +
  labs(title = "Daily Relative Humidity During Experimental Periods",
       y = "Relative Humidity (%)", x = "Date") +
  theme_minimal()

# Precipitation plot
ggplot(experimental_climate, aes(x = Date, y = Prec_sum, fill = Group)) +
  geom_col(position = "dodge") +
  labs(title = "Daily Precipitation During Experimental Periods",
       y = "Precipitation (mm)", x = "Date") +
  theme_minimal()

## 4.2 Temperature Humidity Index plots -----
ggplot(experimental_climate, aes(x = ITU_Thom_mean, fill = Group)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribution of Thermal Comfort Index (ITU - Thom) by Group",
       x = "Thermal Comfort Index", y = "Density") +
  theme_minimal()

ggplot(experimental_climate, aes(x = Temp_mean, y = ITU_Thom_mean, color = Group)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Temperature vs. Thermal Comfort Index",
       x = "Temperature (°C)", y = "Thermal Comfort Index") +
  theme_minimal()

ggplot(experimental_climate, aes(x = Date, y = ITU_Thom_mean, color = Group)) +
  geom_line(size = 1, alpha = 0.8) +
  geom_point(aes(fill = Group), size = 1.5, shape = 21, color = "white", stroke = 0.5) +
  scale_color_brewer(palette = "Dark2", name = "Group") +
  scale_fill_brewer(palette = "Dark2", guide = "none") +
  scale_x_date(
    labels = scales::date_format("%d-%b"),
    breaks = "2 weeks",
    minor_breaks = "1 week",
    expand = expansion(add = c(2, 2))
  ) +
  geom_hline(yintercept = 72, linetype = "dashed", color = "red", alpha = 0.5) +
  labs(
    y = "THI", 
    x = "Date (Day-Month)",
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    legend.title = element_text(face = "bold"),
    panel.grid.major = element_line(color = "grey90"),
    panel.grid.minor = element_line(color = "grey95"),
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "grey40"),
    axis.title.y = element_text(color = "darkred", margin = margin(r = 10)),
    axis.text.x = element_text(angle = 45, hjust = 1)
  ) +
  facet_wrap(~Group, ncol = 4) 

## 4.3 Extreme days plots ----
# Identify days with extreme weather
extreme_days_plot <- experimental_climate %>%
  filter(Temp_mean > quantile(Temp_mean, 0.95, na.rm = TRUE) | 
           Prec_sum > quantile(Prec_sum, 0.95, na.rm = TRUE))

# View extreme days
print(extreme_days_plot %>% select(Date, Group, Temp_mean, Prec_sum) %>% arrange(Date))

# Plot extreme temperature days
ggplot(extreme_days_plot, aes(x = Date, y = Temp_mean, color = Group)) +
  geom_point(size = 3) +
  labs(title = "Extreme Temperature Days During Experimental Periods",
       y = "Temperature (°C)", x = "Date") +
  theme_minimal()

## 4.4 Heat waves ----
heat_wave_timeline <- ggplot(heat_waves) +
  geom_segment(aes(x = start_date, xend = end_date, 
                   y = Group, yend = Group,
                   color = mean_thi), 
               size = 5, alpha = 0.8) +
  geom_text(aes(x = start_date + (end_date - start_date)/2, 
                y = Group, 
                label = duration),
            color = "white", size = 3) +
  scale_color_gradient(low = "#ffeda0", high = "#f03b20",
                       name = "Mean THI") +
  labs(title = "Heat Wave Events by Group",
       subtitle = "Segments show duration (labeled), color shows intensity",
       x = "Date", y = "") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank())

print(heat_wave_timeline)

# First extract all months in experimental period

exp_dates <- experimental_periods %>% 
  summarise(
    min_date = min(Date, na.rm = TRUE),
    max_date = max(Date, na.rm = TRUE)
  )

all_months <- seq(
  floor_date(exp_dates$min_date, "month"),
  ceiling_date(exp_dates$max_date, "month"),
  by = "month"
)

# heat wave plot
heat_wave_timeline <- ggplot(heat_waves) +
  geom_segment(
    aes(x = start_date, xend = end_date, 
        y = Group, yend = Group,
        color = mean_thi), 
    size = 5, alpha = 0.8, lineend = "round"
  ) +
  geom_text(
    aes(x = start_date + (end_date - start_date)/2, 
        y = Group, 
        label = duration),
    color = "white", size = 3.5, fontface = "bold"
  ) +
  scale_color_gradient(
    low = "#ffeda0", high = "#f03b20",
    name = "Mean THI"
  ) +
  scale_x_date(
    breaks = all_months,
    labels = scales::date_format("%b %Y"),
    limits = c(min(all_months), max(all_months)),
    expand = expansion(add = 15)
  ) +
  labs(
    title = "Heat Wave Events by Group",
    subtitle = "Segments show duration (days), color shows THI intensity",
    x = "", y = ""
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
    legend.position = "bottom",
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(color = "gray40")
  )

print(heat_wave_timeline)

