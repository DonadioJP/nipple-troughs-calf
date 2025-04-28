# 1. SETUP ----
source("scripts/00_config.R") 
library(lubridate)  # Time handling
library(chron)      # Day/night periods

# 2. DATA PREP ----
water <- cleaned_data$water_events |> 
  mutate(
    # Time features
    DateTime = ymd_hms(paste(Date, Time)),
    Period = ifelse(hours(DateTime) %in% 7:19, "Day", "Night"),
    # Visit characteristics
    Visit_Duration = as.numeric(Duration)
  ) |> 
  left_join(cleaned_data$climate[c("Date", "Hour", "THI")], 
            by = c("Date", "Hour"))

# 3. ANALYSIS ----
## 3.1 Visit Frequency ----
visit_model <- glmer(
  Visit_Count ~ Treatment * Period + (1 | id) + (1 | Week),
  data = water |> group_by(id, Date, Treatment, Period) |> tally(name = "Visit_Count"),
  family = poisson
)

## 3.2 Duration Analysis ----
duration_model <- lmer(
  log(Visit_Duration) ~ Treatment * THI + (1 | id),
  data = water
)

## 3.3 THI Interaction ----
thi_model <- glmer(
  Visit_Count ~ Treatment * THI + (1 | id),
  data = water,
  family = poisson
)

# 4. DIAGNOSTICS ----
performance::check_model(duration_model)  
simulateResiduals(thi_model) |> plot()

# 5. VISUALIZATION ----
## 5.1 Day/Night Comparison ----
period_plot <- water |> 
  group_by(id, Treatment, Period) |> 
  summarise(Visits = n()) |> 
  ggplot(aes(Period, Visits, fill = Treatment)) +
  geom_boxplot() +
  scale_fill_manual(values = treatment_palette) +
  labs(title = "Daily Visit Frequency by Period") +
  theme_custom()

## 5.2 THI Relationship ----
thi_plot <- water |> 
  ggplot(aes(THI, Visit_Duration, color = Treatment)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  scale_color_manual(values = treatment_palette) +
  labs(title = "Water Visit Duration vs THI") +
  theme_custom()

# 6. SAVE OUTPUTS ----
saveRDS(visit_model, "outputs/water/visit_frequency_model.rds")
ggsave("figures/water/day_night_visits.png", period_plot, width = 7, height = 5)

# 7. REPORTING ----
sink("outputs/water/analysis_report.txt")
cat("=== VISIT FREQUENCY MODEL ===\n")
print(summary(visit_model))
cat("\n\n=== DURATION-THI RELATIONSHIP ===\n")
print(summary(duration_model))
sink()
