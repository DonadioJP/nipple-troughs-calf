# 1. SETUP ----
source("scripts/00_config.R")
library(lubridate) # For date handling

# 2. DATA PREP ----
climate <- cleaned_data$climate |> 
  mutate(
    Date = ymd(Date),
    Hour = factor(Hour, levels = 0:23),
    Month = month(Date, label = TRUE)
  )

# 3. ANALYSIS ----
## 3.1 THI Trend Analysis ----
thi_model <- lm(THI ~ Date + Hour, data = climate)

## 3.2 Extreme THI Days ----
extreme_days <- climate |> 
  group_by(Date) |> 
  summarise(mean_thi = mean(THI)) |> 
  filter(mean_thi > 72) # Threshold for heat stress

# 4. VISUALIZATION ----
## 4.1 Daily THI Trend ----
thi_trend_plot <- climate |> 
  group_by(Date) |> 
  summarise(mean_thi = mean(THI)) |> 
  ggplot(aes(Date, mean_thi)) +
  geom_line(color = "#1f78b4") +
  geom_smooth(method = "lm", color = "#33a02c") +
  labs(title = "Daily THI Trend", x = "Date", y = "Mean THI") +
  theme_custom()

## 4.2 Hourly THI Pattern ----
hourly_plot <- climate |> 
  group_by(Hour) |> 
  summarise(mean_thi = mean(THI)) |> 
  ggplot(aes(Hour, mean_thi)) +
  geom_col(fill = "#a6cee3") +
  labs(title = "Hourly THI Pattern", x = "Hour", y = "THI") +
  theme_custom()

# 5. SAVE OUTPUTS ----
saveRDS(thi_model, "outputs/climate/thi_model.rds")
ggsave("figures/climate/thi_trend.png", thi_trend_plot, width = 8, height = 5)
ggsave("figures/climate/hourly_thi.png", hourly_plot, width = 6, height = 5)

# 6. REPORTING ----
sink("outputs/climate/summary.txt")
cat("THI TREND MODEL SUMMARY:\n")
print(summary(thi_model))
cat("\n\nEXTREME THI DAYS (>72):\n")
print(extreme_days)
sink()