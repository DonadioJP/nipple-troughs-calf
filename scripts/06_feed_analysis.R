# 1. SETUP and DATA PREP ----
source("~/nipple-troughs-calf/scripts/00_config.R")
source("~/nipple-troughs-calf/scripts/01_data_cleaning.R")

feed <- cleaned_data$feed
names(feed)
str(feed)

# Remove outliers
boxplot(feed$Duration)
feed <- feed %>%
  filter(feed$Duration < 10)  
    
# 2. EXPLORATORY ANALYSIS ----
## 2.0 Data Overview ----
visit_freq_stats <- feed |> 
  group_by(Group, Week, Treatment) |> 
  tally(name = "Visit_Count") |> 
  group_by(Treatment) |> 
  get_summary_stats(Visit_Count, type = "full") |> 
  mutate(CV = (sd/mean)*100)

visit_freq_stats

duration_stats <- feed |> 
  group_by(Treatment) |> 
  get_summary_stats(Duration, type = "full") |> 
  mutate(CV = (sd/mean)*100)

duration_stats

## 2.1 Treatment Balance ----
treatment_counts <- feed %>% 
  distinct(Group, Treatment) %>%
  count(Treatment) %>%
  mutate(percentage = n/sum(n)*100)

treatment_counts

## 2.2 Hourly Distribution ----
hourly_distribution <- feed %>%
  mutate(Hour_Cat = cut(Hour, breaks = c(6, 12, 16), 
                        labels = c("Morning", "Afternoon"))) %>%
  count(Treatment, Hour_Cat) %>%
  group_by(Treatment) %>%
  mutate(percentage = n/sum(n)*100)

hourly_distribution

## 2.3 Visits distribution ----
visit_stats <- feed |> 
  group_by(Group, Week, Treatment) |> 
  tally(name = "Visit_Count") |> 
  group_by(Treatment) |> 
  summarise(
    mean_visits = mean(Visit_Count),
    sd_visits = sd(Visit_Count),
    median_visits = median(Visit_Count),
    min_visits = min(Visit_Count),
    max_visits = max(Visit_Count),
    .groups = 'drop'
  )

visit_stats

## 2.4 Weekly visit patterns ----
weekly_visits <- feed |> 
  group_by(Group, Week, Treatment) |> 
  tally(name = "Visit_Count") |> 
  group_by(Week, Treatment) |> 
  summarise(
    avg_visits = mean(Visit_Count),
    total_visits = sum(Visit_Count),
    .groups = 'drop'
  )

weekly_visits

## 2.5 Group Variation ----
group_variation <- feed |> 
  group_by(Group, Week, Treatment) |> 
  tally(name = "Visit_Count") |> 
  group_by(Group, Treatment) |> 
  summarise(
    avg_daily_visits = mean(Visit_Count),
    cv_visits = sd(Visit_Count)/mean(Visit_Count)*100,
    .groups = 'drop'
  )

group_variation

## 2.6 Duration Distribution ----
duration_stats <- feed %>%
  group_by(Treatment) %>%
  summarise(
    mean_dur = mean(Duration, na.rm = TRUE),
    sd_dur = sd(Duration, na.rm = TRUE),
    median_dur = median(Duration, na.rm = TRUE),
    .groups = 'drop'
  )

duration_stats

## 2.7 Weekly Patterns ----
weekly_patterns <- feed %>%
  group_by(Week, Treatment) %>%
  summarise(
    avg_duration = mean(Duration, na.rm = TRUE),
    n_visits = n(),
    .groups = 'drop'
  )

weekly_patterns

## 2.8 Sex durations ----
sex_effects <- feed %>%
  group_by(Sex, Treatment) %>%
  summarise(
    avg_duration = mean(Duration, na.rm = TRUE),
    .groups = 'drop'
  )
sex_effects

## 2.6 Visual Exploration ----

# Total visits per treatment
feed |> 
  group_by(Group, Week, Treatment) |> 
  tally(name = "Visit_Count") |> 
  ggplot(aes(Treatment, Visit_Count, fill = Treatment)) +
  geom_boxplot() +
  labs(title = "Feed Visits per Group-Week by Treatment",
       y = "Visit Count") +
  theme_minimal() +
  theme(legend.position = "none")

# Weekly trends
weekly_visits |> 
  ggplot(aes(Week, avg_visits, color = Treatment)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  labs(title = "Weekly Feed Visit Trends",
       y = "Average Visits per Group") +
  theme_minimal()

# Group variability
group_variation |> 
  ggplot(aes(x = Group, y = avg_daily_visits, fill = Treatment)) +
  geom_col(position = "dodge") +
  labs(title = "Average Weekly Visits by Group",
       y = "Mean Visits") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Visit distribution
feed |> 
  group_by(Group, Week, Treatment) |> 
  tally(name = "Visit_Count") |> 
  ggplot(aes(Visit_Count, fill = Treatment)) +
  geom_histogram(binwidth = 1, alpha = 0.7, position = "identity") +
  labs(title = "Distribution of Weekly Visit Counts",
       x = "Visits per Group-Week") +
  theme_minimal()

# Duration by Treatment
ggplot(feed, aes(Treatment, Duration, fill = Treatment)) +
  geom_boxplot() +
  labs(title = "Feeding Duration by Treatment", y = "Duration (seconds)") +
  theme_minimal()

# Weekly trends
ggplot(weekly_patterns, aes(Week, avg_duration, color = Treatment)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Weekly Feeding Duration Trends", y = "Mean Duration (s)") +
  theme_minimal()

ggplot(feed, aes(x = Treatment, fill = Treatment)) +
  geom_bar() +
  labs(title = "Total Feeding Events by Treatment",
       y = "Number of Visits") +
  theme_minimal() +
  theme(legend.position = "none")

# 3. ANALYSIS ----
## 3.1 Visit Frequency ----
### 3.1.1 Model Selection-----
m1_visit <- glm(
  Visit_Count ~ Treatment * Week,
  data = feed |> group_by(Group, Week, Treatment) |> tally(name = "Visit_Count"),
  family = poisson
)

m2_visit <- glmer(
  Visit_Count ~ Treatment * Week + (1 | Group),
  data = feed |> group_by(Group, Week, Treatment) |> tally(name = "Visit_Count"),
  family = poisson
)

#### 3.1.2 Model Comparison ----
visit_models <- list(
  "Fixed Effects" = m1_visit,
  "+ Random Effects" = m2_visit
)

aic_visit <- map_dfr(seq_along(visit_models), ~ {
  tibble(
    Model = names(visit_models)[.x],
    AIC = AIC(visit_models[[.x]]),
    BIC = BIC(visit_models[[.x]]),
    logLik = as.numeric(logLik(visit_models[[.x]])))
}) |> arrange(AIC)
  
aic_visit
  
### 3.1.3 Best Model Analysis----
best_visit_name <- aic_visit$Model[which.min(aic_visit$AIC)]
best_visit <- visit_models[[best_visit_name]]
summary(best_visit)

### 3.1.4 Post-hoc Analysis----
emm_visit <- emmeans(best_visit, ~ Treatment * Week)
pairs(emm_visit, adjust = "tukey")

### 3.1.5 Diagnostics ----
# Residuals vs Fitted
plot(residuals(best_visit) ~ fitted(best_visit))
abline(h = 0, col = "red")
qqnorm(residuals(best_visit))
qqPlot(residuals(best_visit))

## 3.2 Duration Analysis ----
### 3.2.1 Model Selection ----
m1_dur <- lmer(
  Duration ~ Treatment + (1 | Group),
  data = feed
)

m2_dur <- lmer(
  Duration ~ Treatment * Week + (1 | Group),
  data = feed
)

m3_dur <- lmer(
  Duration ~ Treatment * Week + Sex + (1 | Group),
  data = feed
)

#### 3.2.2 Model Comparison ----
dur_models <- list(
  "Base" = m1_dur,
  "+ Week Interaction" = m2_dur,
  "+ Sex" = m3_dur
)

aic_dur <- map_dfr(seq_along(dur_models), ~ {
  tibble(
    Model = names(dur_models)[.x],
    AIC = AIC(dur_models[[.x]]),
    BIC = BIC(dur_models[[.x]]))
}) |> arrange(AIC)
  
aic_dur

### 3.2.3 Best Model Analysis ----
best_dur_name <- aic_dur$Model[which.min(aic_dur$AIC)]
best_dur <- dur_models[[best_dur_name]]
summary(best_dur)
Anova(best_dur, type = 3)

### 3.2.4 Post-hoc Analysis ----
emm_dur <- emmeans(best_dur, ~ Treatment)
pairs(emm_dur, adjust = "tukey")

### 3.2.5 Diagnostics ----
plot(residuals(best_dur) ~ fitted(best_dur))
abline(h = 0, col = "red")
qqnorm(residuals(best_dur))
qqPlot(residuals(best_dur))


# 5. VISUALIZATION ----
## 5.1 Weekly Visit Patterns ----
weekly_visits <- ggplot(pred_visits, aes(x = as.numeric(as.character(group)), y = predicted, color = x)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = x), alpha = 0.2) +
  labs(title = "Weekly Feed Visit Trends by Treatment",
       x = "Week",
       y = "Predicted Visits",
       color = "Treatment",
       fill = "Treatment") +
  theme_minimal()

weekly_visits

## 5.2 Visits effects ----
pred_visits <- ggpredict(best_visit, terms = c("Treatment", "Week"))

ggplot(pred_visits, aes(x = x, y = predicted, color = group)) +
  geom_boxplot(data = feed |> group_by(Group, Week, Treatment) |> tally(name = "Visit_Count"), 
               aes(Treatment, Visit_Count), inherit.aes = FALSE) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.2, position = position_dodge(width = 0.5)) +
  labs(title = "Feed Visits: Treatment Effects Across Weeks",
       y = "Predicted Visit Count",
       x = "Treatment",
       color = "Week") +
  theme_minimal() +
  theme(legend.position = "top")


## 5.2 Duration Distribution ----
duration_plot <- feed |>
  ggplot(aes(Duration, fill = Treatment)) +
  geom_density(alpha = 0.6) +
  geom_vline(xintercept = mean(feed$Duration), linetype = "dashed") +
  scale_fill_manual(values = treatment_palette) +
  labs(title = "Feed Visit Duration Distribution",
       x = "Duration (minutes)", y = "Density") +
  theme_custom()

duration_plot
