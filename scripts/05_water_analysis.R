# 1. SETUP and DATA PREP ----
source("~/nipple-troughs-calf/scripts/00_config.R")

water <- cleaned_data$water_visits
names(water)
str(water)

water_intake <- cleaned_data$water_intake
names(water_intake)
str(water_intake)

water_intake$Time_day <- factor(water_intake$Time_day,
                                levels = c("Diurno", "Noturno"),
                                labels = c("Day-time", "Night-time"))

# 2. EXPLORATORY ANALYSIS ----
## 2.1 Treatment Balance ----
treatment_counts <- water_intake %>% 
  distinct(Group, Treatment) %>%
  count(Treatment) %>%
  mutate(percentage = n/sum(n)*100)

treatment_counts

## 2.2 Time Period Distribution ----
time_distribution <- water_intake %>%
  count(Treatment, Time_day) %>%
  group_by(Treatment) %>%
  mutate(percentage = n/sum(n)*100)

time_distribution

## 2.3 Intake Distribution ----
intake_distribution <- water_intake %>%
  group_by(Treatment, Time_day) %>%
  summarise(
    n_obs = n(),
    mean_intake = mean(Intake_calf, na.rm = TRUE),
    sd_intake = sd(Intake_calf, na.rm = TRUE),
    median_intake = median(Intake_calf, na.rm = TRUE),
    min_intake = min(Intake_calf, na.rm = TRUE),
    max_intake = max(Intake_calf, na.rm = TRUE),
    .groups = 'drop'
  )

intake_distribution

## 2.4 Weekly Patterns ----
weekly_patterns <- water_intake %>%
  group_by(Week, Treatment) %>%
  summarise(
    avg_intake = mean(Intake_calf, na.rm = TRUE),
    avg_thi = mean(THI, na.rm = TRUE),
    .groups = 'drop'
  )

weekly_patterns

## 2.5 THI Relationship ----
thi_analysis <- water_intake %>%
  group_by(Treatment, Time_day) %>%
  summarise(
    correlation = cor(Intake_calf, THI, use = "complete.obs"),
    .groups = 'drop'
  )

thi_analysis

## 2.6 Group Size Effects ----
group_size_effects <- water_intake %>%
  group_by(Group_Size, Treatment) %>%
  summarise(
    mean_intake = mean(Intake_group, na.rm = TRUE),
    .groups = 'drop'
  )

group_size_effects

## 2.7 Visual Explorations ----

### Treatment vs Intake
ggplot(water_intake, aes(Treatment, Intake_calf, fill = Time_day)) +
  geom_boxplot() +
  labs(title = "Water Intake by Treatment and Time of Day",
       y = "Intake per calf (L)") +
  theme_minimal()

### Weekly Trends
ggplot(weekly_patterns, aes(Week, avg_intake, color = Treatment)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Weekly Intake Trends",
       y = "Average intake per calf (L)") +
  theme_minimal()

### THI Relationship
ggplot(water_intake, aes(THI, Intake_calf, color = Treatment)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap(~Time_day) +
  labs(title = "Water Intake vs THI",
       y = "Intake per calf (L)",
       x = "Temperature-Humidity Index") +
  theme_minimal()

### Group Size Impact
ggplot(water_intake, aes(factor(Group_Size), Intake_group, fill = Treatment)) +
  geom_boxplot() +
  labs(title = "Water Intake by Group Size",
       x = "Number of calves in group",
       y = "Intake per calf (L)") +
  theme_minimal()


# 3. VISIT FREQUENCY ANALYSIS ----
### 3.1 Descriptive Statistics ----
visit_freq_stats <- water |> 
  group_by(Group, Week, Treatment) |> 
  tally(name = "Visit_Count") |> 
  group_by(Treatment) |> 
  get_summary_stats(Visit_Count, type = "full") |> 
  mutate(CV = (sd/mean)*100)

view(visit_freq_stats)

### 3.2 Model Selection ----
# Base model
m1_visit <- glmer(
  Visit_Count ~ Treatment * Week,
  data = water |> group_by(Group, Week, Treatment) |> tally(name = "Visit_Count"),
  family = poisson
)

# Add week random effect
m2_visit <- glmer(
  Visit_Count ~ Treatment * Week + (1 | Group),
  data = water |> group_by(Group, Treatment, Week) |> tally(name = "Visit_Count"),
  family = poisson
)

# Compare models
visit_models <- list(
  "* Week" = m1_visit,
  "+(1|Group)" = m2_visit
)

aic_visit <- map_dfr(seq_along(visit_models), ~ {
  tibble(
    Model = names(visit_models)[.x],
    Formula = deparse(formula(visit_models[[.x]])),
    AIC = AIC(visit_models[[.x]]),
    BIC = BIC(visit_models[[.x]]),
    logLik = as.numeric(logLik(visit_models[[.x]]))
  )
}) |> arrange(AIC)

print(aic_visit)

### 3.3 Best Model Analysis ----
best_visit <- visit_models[[which.min(aic_visit$AIC)]]
summary(best_visit)
anova(best_visit)

### 3.4 Post-hoc Analysis ----
emmeans(best_visit, pairwise ~ Treatment * Week, adjust = "tukey")

### 3.5 Diagnostics ----
plot(best_visit)
qqnorm(residuals(best_visit))
qqPlot(residuals(best_visit))

# 4. VISIT DURATION ANALYSIS ----

### 4.1 Descriptive Statistics ----
duration_stats <- water |> 
  group_by(Treatment, Week) |> 
  group_by(Treatment) |> 
  get_summary_stats(Duration, type = "full") |> 
  mutate(CV = (sd/mean)*100)

view(duration_stats)

### 4.2 Model Selection ----
# Base model (log-transformed)
m1_dur <- lmer(
  log(Duration) ~ Treatment + (1|Week),
  data = water
)

# Add Sex effect
m2_dur <- lmer(
  log(Duration) ~ Treatment * Week + (1|Sex),
  data = water
)

# Add Period interaction
m3_dur <- lmer(
  log(Duration) ~ Treatment * Week + (1 | Group),
  data = water
)

# Compare models
dur_models <- list(
  "+ Week" = m1_dur,
  "+ Sex" = m2_dur,
  "+ Group" = m3_dur
)

aic_dur <- map_dfr(seq_along(dur_models), ~ {
  tibble(
    Model = names(dur_models)[.x],
    Formula = deparse(formula(dur_models[[.x]])),
    AIC = AIC(dur_models[[.x]]),
    BIC = BIC(dur_models[[.x]]),
    logLik = as.numeric(logLik(dur_models[[.x]])))
}) |> arrange(AIC)
  
  print(aic_dur)
  
### 4.3 Best Model Analysis ----
best_dur <- dur_models[[which.min(aic_dur$AIC)]]
summary(best_dur)
anova(best_dur)
  
### 4.4 Post-hoc Analysis ----
emmeans(best_dur, pairwise ~ Treatment * Week, adjust = "tukey")
  
### 4.5 Diagnostics ----
plot(best_dur)
qqnorm(residuals(best_dur))
qqPlot(residuals(best_dur))

# 5. WATER INTAKE ANALYSIS ----
## 5.1 Data Preparation ----
### Identify and Remove outliers
boxplot(water_intake$Intake_calf)
water_intake <- subset(water_intake, !(Intake_calf > 5)) 

## 5.2 Descriptive Statistics ----
### Overall statistics
intake_stats <- water_intake %>%
    group_by(Treatment) %>%
    get_summary_stats(Intake_calf, type = "full") %>%
    mutate(CV = (sd/mean)*100)
  
view(intake_stats)
  
### By time period
intake_time_stats <- water_intake %>%
    group_by(Treatment, Time_day) %>%
    get_summary_stats(Intake_calf, type = "full")
  
view(intake_time_stats)
  
## 5.3 Normality and Variance Tests ----
### Normality tests
shapiro.test(water_intake$Intake_calf)
by(water_intake$Intake_calf, water_intake$Treatment, shapiro.test)
  
### Variance homogeneity
fligner.test(Intake_calf ~ Treatment, data = water_intake)
fligner.test(Intake_calf ~ Week, data = water_intake)
  
## 5.4 Model Selection ----
### Prepare data (Gamma family requires positive values)
water_intake <- water_intake %>%
    mutate(Intake_adj = Intake_calf + 0.01)
  
### Define contrasts
contrasts(water_intake$Treatment) <- contr.treatment(levels(water_intake$Treatment), base=2)
contrasts(water_intake$Time_day) <- contr.treatment(levels(water_intake$Time_day), base=2)
  
### Candidate models
m1_intake <- glmer(Intake_adj ~ Treatment + (1 | Group), 
                   data = water_intake, 
                   family = Gamma(log))
  
m2_intake <- glmer(Intake_adj ~ Time_day + (1 | Group), 
                   data = water_intake,
                   family = Gamma(log))
  
m3_intake <- glmer(Intake_adj ~ Treatment * Time_day + (1 | Group), 
                   data = water_intake,
                   family = Gamma(log))
  
m4_intake <- glmer(Intake_adj ~ Treatment * Time_day + Week + (1 | Group), 
                   data = water_intake,
                   family = Gamma(log))
  
### Model comparison
intake_models <- list(
    "Treatment" = m1_intake,
    "Time_day" = m2_intake,
    "Treatment*Time_day" = m3_intake,
    "Full" = m4_intake
)
  
aic_intake <- map_dfr(seq_along(intake_models), ~ {
    tibble(
      Model = names(intake_models)[.x],
      Formula = deparse(formula(intake_models[[.x]])),
      AIC = AIC(intake_models[[.x]]),
      BIC = BIC(intake_models[[.x]]),
      logLik = as.numeric(logLik(intake_models[[.x]]))
    )
  }) %>% arrange(AIC)
  
print(aic_intake)
  
  ## 5.5 Best Model Analysis ----
best_intake_name <- aic_intake$Model[which.min(aic_intake$AIC)]
best_intake <- intake_models[[best_intake_name]]
summary(best_intake)
anova(best_intake)

## 5.6 Post-hoc Analysis ----
### Treatment effects
emm <- emmeans(best_intake, ~ Treatment * Time_day)
pairs(emm, simple = "each")  

## 5.7 Diagnostics ----
plot(best_intake)
qqnorm(residuals(best_intake))
qqPlot(residuals(best_intake))

# 6. VISUALIZATIONS ----
## 6.1 Visit Frequency -----

ggplot(water |> group_by(Group, Week, Treatment) |> tally(name = "Visit_Count"),
       aes(x = Treatment, y = Visit_Count, fill = Treatment)) +
  geom_boxplot() +
  labs(title = "Water Visits per Group by Treatment",
       y = "Visit Count") +
  scale_fill_manual(values = (treatment_palette)) +
  theme_minimal() +
  theme(legend.position = "none")


ggplot(water |> group_by(Group, Week, Treatment) |> tally(name = "Visit_Count"), aes(x = Week, y = Visit_Count, fill = Treatment)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Weekly Water Visits by Treatment",
       y = "Total Visits") +
  scale_fill_manual(values = (treatment_palette)) +
  theme_minimal()

## 6.2 Visit Duration -----
ggplot(water, aes(x = Treatment, y = Duration, fill = Treatment)) +
  geom_boxplot() +
  labs(title = "Visit Duration by Treatment",
       y = "Duration (seconds)") +
  scale_fill_manual(values = (treatment_palette)) +
  theme_minimal() +
  theme(legend.position = "none")

water %>% 
  mutate(Hour = factor(Hour)) %>% 
  group_by(Hour, Treatment) %>% 
  summarise(Avg_Duration = mean(Duration, na.rm = TRUE)) %>% 
  ggplot(aes(x = Hour, y = Treatment, fill = Avg_Duration)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Avg Duration (s)") +
  labs(title = "Visit Duration by Hour and Treatment") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## 6.3 Water Intake -----

ggplot(water_intake, aes(x = Treatment, y = Intake_calf, fill = Time_day)) +
  geom_boxplot() +
  ggsignif::geom_signif(
    annotation = "***",
    y_position = max(water_intake$Intake_calf) * 1.05,
    xmin = 0.8,  # Nipple-WT left position
    xmax = 1.2,  # Nipple-WT right position
    tip_length = 0.01
  ) +
  ggsignif::geom_signif(
    annotation = "ns",
    y_position = max(water_intake$Intake_calf) * 1.05,
    xmin = 1.8,  # Open-WT left position
    xmax = 2.2,  # Open-WT right position
    tip_length = 0.01
  ) +
  # Between-treatment comparison (higher position)
  ggsignif::geom_signif(
    annotation = "ns",
    y_position = max(water_intake$Intake_calf) * 1.15,
    xmin = 1,    # Middle of Nipple-WT
    xmax = 2,    # Middle of Open-WT
    tip_length = 0.01
  ) +
  labs(
    title = "Water Intake by Treatment and Time of Day",
    y = "Intake per Calf (L)",
    fill = "Time of Day",
    caption = "***p<0.001 | Boxes show IQR/median"
  ) +
  scale_fill_manual(
    values = c("Day-time" = "#1B9E77", "Night-time" = "#D95F02")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "top"
  )

## 6.4 Water Intake vs THI -----
ggplot(water_intake, aes(x = Intake_calf, y = THI, color = Treatment)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Water Intake vs Temperature-Humidity Index (THI)",
       y = "Intake per Calf (L)") +
  scale_color_manual(values = (treatment_palette)) +
  theme_minimal()

