# 1. SETUP AND DATA PREP ----
source("~/nipple-troughs-calf/scripts/00_config.R")

behavior <- cleaned_data$behavior |> 
  mutate(across(c(Active, Lying, Ruminating, Lying_Ruminating, Grazing, Idle), 
           ~ ./Total * 100, .names = "Percent_{.col}")) |>
  drop_na()

view(behavior)
names(behavior)
str(behavior)

# 2. EXPLORATORY ANALYSIS ----
## 2.1 Counts Balance ----
treatment_counts <- behavior %>% 
  distinct(id, Treatment) %>% 
  count(Treatment)

treatment_counts

week_counts <- table(behavior$id, behavior$Week)
week_counts

## 2.2 Behavior Distributions ----
behavior_names <- c("Active", "Lying", "Ruminating", "Lying_Ruminating", "Grazing", "Idle")

### Descriptive Statistics ----
behavior_stats <- behavior %>%
  group_by(Treatment) %>%
  get_summary_stats(all_of(behavior_names), type = "full") %>%
  mutate(CV = (sd/mean)*100)

view(behavior_stats)

### Percentage Statistics ----
percent_stats <- behavior %>%
  group_by(Treatment) %>%
  summarise(
    across(starts_with("Percent"), 
           list(
             mean = ~ mean(.x, na.rm = TRUE),
             sd = ~ sd(.x, na.rm = TRUE),
             se = ~ sd(.x, na.rm = TRUE)/sqrt(sum(!is.na(.x)))
           ), 
           .names = "{.col}_{.fn}")
  )

view(percent_stats)

## 2.3 Normality Checks ----
### Raw Counts ----
normality_tests <- lapply(behavior[behavior_names], shapiro.test)
print(normality_tests)

### Percentages ----
percent_normality <- lapply(behavior[grep("Percent", names(behavior))], shapiro.test)
print(percent_normality)

## 2.4 Variance Homogeneity ----
levene_results <- lapply(behavior_names, 
                         function(b) leveneTest(as.formula(paste(b, "~ Treatment")), 
                                                data = behavior))
print(levene_results)

## 2.5 Visualizations ----
### Time Series Plots ----
behavior_long <- behavior %>%
  pivot_longer(cols = all_of(behavior_names), 
               names_to = "Behavior", 
               values_to = "Count")

ggplot(behavior_long, aes(x = Week, y = Count, color = Treatment)) +
  geom_smooth(method = "loess") +
  facet_wrap(~Behavior, scales = "free_y") +
  labs(title = "Behavior Patterns Over Time by Treatment")

### Percentage Composition ----
percent_long <- behavior %>%
  pivot_longer(cols = starts_with("Percent"), 
               names_to = "Behavior", 
               values_to = "Percentage")

ggplot(percent_long, aes(x = Treatment, y = Percentage, fill = Treatment)) +
  geom_boxplot() +
  facet_wrap(~Behavior) +
  labs(title = "Percentage Time Spent on Each Behavior")

# 3. BEHAVIOR ANALYSIS ----
## 3.1 Model Selection Function ----
fit_behavior_models <- function(response_var, data) {
  
  contrasts(data$Treatment) <- contr.treatment(levels(data$Treatment), base = 2)
  
  base_form <- paste(response_var, "~ Treatment * Week")
  
  m1 <- glmmTMB(as.formula(paste(base_form, "+ (1 | Group)")), 
                data = data, family = poisson(link = "log"))
  m2 <- glmmTMB(as.formula(paste(base_form, "+ (1 | Group) + (1 | Hour)")), 
                data = data, family = poisson(link = "log"))
  m3 <- glmmTMB(as.formula(paste(base_form, "+ (1 | Group) + (1 | Hour) + (1 | Week)")), 
                data = data, family = poisson(link = "log"))
  m4 <- glmmTMB(as.formula(paste(base_form, "+ (1 | Group/Hour)")), 
                data = data, family = poisson(link = "log"))
  m5 <- glmmTMB(as.formula(paste(base_form, "+ (1 | Group) + (1 | Week)")), 
                data = data, family = poisson(link = "log"))
  
  models <- list(
    "+ (1|Group)" = m1,
    "+ (1|Group) + (1|Hour)" = m2,
    "+ (1|Group) + (1|Hour) + (1|Week)" = m3,
    "+ (1|Group/Hour)" = m4,
    "+ (1|Group) + (1|Week)" = m5
  )
  
  aic_table <- purrr::map_dfr(seq_along(models), ~ {
    tibble::tibble(
      Behavior = response_var,
      Model = names(models)[.x],
      Formula = deparse(formula(models[[.x]])),
      AIC = AIC(models[[.x]]),
      BIC = BIC(models[[.x]]),
      logLik = as.numeric(logLik(models[[.x]])))
  }) %>%
    dplyr::arrange(AIC)
  
  best_model_name <- aic_table$Model[which.min(aic_table$AIC)]
  best_model <- models[[best_model_name]]
  
  return(invisible(list(
    aic_table = aic_table,
    best_model = best_model
    )))
}

## 3.2 Posthoc Function ----
run_posthoc <- function(model, response_name) {
  # Pairwise treatment comparisons at mean Week
  cat("\n=== Pairwise Treatment Comparisons (", response_name, ") ===\n")
  print(pairs(emmeans(model, ~ Treatment)))
  
  # Treatment effects over time
  cat("\n=== Treatment × Week Interaction (", response_name, ") ===\n")
  print(emtrends(model, ~ Treatment, var = "Week"))
  
  # Simple slopes analysis
  cat("\n=== Simple Slopes Analysis (", response_name, ") ===\n")
  print(emmeans(model, ~ Treatment * Week, 
                at = list(Week = c(min(behavior$Week), 
                                   mean(behavior$Week), 
                                   max(behavior$Week)))))
}

## 3.3 Check Models Function ----
check_models <- function(model, response_name) {
  cat("\n=== Diagnostics for", response_name, "model ===\n")
  
  # Residual checks
  sim_res <- simulateResiduals(model)
  plot(sim_res)
  
  # Overdiswpersion test
  cat("\nOverdispersion test:\n")
  print(testDispersion(sim_res))
}

## 3.4 Effect size analysis Function ----
calculate_effects <- function(model, response_name) {
  cat("\n=== Effect Sizes for", response_name, "===\n")
  
  # Fixed effects sizes
  fixed_effects <- standardize_parameters(model)
  print(fixed_effects)
  
  # Variance explained (R²)
  cat("\nVariance explained:\n")
  print(r2_nakagawa(model))
}

## 3.5 Running for each behavior ----
#Active
results_active <- fit_behavior_models("Active", behavior)
summary(results_active$best_model)
run_posthoc(results_active$best_model, "Active")
check_models(results_active$best_model, "Active")
calculate_effects(results_active$best_model, "Active")
plot(ggpredict(results_active$best_model, terms = c("Week", "Treatment")))

#Lying
results_lying <- fit_behavior_models("Lying", behavior)
summary(results_lying$best_model)
run_posthoc(results_lying$best_model, "Lying")
check_models(results_lying$best_model, "Lying")
calculate_effects(results_lying$best_model, "Lying")
plot(ggpredict(results_lying$best_model, terms = c("Week", "Treatment")))

#Ruminating
results_ruminating <- fit_behavior_models("Ruminating", behavior)
summary(results_ruminating$best_model)
run_posthoc(results_ruminating$best_model, "Ruminating")
check_models(results_ruminating$best_model, "Ruminating")
calculate_effects(results_ruminating$best_model, "Ruminating")
plot(ggpredict(results_ruminating$best_model, terms = c("Week", "Treatment")))

#Lying_Ruminating
results_lr <- fit_behavior_models("Lying_Ruminating", behavior)
summary(results_lr$best_model)
run_posthoc(results_lr$best_model, "Lying_Ruminating")
check_models(results_lr$best_model, "Lying_Ruminating")
calculate_effects(results_lr$best_model, "Lying_Ruminating")
plot(ggpredict(results_lr$best_model, terms = c("Week", "Treatment")))

#Grazing
results_grazing <- fit_behavior_models("Grazing", behavior)
summary(results_grazing$best_model)
run_posthoc(results_grazing$best_model, "Grazing")
check_models(results_grazing$best_model, "Grazing")
calculate_effects(results_grazing$best_model, "Grazing")
plot(ggpredict(results_grazing$best_model, terms = c("Week", "Treatment")))

#Idle
results_idle <- fit_behavior_models("Idle", behavior)
summary(results_idle$best_model)
run_posthoc(results_idle$best_model, "Idle")
check_models(results_idle$best_model, "Idle")
calculate_effects(results_idle$best_model, "Idle")
plot(ggpredict(results_idle$best_model, terms = c("Week", "Treatment")))

# 4. Visualization -----
#Observed plot
behavior_long <- behavior %>%
  pivot_longer(cols = all_of(behavior_names), 
               names_to = "Behavior", 
               values_to = "Count") %>%
  mutate(Behavior = factor(Behavior, 
                           levels = c("Active", "Grazing", "Ruminating", 
                                      "Lying", "Lying_Ruminating", "Idle"),
                           labels = c("Active", "Grazing", "Ruminating", 
                                      "Lying", "Lying + Ruminating", "Idle")))

ggplot(behavior_long, aes(x = Week, y = Count, color = Treatment, fill = Treatment)) +
  geom_smooth(method = "loess", alpha = 0.2, linewidth = 1) +
  geom_point(alpha = 0.1, position = position_jitter(width = 0.2), shape = 16) +
  facet_wrap(~Behavior, scales = "free_y", ncol = 3) +
  scale_color_manual(values = c("#E69F00", "#56B4E9")) +  # Colorblind-friendly
  scale_fill_manual(values = c("#E69F00", "#56B4E9")) +
  labs(x = "Week", 
       y = "Behavior Count") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey90", color = NA),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "grey40")
  ) +
  scale_x_continuous(breaks = seq(min(behavior$Week), max(behavior$Week), by = 1))

# Predictions plot
all_preds <- map_dfr(
  list(results_active, results_grazing, results_ruminating, 
       results_lying, results_lr, results_idle),
  ~ as_tibble(ggpredict(.x$best_model, terms = c("Week", "Treatment"))) %>%
    mutate(Behavior = .x$aic_table$Behavior[1])
) %>%
  mutate(Behavior = factor(Behavior, 
                           levels = c("Active", "Grazing", "Ruminating", 
                                      "Lying", "Lying_Ruminating", "Idle"),
                           labels = c("Active", "Grazing", "Ruminating", 
                                      "Lying", "Lying + Ruminating", "Idle")))

ggplot(all_preds, aes(x = x, y = predicted, color = group)) +
  geom_line(linewidth = 1) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), 
              alpha = 0.2) +
  facet_wrap(~Behavior, scales = "free_y", ncol = 3) +
  scale_color_manual(values = c("#E69F00", "#56B4E9")) +
  scale_fill_manual(values = c("#E69F00", "#56B4E9")) +
  labs(x = "Week", 
       y = "Behavior Count") +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "grey90", color = NA),
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "grey40")
  ) +
  scale_x_continuous(breaks = seq(min(behavior$Week), max(behavior$Week), by = 1))

# 5. Descriptive Analysis - Start timing of Grazing and Ruminating Behaviors----
# Set threshold for behavior detection
Behavior_threshold <- 5

## 5.1 Grazing Analysis ----
# First week each calf meets grazing threshold
grazing_start <- behavior %>%
  group_by(id, Week, Treatment) %>%
  summarise(Grazing_Scans = sum(Grazing)) %>% 
  filter(Grazing_Scans >= Behavior_threshold) %>% 
  slice_min(Week, with_ties = FALSE) %>% 
  ungroup()

View(grazing_start)

grazing_start_min_week <- grazing_start %>%
  group_by(id, Treatment) %>%
  summarise(
    First_Week = min(Week, na.rm = TRUE),  
    .groups = 'drop' 
  )

View(grazing_start_min_week)

# Visualization
ggplot(grazing_start_min_week, 
       aes(x = reorder(as.factor(id), First_Week), 
           y = First_Week,
           fill = Treatment)) +  # Still map Treatment to fill
  geom_bar(stat = "identity") +
  facet_wrap(~ Treatment, scales = "free_y", ncol = 1) +
  scale_fill_manual(values = treatment_palette) +  # Apply your palette
  labs(title = "Start of Grazing Behavior by Calf and Treatment",
       x = "Calf ID",
       y = "First Week Observed") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold", size = 10),
    strip.background = element_rect(fill = "lightgray"),
    panel.spacing = unit(1, "lines")
  ) +
  guides(fill = "none") +
  coord_flip()

# Calculate average start by treatment
average_grazing_start <- grazing_start_min_week %>%
  group_by(Treatment) %>%
  summarise(Average_First_Week = mean(First_Week, na.rm = TRUE), .groups = "drop")

view(average_grazing_start)

## 5.2 Ruminating Analysis ----
# First week each calf meets ruminating threshold

behavior$Ruminating_total <- behavior$Ruminating + behavior$Lying_Ruminating

ruminating_start <- behavior %>%
  group_by(id, Week, Treatment) %>%
  summarise(ruminating_scans = sum(Ruminating_total)) %>% 
  filter(ruminating_scans >= Behavior_threshold) %>% 
  slice_min(Week, with_ties = FALSE) %>% 
  ungroup()

view(ruminating_start)

ruminating_start_min_week <- ruminating_start %>%
  group_by(id, Treatment) %>%
  summarise(
    First_Week = min(Week, na.rm = TRUE),  
    .groups = 'drop' 
  )

View(ruminating_start_min_week)

# Visualization
ggplot(ruminating_start_min_week, 
       aes(x = reorder(as.factor(id), First_Week), 
           y = First_Week,
           fill = Treatment)) +  # Still map Treatment to fill
  geom_bar(stat = "identity") +
  facet_wrap(~ Treatment, scales = "free_y", ncol = 1) +
  scale_fill_manual(values = treatment_palette) +  # Apply your palette
  labs(title = "Start of Ruminating Behavior by Calf and Treatment",
       x = "Calf ID",
       y = "First Week Observed") +
  theme_minimal() +
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold", size = 10),
    strip.background = element_rect(fill = "lightgray"),
    panel.spacing = unit(1, "lines")
  ) +
  guides(fill = "none") +
  coord_flip()

# Calculate average start by treatment
average_ruminating_start <- ruminating_start_min_week %>%
  group_by(Treatment) %>%
  summarise(Average_First_Week = mean(First_Week, na.rm = TRUE), .groups = "drop")

view(average_ruminating_start)
