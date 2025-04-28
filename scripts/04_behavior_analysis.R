# 1. SETUP ----
source("scripts/00_config.R")
library(glmmTMB)    # Zero-inflated models
library(multcomp)   # Multiple comparisons
library(DHARMa)     # Model diagnostics for GLMMs

# 2. DATA PREP ----
behavior <- cleaned_data$behavior |> 
  mutate(
    # Convert to factors
    across(c(id, Treatment, Hour), ~factor(.)),
    # Calculate percentages
    across(c(Active, Lying, Ruminating, Grazing, Idle), 
           ~ ./Total_Scans * 100, .names = "Percent_{.col}"),
    # Behavioral clusters
    Resting = Percent_Lying + Percent_Ruminating,
    Active = Percent_Active + Percent_Grazing
  ) |> 
  filter(Week != 9) # Remove weaning week

# 3. ANALYSIS ----
## 3.1 Main Behavior Model ----
behavior_model <- glmmTMB(
  Active ~ Treatment * Week + (1 | id) + (1 | Hour),
  data = behavior,
  family = poisson(link = "log")
)

## 3.2 Grazing Initiation ----
grazing_model <- glmer(
  Grazing ~ Treatment * Week + (1 | id),
  data = filter(behavior, Grazing >= 5), # Threshold
  family = binomial(link = "logit")
)

## 3.3 Post-hoc Comparisons ----
emmeans_results <- emmeans(behavior_model, pairwise ~ Treatment, adjust = "tukey")

# 4. DIAGNOSTICS ----
## 4.1 Simulation-based diagnostics
sim_res <- simulateResiduals(behavior_model)
plot(sim_res)

## 4.2 Zero-inflation check
performance::check_zeroinflation(behavior_model)

# 5. VISUALIZATION ----
## 5.1 Time Budget Plot ----
time_budget <- behavior |> 
  pivot_longer(cols = starts_with("Percent_"), 
               names_to = "Behavior", values_to = "Percentage") |> 
  ggplot(aes(Week, Percentage, color = Treatment)) +
  stat_summary(fun = mean, geom = "line", size = 1.2) +
  facet_wrap(~ Behavior, scales = "free_y") +
  scale_color_manual(values = treatment_palette) +
  labs(title = "Behavioral Time Budgets by Treatment",
       y = "Percentage of Time") +
  theme_custom()

## 5.2 Grazing Initiation ----
grazing_plot <- behavior |> 
  group_by(id, Treatment) |> 
  filter(Grazing >= 5) |> 
  summarise(First_Week = min(Week)) |> 
  ggplot(aes(Treatment, First_Week, fill = Treatment)) +
  geom_boxplot() +
  geom_jitter(width = 0.1) +
  scale_fill_manual(values = treatment_palette) +
  labs(title = "Onset of Sustained Grazing (≥5 scans)",
       y = "First Week Observed") +
  theme_custom()

# 6. SAVE OUTPUTS ----
## Models
saveRDS(behavior_model, "outputs/behavior/main_model.rds")
saveRDS(grazing_model, "outputs/behavior/grazing_model.rds")

## Figures
ggsave("figures/behavior/time_budgets.png", time_budget, 
       width = 10, height = 6)
ggsave("figures/behavior/grazing_onset.png", grazing_plot,
       width = 6, height = 5)

## 7. REPORTING ----
sink("outputs/behavior/summary_report.txt")
cat("=== BEHAVIOR MODEL SUMMARY ===\n")
print(summary(behavior_model))
cat("\n\n=== GRAZING ONSET MODEL ===\n")
print(summary(grazing_model))
cat("\n\n=== TREATMENT COMPARISONS ===\n")
print(emmeans_results)
sink()
