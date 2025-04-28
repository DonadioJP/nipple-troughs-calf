# 1. SETUP ----
source("scripts/00_config.R")
library(lme4)       # Mixed effects models
library(emmeans)    # Post-hoc comparisons
library(broom.mixed)# Model tidying

# 2. DATA PREP ----
growth <- cleaned_data$growth |> 
  mutate(
    # Ensure proper typing
    across(c(id, Group, Treatment, Sex, Breed), factor),
    Week = as.numeric(Week),
    # Create composite variables
    Weight_gain = Weight - Weight_birth
  ) |> 
  filter(!is.na(Weight))  # Remove missing weights

# 3. ANALYSIS ----
## 3.1 Primary Growth Model ----
growth_model <- lmer(
  Weight ~ Treatment * Week + (1 | id) + (1 | Group),
  data = growth,
  REML = TRUE
)

## 3.2 ADG Analysis ----
adg_model <- lmer(
  ADG ~ Treatment + (1 | id) + (1 | Week),
  data = growth,
  REML = TRUE
)

## 3.3 Body Measurements ----
body_model <- glmer(
  CC ~ Treatment * Week + (1 | id),
  data = growth,
  family = Gamma(link = "log")
)

# 4. DIAGNOSTICS ----
performance::check_model(growth_model)  # Model assumptions
vif_results <- car::vif(growth_model)   # Multicollinearity

# 5. VISUALIZATION ----
## 5.1 Weight Trajectories ----
weight_plot <- ggplot(growth, aes(Week, Weight, color = Treatment)) +
  geom_line(aes(group = id), alpha = 0.3) +
  stat_summary(fun = mean, geom = "line", size = 1.5) +
  scale_color_manual(values = treatment_palette) +
  labs(title = "Weight Development by Treatment",
       x = "Week", y = "Body Weight (kg)") +
  theme_custom()

## 5.2 Body Measurements Grid ----
body_plots <- growth |> 
  pivot_longer(cols = c(CC, WH, CH, CW, CL, BL), 
               names_to = "measure") |> 
  ggplot(aes(Week, value, color = Treatment)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  facet_wrap(~ measure, scales = "free_y") +
  scale_color_manual(values = treatment_palette) +
  labs(title = "Body Measurements Over Time") +
  theme_custom()

# 6. SAVE OUTPUTS ----
## Models
saveRDS(growth_model, "outputs/growth/weight_model.rds")
saveRDS(adg_model, "outputs/growth/adg_model.rds")

## Figures
ggsave("figures/growth/weight_trajectories.png", weight_plot, 
       width = 8, height = 5)
ggsave("figures/growth/body_measurements.png", body_plots,
       width = 10, height = 7)

## 7. REPORTING ----
sink("outputs/growth/summary_report.txt")
cat("=== WEIGHT MODEL SUMMARY ===\n")
print(summary(growth_model))
cat("\n\n=== ADG MODEL SUMMARY ===\n")
print(summary(adg_model))
cat("\n\n=== VIF RESULTS ===\n")
print(vif_results)
sink()