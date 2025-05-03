# 1. SETUP ----
source("~/nipple-troughs-calf/scripts/00_config.R")

# 2. DATA PREP ----
view(cleaned_data$growth)
names(cleaned_data$growth)
str(cleaned_data$growth)
growth <- cleaned_data$growth

# 2. EXPLORATORY ANALYSIS ----
## 2.1 Counts Balance ----
treatment_counts <- growth %>% 
  distinct(id, Treatment) %>% 
  count(Treatment)

treatment_counts

sex_counts <- growth %>%
  distinct(id, Sex) %>%
  count(Sex)

sex_counts

week_counts <- table(growth$id, growth$Week)

week_counts

# 3. WEIGHT ANALYSIS ----

### 3.1 Birth Weight (Week 0) ----
birth_weight <- growth %>% 
  filter(Week == 0)

#### Descriptive Statistics
birth_stats <- birth_weight %>%
  group_by(Treatment) %>%
  get_summary_stats(Weight, type = "full") %>%
  mutate(CV = (sd/mean)*100)

view(birth_stats)

#### Visualization
ggboxplot(birth_weight, x = "Treatment", y = "Weight", 
          add = "jitter", ylab = "Weight (kg)", xlab = "")

#### Statistical Analysis
# Treatment effect
m_birth <- lm(Weight ~ Treatment, data = birth_weight)
summary(m_birth) # no effect

# Sex effect
m_birth_sex <- lm(Weight ~ Sex, data = birth_weight)
summary(m_birth_sex) # no effect

### 3.2 Growth Weight (All Weeks) ----

#### 3.2.1 Descriptive Statistics ----
weight_stats <- growth %>%
  group_by(Treatment, Week) %>%
  get_summary_stats(Weight, type = "full") %>%
  mutate(CV = (sd/mean)*100)

view(weight_stats)

#### 3.2.2 Visualization ----
ggplot(growth, aes(x = Week, y = Weight, color = Treatment)) +
  geom_smooth(method = "loess", se = TRUE) +
  labs(title = "Weight Growth Curve by Treatment")

#### 3.2.3 Normality Tests ----
shapiro.test(growth$Weight) #no-normal
by(growth$Weight, growth$Treatment, shapiro.test) #no-normal

#### 3.2.4 Variance Homogeneity ----
leveneTest(Weight ~ Treatment, data = growth) #homogeneity
leveneTest(Weight ~ as.factor(Week), data = growth) #heterogeneity

#### 3.2.5 Model Selection ----
# Fit models
m1 <- lmer(Weight ~ Treatment * Week + (1 | id), 
           data = growth)
m2 <- lmer(Weight ~ Treatment * Week + (1 | id) + (1 | Sex), 
           data = growth)
m3 <- lmer(Weight ~ Treatment * Week + (1 | id) + (1 | Sex) + (1 | Health), 
           data = growth)
m4 <- lmer(Weight ~ Treatment * Week + (1 | id) + (1 | Week), 
           data = growth)
m5 <- lmer(Weight ~ Treatment * Week + (1 | id) + (1 | Week) + (1 | Sex), 
           data = growth)

# Candidate models
weight_models <- list(
  "+ (1|id)" = m1,
  "+ (1|Sex)" = m2,
  "+ (1|Sex) + (1|Health)" = m3,
  "+ (1|Week)" = m4,
  "+ (1|Week) + (1|Sex)" = m5
)
  
# AIC comparison
# Create AIC table - most robust method
aic_table <- map_dfr(seq_along(weight_models), ~ {
  tibble(
    Model = names(weight_models)[.x],
    Formula = deparse(formula(weight_models[[.x]])),
    AIC = AIC(weight_models[[.x]]),
    BIC = BIC(weight_models[[.x]]),
    logLik = as.numeric(logLik(weight_models[[.x]]))
  )
}) %>%
  arrange(AIC)

# Print results
print(aic_table)

#### 3.2.6 Best Model Analysis ----
best_weight <- lmer(Weight ~ Treatment * Week + (1|Week), 
                      data = growth)
  
summary(best_weight)
anova(best_weight)
  
#### 3.2.7 Post-hoc ----
emmeans(best_weight, pairwise ~ Treatment | Week, adjust = "tukey")
  
#### 3.2.8 Diagnostics ----
plot(best_weight)
qqnorm(residuals(best_weight))
qqPlot(residuals(best_weight))

### 3.3 Weaning Weight (Week 8) ----
weaning_weight <- growth %>% 
  filter(Week == 8)

#### Descriptive Statistics
weaning_stats <- weaning_weight %>%
  group_by(Treatment) %>%
  get_summary_stats(Weight, type = "full") %>%
  mutate(CV = (sd/mean)*100)

view(weaning_stats)

#### Visualization
ggboxplot(weaning_weight, x = "Treatment", y = "Weight", 
          add = "jitter", ylab = "Weight (kg)", xlab = "")

#### Statistical Analysis
# Treatment effect
m_weaning <- lm(Weight ~ Treatment, data = weaning_weight)
summary(m_weaning) # no effect

# Sex effect
m_weaning_sex <- lm(Weight ~ Sex, data = weaning_weight)
summary(m_weaning_sex) # no effect

# 4. AVERAGE DAILY GAIN (ADG) ----

### 4.1 Descriptive Statistics ----
adg_stats <- growth %>%
  group_by(Treatment) %>%
  get_summary_stats(ADG, type = "full") %>%
  mutate(CV = (sd/mean)*100)

view(adg_stats)

### 4.2 Visualization ----
ggboxplot(growth, x = "Treatment", y = "ADG", 
          add = "jitter", ylab = "ADG (kg/day)")

### 4.3 Model Selection ----
# Fit models
m1_adg <- lmer(ADG ~ Treatment * Week + (1 | id), 
               data = growth)
m2_adg <- lmer(ADG ~ Treatment * Week + (1 | id) + (1 | Sex), 
               data = growth)
m3_adg <- lmer(ADG ~ Treatment * Week + (1 | id) + (1 | Week), 
               data = growth)
m4_adg <- lmer(ADG ~ Treatment + (1 | id) + (1 | Week), 
               data = growth)

# Candidate models
adg_models <- list(
  "Treatment*Week + (1|id)" = m1_adg,
  "+ (1|Sex)" = m2_adg,
  "+ (1|Week)" = m3_adg,
  "Simplified" = m4_adg
)

# AIC comparison
aic_adg <- map_dfr(seq_along(adg_models), ~ {
  tibble(
    Model = names(adg_models)[.x],
    Formula = deparse(formula(adg_models[[.x]])),
    AIC = AIC(adg_models[[.x]]),
    BIC = BIC(adg_models[[.x]]),
    logLik = as.numeric(logLik(adg_models[[.x]]))
  )
}) %>%
  arrange(AIC)

# Print results
print(aic_adg)

### 4.4 Best Model Analysis ----
# Select model with lowest AIC
best_adg <- adg_models[[which.min(aic_adg$AIC)]]

summary(best_adg)
anova(best_adg)

### 4.5 Post-hoc Analysis ----
emmeans(best_adg, pairwise ~ Treatment, adjust = "tukey")

### 4.6 Diagnostics ----
# Basic plots
plot(best_adg)
qqnorm(residuals(best_adg))

# 5. BODY MEASUREMENTS ANALYSIS----
body_vars <- c("CC", "WH", "CH", "CW", "CL", "BL")

## 5.1 Descriptive Statistics ----
get_descriptives <- function(var, data) {
  data %>%
    group_by(Treatment, Week) %>%
    get_summary_stats(!!sym(var), type = "full") %>%
    mutate(CV = (sd/mean)*100)
}

# Get all descriptives in one table
body_summary <- map_dfr(body_vars, ~get_descriptives(.x, growth), .id = "Variable")

View(body_summary)

## 5.2. Model selection ----
fit_growth_models <- function(response_var, data) {
  
  contrasts(data$Treatment) <- contr.treatment(levels(data$Treatment), base = 2)

  base_form <- paste(response_var, "~ Treatment * Week")
  
  m1 <- lmer(as.formula(paste(base_form, "+ (1|id)")), data = data)
  m2 <- lmer(as.formula(paste(base_form, "+ (1|id) + (1|Sex)")), data = data)
  m3 <- lmer(as.formula(paste(base_form, "+ (1|id) + (1|Week)")), data = data)
  m4 <- lmer(as.formula(paste(base_form, "+ (1|id) + (1|Week) + (1|Sex)")), data = data)
  
  models <- list(
    "+(1|id)" = m1,
    "+(1|id)+(1|Sex)" = m2,
    "+(1|id)+(1|Week)" = m3,
    "+(1|id)+(1|Week)+(1|Sex)" = m4
  )
  
  aic_table <- map_dfr(seq_along(models), ~ {
    tibble(
      Variable = response_var,
      Model = names(models)[.x],
      Formula = deparse(formula(models[[.x]])),
      AIC = AIC(models[[.x]]),
      BIC = BIC(models[[.x]]),
      logLik = as.numeric(logLik(models[[.x]]))
    )
  }) %>% arrange(AIC)
  
  best_model_name <- aic_table$Model[which.min(aic_table$AIC)]
  best_model <- models[[best_model_name]]
  
  return(list(
    aic_table = aic_table,
    best_model = best_model
  ))
}

## 5.3 Posthoc Analysis ----
run_growth_posthoc <- function(model, var_name) {
  cat("\n=== Pairwise Treatment Comparisons (", var_name, ") ===\n")
  print(pairs(emmeans(model, ~ Treatment)))
  
  cat("\n=== Treatment × Week Interaction (", var_name, ") ===\n")
  print(emtrends(model, ~ Treatment, var = "Week"))
  
  cat("\n=== Simple Slopes Analysis (", var_name, ") ===\n")
  print(emmeans(model, ~ Treatment * Week, 
                at = list(Week = c(min(growth$Week), 
                                   mean(growth$Week), 
                                   max(growth$Week)))))
}

## 5.4 Model Diagnostics ----
check_growth_models <- function(model, var_name) {
  cat("\n=== Diagnostics for", var_name, "model ===\n")
  
  par(mfrow = c(1,2))
  qqnorm(residuals(model))
  qqline(residuals(model))
  plot(fitted(model), residuals(model))
  abline(h = 0, lty = 2)
  par(mfrow = c(1,1))
}

## 5.5 Effect Size Analysis ----
calculate_growth_effects <- function(model, var_name) {
  cat("\n=== Effect Sizes for", var_name, "===\n")
  
  # Fixed effects
  print(tidy(model, conf.int = TRUE))
  
  # Variance components
  print(VarCorr(model))
}

## 5.6 Run Analysis for All Body Variables ----
body_vars <- c("CC", "WH", "CH", "CW", "CL", "BL")

# Initialize results list
growth_results <- list()

for (var in body_vars) {
  cat("\n\n===== Analyzing", var, "=====\n")
  
  # Fit models
  results <- fit_growth_models(var, growth)
  growth_results[[var]] <- results
  
  # Show best model
  cat("\n--- Best Model Summary ---\n")
  print(summary(results$best_model))
  
  # Posthoc tests
  run_growth_posthoc(results$best_model, var)
  
  # Diagnostics
  check_growth_models(results$best_model, var)
  
  # Effect sizes
  calculate_growth_effects(results$best_model, var)
  
  # Plot predictions
  pred_plot <- plot(ggpredict(results$best_model, terms = c("Week", "Treatment"))) +
    labs(title = paste("Growth in", var), y = var)
  print(pred_plot)
}

# 5. VISUALIZATION ----
## 5.1 Weight Trajectories individuals----
weight_plot <- ggplot(growth, aes(Week, Weight, color = Treatment)) +
  geom_line(aes(group = id), alpha = 0.3) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.5) +
  scale_color_manual(values = treatment_palette) +
  labs(title = "Weight Development by Treatment",
       x = "Week", y = "Body Weight (kg)") +
  theme_custom()

weight_plot

weight_group_plot <- ggplot(growth, aes(Week, Weight, color = Group, linetype = Treatment)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.2) +  
  stat_summary(fun.data = mean_se, geom = "ribbon", 
               aes(fill = Group), alpha = 0.1, color = NA) +  
  labs(title = "Weight Development by Group",
       x = "Week", y = "Body Weight (kg)") +
  theme_custom() +
  facet_wrap(~ Treatment) 

weight_group_plot

## 5.2 Average daily gains Grid individuals ----

adg_plot <- ggplot(growth, aes(Week, ADG, color = Treatment)) +
  geom_line(aes(group = id), alpha = 0.3) +  
  stat_summary(fun = mean, geom = "line", linewidth = 1.5) +  
  scale_color_manual(values = treatment_palette) +
  labs(title = "ADG Development by Treatment",
       x = "Week", 
       y = "Average Daily Gain (kg/day)") +
  theme_custom()

adg_plot

adg_group_plot <- ggplot(growth, aes(Week, ADG, color = Group, linetype = Treatment)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1.2) +
  stat_summary(fun.data = mean_se, geom = "ribbon", 
               aes(fill = Group), alpha = 0.1, color = NA) +
  labs(title = "ADG Development by Group",
       x = "Week", y = "Average Daily Gain (kg/day)") +
  theme_custom() +
  facet_wrap(~ Treatment) 

adg_group_plot

## 5.3 Body Measurements Grid individuals ----
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

body_plots

body_group_plots <- growth |> 
  pivot_longer(cols = c(CC, WH, CH, CW, CL, BL), names_to = "measure") |> 
  ggplot(aes(Week, value, color = Group, linetype = Treatment)) +
  stat_summary(fun = mean, geom = "line", linewidth = 1) +
  stat_summary(fun.data = mean_se, geom = "ribbon", 
               aes(fill = Group), alpha = 0.1, color = NA) +
  facet_grid(measure ~ Treatment, scales = "free_y") + 
  labs(title = "Body Measurements by Group and Treatment") +
  theme_custom()

body_group_plots

#Manuscript figure 2
g2 <- ggboxplot(Des, x = "Week", y = "CC", add = "mean_sd", color = "Treatment") +
  geom_point(aes(color = Treatment, shape = Treatment), 
             position = position_dodge(width = 0.75), size = 2) +
  scale_y_continuous(limits = c(60, 102), breaks = seq(60, 100, by = 10)) +
  scale_x_discrete(labels = function(x) ifelse(x == "0", "Birth", x)) +  
  labs(x = "", y = "Chest girth", color = "Treatment", shape = "Treatment") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 15),  
    axis.text.y = element_text(size = 15),                        
    axis.title.x = element_text(size = 18),                       
    axis.title.y = element_text(size = 18),                       
    legend.position = "none",  
    panel.grid = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.2, "cm")
  )


g3 <- ggboxplot(Des, x = "Week", y = "WH", add = "mean_sd", color = "Treatment") +
  geom_point(aes(color = Treatment, shape = Treatment), 
             position = position_dodge(width = 0.75), size = 2) +
  scale_y_continuous(limits = c(60, 90), breaks = seq(60, 90, by = 10)) +
  scale_x_discrete(labels = function(x) ifelse(x == "0", "Birth", x)) +  
  labs(x = "", y = "Withers height", color = "Treatment", shape = "Treatment") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 15),  
    axis.text.y = element_text(size = 15),                        
    axis.title.x = element_text(size = 18),                       
    axis.title.y = element_text(size = 18),                       
    legend.position = "none",  
    panel.grid = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.2, "cm")
  )


g4 <- ggboxplot(Des, x = "Week", y = "CH", add = "mean_sd", color = "Treatment") +
  geom_point(aes(color = Treatment, shape = Treatment), 
             position = position_dodge(width = 0.75), size = 2) +
  scale_y_continuous(limits = c(60, 95), breaks = seq(60, 90, by = 10)) +
  scale_x_discrete(labels = function(x) ifelse(x == "0", "Birth", x)) +  
  labs(x = "", y = "Rump height", color = "Treatment", shape = "Treatment") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 15),  
    axis.text.y = element_text(size = 15),                        
    axis.title.x = element_text(size = 18),                       
    axis.title.y = element_text(size = 18),                       
    legend.position = "none", 
    panel.grid = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.2, "cm")
  )


g5 <- ggboxplot(Des, x = "Week", y = "CW", add = "mean_sd", color = "Treatment") +
  geom_point(aes(color = Treatment, shape = Treatment), 
             position = position_dodge(width = 0.75), size = 2) +
  scale_x_discrete(labels = function(x) ifelse(x == "0", "Birth", x)) +  
  labs(x = "", y = "Hump width", color = "Treatment", shape = "Treatment") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 15), 
    axis.text.y = element_text(size = 15),                        
    axis.title.x = element_text(size = 18),                       
    axis.title.y = element_text(size = 18),                       
    legend.position = "none",  
    panel.grid = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.2, "cm")
  )


g6 <- ggboxplot(Des, x = "Week", y = "CL", add = "mean_sd", color = "Treatment") +
  geom_point(aes(color = Treatment, shape = Treatment), 
             position = position_dodge(width = 0.75), size = 2) +
  scale_y_continuous(limits = c(12.5, 25), breaks = seq(12.5, 25, by = 2.5)) +
  scale_x_discrete(labels = function(x) ifelse(x == "0", "Birth", x)) +  
  labs(x = "", y = "Rump lenght", color = "Treatment", shape = "Treatment") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 15),  
    axis.text.y = element_text(size = 15),                        
    axis.title.x = element_text(size = 18),                       
    axis.title.y = element_text(size = 18),                       
    legend.position = "none",
    panel.grid = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.2, "cm")
  )


g1 <-  ggboxplot(Des, x = "Week", y = "BL", color = "Treatment") +
  geom_point(aes(color = Treatment, shape = Treatment), 
             position = position_dodge(width = 0.75), size = 2) +
  scale_y_continuous(limits = c(48, 80), breaks = seq(50, 80, by = 10)) +
  scale_x_discrete(labels = function(x) ifelse(x == "0", "Birth", x)) + 
  labs(x = "", y = "Body lenght", color = "Treatment", shape = "Treatment") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 15),  
    axis.text.y = element_text(size = 15),                        
    axis.title.x = element_text(size = 18),                       
    axis.title.y = element_text(size = 18),                       
    legend.position = "none",  
    panel.grid = element_blank(),
    axis.line.x = element_line(color = "black"),
    axis.line.y = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.2, "cm")
  )

# Arrange all boxplots in a grid
combined_plot <- ggarrange(
  g1, g2, g3, g4, g5, g6,
  ncol = 2, nrow = 3,
  heights = c(1, 1, 1), widths = c(1, 1),
  common.legend = TRUE, legend = "none",
  align = "hv",
  vjust = 1, hjust = 0.5
)

# Annotate the figure with titles and axis labels
annotated_plot <- annotate_figure(
  combined_plot,
  left = text_grob("Body measurements", rot = 90, size = 20),
  bottom = text_grob("Week after birth", size = 20),
  top = text_grob(" ", size = 18, face = "bold")
)

annotated_plot