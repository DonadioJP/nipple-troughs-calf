# 1. SETUP AND DATA PREP ----
source("~/nipple-troughs-calf/scripts/00_config.R")
source("~/nipple-troughs-calf/scripts/01_data_cleaning.R")

cs_data <- cleaned_data$cross_suck
view(cs_data)
names(cs_data)
str(cs_data)

# 2. EXPLORATORY ANALYSIS ----
## 2.1 Data Balance ----
### Treatment counts ----
treatment_counts <- cs_data %>% 
  distinct(Group, Treatment) %>% 
  count(Treatment)

treatment_counts

### Week counts ----
week_counts <- table(cs_data$Group, cs_data$Week)
week_counts

## 2.2 Cross-Sucking Distributions ----
### Event counts ----
cs_counts <- cs_data %>%
  group_by(Treatment, Week, Group) %>%
  summarise(Event_Count = n(), .groups = "drop")

view(cs_counts)

### Duration statistics ----
duration_stats <- cs_data %>%
  group_by(Treatment, Week) %>%
  get_summary_stats(Duration, type = "full") %>%
  mutate(CV = (sd/mean)*100)

view(duration_stats)

### Frequency by body part ----
body_part_counts <- cs_data %>%
  count(Treatment, Body_part) %>%
  group_by(Treatment) %>%
  mutate(percentage = n/sum(n)*100)

view(body_part_counts)

### Sex combinations ----
sex_combinations <- cs_data %>%
  count(Sex_Ex, Sex_Receiver) %>%
  mutate(percentage = n/sum(n)*100)

view(sex_combinations)

## 2.3 Temporal Patterns ----
### Daily distribution ----
time_distribution <- cs_data %>%
  count(Treatment, Hour) %>%
  group_by(Treatment) %>%
  mutate(percentage = n/sum(n)*100)

view(time_distribution)

### Weekly trends ----
weekly_trends <- cs_data %>%
  count(Treatment, Week) %>%
  group_by(Treatment) %>%
  mutate(percentage = n/sum(n)*100)

view(weekly_trends)

## 2.4 Calf Participation Analysis ----
### Executor-receiver relationships ----
executor_receiver_summary <- cs_data %>%
  group_by(Executor, Receiver) %>%
  summarise(Event_Count = n(), .groups = "drop")

view(executor_receiver_summary)

### Participation proportions ----
executor_receiver_percent <- cs_data %>%
  group_by(Treatment, Calf = Executor) %>%
  summarise(Total_Executed = n(), .groups = "drop") %>%
  full_join(
    cs_data %>%
      group_by(Treatment, Calf = Receiver) %>%
      summarise(Total_Received = n(), .groups = "drop"),
    by = c("Treatment", "Calf")
  ) %>%
  replace_na(list(Total_Executed = 0, Total_Received = 0)) %>%
  mutate(
    Total_Participated = Total_Executed + Total_Received,
    Percent_Executed = 100 * Total_Executed / Total_Participated,
    Percent_Received = 100 * Total_Received / Total_Participated
  ) %>%
  arrange(Treatment, Calf)

view(executor_receiver_percent)

## 2.6 Normality Checks ----
### Duration ----
duration_normality <- by(cs_data$Duration, cs_data$Treatment, shapiro.test)
print(duration_normality)

### Event counts ----
count_normality <- by(cs_counts$Event_Count, 
                      cs_counts$Treatment, shapiro.test)
print(count_normality)


# 3. ANALYSIS ----
## 3.1 Frequency Analysis ----
### Model Selection ----
# Set contrast for Treatment
contrasts(cs_counts$Treatment) <- contr.treatment(levels(cs_counts$Treatment), base = 2)

# Fit candidate models
f1 <- glmmTMB(Event_Count ~ Treatment * Week, 
              data = cs_counts,
              family = nbinom2)

f2 <- glmmTMB(Event_Count ~ Treatment + Week, 
              data = cs_counts,
              family = nbinom2)

f3 <- glmmTMB(Event_Count ~ Treatment * Week + (1|Group), 
              data = cs_counts,
              family = nbinom2)

f4 <- glmmTMB(Event_Count ~ Treatment + Week + (1|Group), 
              data = cs_counts,
              family = nbinom2)


# Create model comparison table
freq_models <- list(
  "*Week" = f1,
  "+ Week" = f2,
  "*Week + Group" = f3,
  "+ Week + Group" = f4
)

freq_aic_table <- map_dfr(seq_along(freq_models), ~ {
  tibble(
    Model = names(freq_models)[.x],
    Formula = ifelse(inherits(freq_models[[.x]], "zeroinfl"),
                     paste(deparse(freq_models[[.x]]$formula), "|", deparse(freq_models[[.x]]$zeroformula)),
                     deparse(formula(freq_models[[.x]]))),
    AIC = AIC(freq_models[[.x]]),
    BIC = BIC(freq_models[[.x]]),
    logLik = as.numeric(logLik(freq_models[[.x]]))
  )
}) %>%
  arrange(AIC)

print(freq_aic_table)

### Best Model Analysis ----
best_freq_name <- freq_aic_table$Model[which.min(freq_aic_table$AIC)]
best_freq <- freq_models[[best_freq_name]]
summary(best_freq)
Anova(best_freq, type = "III")

### Post-hoc Analysis ----
# Treatment effects
tukey_treatment <- emmeans(best_freq, pairwise ~ Treatment, adjust = "tukey")
summary(tukey_treatment)
confint(tukey_treatment)

# Week effects if significant
tukey_week <- emmeans(best_freq, pairwise ~ Week, adjust = "tukey")
summary(tukey_week)

### Model Diagnostics ----
# Residual plots
plot(residuals(best_freq) ~ fitted(best_freq))
abline(h = 0, col = "red")
qqnorm(residuals(best_freq))
qqPlot(residuals(best_freq))

# Dispersion check
sim_res <- simulateResiduals(best_freq)
plot(sim_res)

## 3.2 Duration Analysis ----
### Model Selection ----
# Fit candidate models
d1 <- glmmTMB(Duration ~ Treatment * Week, 
              data = cs_data,
              family = Gamma(link = "log"))

d2 <- glmmTMB(Duration ~ Treatment + Week, 
              data = cs_data,
              family = Gamma(link = "log"))

d3 <- glmmTMB(Duration ~ Treatment * Week + (1|Group), 
              data = cs_data,
              family = Gamma(link = "log"))

d4 <- glmmTMB(Duration ~ Treatment + Week + (1|Group), 
              data = cs_data,
              family = Gamma(link = "log"))

dur_models <- list(
  "* Week" = d1,
  "+ Week" = d2,
  "* Week + Group" = d3,
  "+ Week + Group" = d4
)

# Model comparison
dur_aic_table <- map_dfr(seq_along(dur_models), ~ {
  tibble(
    Model = names(dur_models)[.x],
    Formula = ifelse(inherits(dur_models[[.x]], "zeroinfl"),
                     paste(deparse(dur_models[[.x]]$formula), "|", deparse(freq_models[[.x]]$zeroformula)),
                     deparse(formula(dur_models[[.x]]))),
    AIC = AIC(dur_models[[.x]]),
    BIC = BIC(dur_models[[.x]]),
    logLik = as.numeric(logLik(dur_models[[.x]]))
  )
}) %>%
  arrange(AIC)

print(dur_aic_table)

### Best Model Analysis ----
best_dur_name <- dur_aic_table$Model[which.min(dur_aic_table$AIC)]
best_dur <- dur_models[[best_dur_name]]
summary(best_dur)
Anova(best_dur, type = "III")

### Post-hoc Analysis ----
dur_emmeans <- emmeans(best_dur, pairwise ~ Treatment * Week, adjust = "tukey")
summary(dur_emmeans)

# Calculate effect sizes
dur_coefs <- fixef(best_dur)$cond
dur_effects <- exp(dur_coefs)  # Convert to multiplicative effects
print(dur_effects)

### Model Diagnostics ----
plot(residuals(best_dur) ~ fitted(best_dur))
qqPlot(residuals(best_dur))

# 4. VISUALIZATION ----
### 4.0 Plot ----
ggplot(cs_counts, 
                     aes(x = Treatment, y = Event_Count, fill = Treatment)) +
  
  # Half-violin + boxplot
  geom_violin(alpha = 0.7, width = 0.8, trim = FALSE) +
  geom_boxplot(width = 0.15, fill = "white", alpha = 0.9, 
               outlier.shape = NA) +
  
  ggsignif::geom_signif(
    comparisons = list(c("Nipple-WT", "Open-WT")),
    annotations = "***",  # Use your actual p-value symbol
    y_position = max(cs_counts$Event_Count) * 1.1,
    tip_length = 0.01,
    color = "black"
  ) +
  
  scale_fill_manual(values = (treatment_palette)) +  # Blue gradient
  
  # Minimal labels
  labs(
    title = "Cross-Sucking Event Counts by Treatment",
    x = "Treatments",
    y = "Event Count",
    caption = "***p=0.003 | GLMM-adjusted means | Boxes show IQR/median"
  ) +
  
  theme_classic(base_size = 14) +
  theme(
    plot.title = element_text(hjust = 0.5, face = "plain"),
    legend.position = "none",
    axis.text.x = element_text(color = "black"),
    panel.grid.major.y = element_line(color = "grey90")
  )

### 4.1 Duration by Treatment ----
ggplot(cs_data, aes(x = Treatment, y = Duration, fill = Treatment)) +
  geom_boxplot() +
  labs(title = "Cross-Sucking Duration by Treatment",
       y = "Duration (seconds)")

### 4.2 Body part distribution ----
ggplot(cs_data, aes(x = Treatment, fill = Body_part)) +
  geom_bar(position = "fill") +
  labs(title = "Body Part Distribution by Treatment",
       y = "Proportion")

### 4.3 Weekly trends ----
ggplot(cs_data, aes(x = Week, fill = Treatment)) +
  geom_bar(position = "dodge") +
  labs(title = "Cross-Sucking Frequency by Week",
       y = "Count")

### 4.4 Sex combinations ----
ggplot(cs_data, aes(x = interaction(Sex_Ex, Sex_Receiver), fill = Treatment)) +
  geom_bar(position = "dodge") +
  labs(title = "Cross-Sucking by Sex Combinations",
       x = "Executor-Receiver Sex Combination") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### 4.5 Calf participation ----
calf_csk_plot <- ggplot(executor_receiver_percent, 
                        aes(y = fct_relevel(as.factor(Calf)))) +  # Convert Calf to factor first
  geom_bar(aes(x = Percent_Executed, fill = "Executor"), 
           stat = "identity", width = 0.6) +
  geom_bar(aes(x = -Percent_Received, fill = "Receiver"), 
           stat = "identity", width = 0.6) +
  scale_x_continuous(
    labels = abs,
    limits = c(-100, 100),
    breaks = seq(-100, 100, 20)
  ) +
  scale_fill_manual(
    values = c("Executor" = "#1E90FF", "Receiver" = "#D43F3A"),
    name = "Role",
    labels = c("Receiver", "Executor"),
    guide = guide_legend(reverse = TRUE)
  ) +
  labs(
    x = "Proportion of Cross-sucking Events (%)",
    y = "Calf ID"
  ) +
  facet_wrap(~Treatment, scales = "free_y", ncol = 1) +
  theme_minimal(base_size = 15) +
  theme(
    legend.position = "top",
    panel.grid = element_blank(),
    strip.background = element_rect(fill = "lightgrey", color = "black"),
    strip.text = element_text(face = "bold")
  )

### 4.6 Body part frequency plot ----
body_part_freq <- data.frame(
  Body_part = c("navel", "ears", "neck", "snout", "tail", "belly", "head", "back"),
  frequency = c(19.1, 58.1, 12.5, 4.4, 1.5, 0.7, 0.7, 0.7)
)

ggplot(body_part_freq) +
  geom_circle(aes(x0 = seq(1, by = 10, length.out = nrow(body_part_freq)), 
                  y0 = 0, r = sqrt(body_part_freq$frequency / pi)), 
              color = "black", alpha = 0.5) +
  scale_x_continuous(breaks = seq(1, by = 10, length.out = nrow(body_part_freq)),
                     labels = body_part_freq$Body_part) +
  coord_fixed() +
  labs(title = "Frequency of Cross-Sucking by Body Part",
       x = "Body Part") +
  theme_minimal(base_size = 15) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank())

### 4.7 Temporal Heatmap ----
heatmap <- cleaned_data$behavior_heatmap

ggplot(heatmap, aes(x = Hour, y = Treatment, fill = Freq_Csk)) +
  geom_tile(color = "white", width = 1, height = 1) +  # Remove as bordas
  geom_tile(aes(fill = Freq_Csk, color = "white"), color = "white", width = 0.6, height = 0.2, alpha = 0.4) +
  geom_text(aes(label = Count_Csk), color = "black", size = 4) +  
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Frequência") +  # Azul contínuo
  geom_vline(xintercept = 7.5, color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 15.5, color = "red", linetype = "dashed", linewidth = 1) +
  labs(title = "Heatmap de Frequência de Cross-Sucking por Hora e Tratamento",
       x = "Hora",
       y = "Tratamentos") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.x = element_blank(), 
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 12)) + 
  scale_x_continuous(breaks = 7:16) +  
  facet_wrap(~ Treatment, scales = "free_y", nrow = 2)  

ggplot(heatmap, aes(x = Hour, y = Treatment, fill = Freq_Csk)) +
  geom_tile(color = NA, width = 1, height = 1) +  
  scale_fill_gradient(low = "lightblue", high = "darkblue", name = "Frequency of cross-sucking events") +  # Azul contínuo
  geom_vline(xintercept = 7.5, color = "red", linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = 15.5, color = "red", linetype = "dashed", linewidth = 1) +
  labs(x = "Hour",
       y = "Treatment") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        axis.ticks.x = element_blank(), 
        axis.text.y = element_text(size = 12)) + 
  scale_x_continuous(breaks = 7:16) +  
  geom_tile(aes(fill = Freq_Csk), fill = "white", width = 0.8, height = 0.15) +
  geom_text(aes(label = Count_Csk), color = "black", fontface = "bold", size = 4) 

