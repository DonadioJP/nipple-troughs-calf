# 1. SETUP ----
source("scripts/00_config.R")
library(ggforce)       # Circle plots
library(pscl)          # Zero-inflated models
library(patchwork)     # Plot composition

# 2. DATA PREP ----
cs_data <- cleaned_data$cross_suck |> 
  mutate(
    # Temporal features
    DateTime = ymd_hms(paste(Date, Time)),
    Hour = hour(DateTime),
    # Behavioral features
    After_Milk = ifelse(Hour %in% c(7, 15), "0-2h Post", "Other"),
    Body_part = fct_lump_min(Body_part, min = 5)  # Group rare categories
  ) |> 
  # Merge milk feeding times
  left_join(
    cleaned_data$milk |> select(id, DateTime = DateTime, Milk_Duration = Duration),
    by = c("id", "DateTime")
  )

# 3. ANALYSIS ----
## 3.1 Frequency Analysis ----
freq_model <- glmmTMB(
  Event_Count ~ Treatment * Week + (1 | id) + (1 | Body_part),
  data = cs_data |> group_by(id, Treatment, Week) |> tally(name = "Event_Count"),
  family = nbinom2
)

## 3.2 Duration Analysis ----
dur_model <- glmmTMB(
  Duration ~ Treatment * After_Milk + (1 | id),
  data = cs_data,
  family = Gamma(link = "log")
)

## 3.3 Zero-Inflated Model ----
zi_model <- zeroinfl(
  Event_Count ~ Treatment | Week,
  data = cs_data |> group_by(id, Treatment, Week) |> tally(name = "Event_Count"),
  dist = "negbin"
)

# 4. DIAGNOSTICS ----
## 4.1 Model validation
sim_res <- simulateResiduals(freq_model)
plot(sim_res)

## 4.2 Zero-inflation check
testZeroInflation(sim_res)

## 4.3 Temporal autocorrelation
acf(resid(dur_model))

# 5. VISUALIZATION ----
## 5.1 Body Part Frequency ----
body_freq <- cs_data |> 
  count(Body_part, Treatment, name = "Frequency") |> 
  group_by(Treatment) |> 
  mutate(Proportion = Frequency/sum(Frequency))

circle_plot <- ggplot(body_freq) +
  geom_circle(aes(x0 = 0, y0 = 0, r = Proportion*2, fill = Body_part)) +
  coord_fixed() +
  facet_wrap(~ Treatment) +
  scale_fill_viridis_d() +
  labs(title = "Cross-sucking Distribution by Body Part",
       subtitle = "Circle area represents relative frequency") +
  theme_void() +
  theme(legend.position = "bottom")

## 5.2 Temporal Heatmap ----
heatmap <- cs_data |> 
  count(Treatment, Hour) |> 
  ggplot(aes(Hour, Treatment, fill = n)) +
  geom_tile() +
  geom_vline(xintercept = c(7, 15), color = "red", linetype = 2) +
  scale_fill_viridis_c(option = "inferno") +
  labs(title = "Hourly Cross-sucking Frequency",
       x = "Hour of Day", y = "", fill = "Events") +
  theme_custom()

## 5.3 Milk Feeding Relationship ----
milk_plot <- cs_data |> 
  filter(!is.na(Milk_Duration)) |> 
  ggplot(aes(Milk_Duration, Duration, color = Treatment)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  scale_color_manual(values = treatment_palette) +
  labs(title = "Cross-sucking Duration vs Milk Feeding Time",
       x = "Milk Feeding Duration (min)", y = "CS Event Duration (sec)") +
  theme_custom()

# 6. COMPOSITE FIGURE ----
composite_fig <- (circle_plot + heatmap) / milk_plot +
  plot_annotation(tag_levels = 'A') +
  plot_layout(heights = c(1, 0.8))

# 7. SAVE OUTPUTS ----
saveRDS(freq_model, "outputs/cross_suck/frequency_model.rds")
ggsave("figures/cross_suck/composite_analysis.png", composite_fig, 
       width = 12, height = 10)
ggsave("figures/cross_suck/body_part_circles.png", circle_plot,
       width = 8, height = 6)

# 8. REPORTING ----
sink("outputs/cross_suck/full_analysis.txt")
cat("=== FREQUENCY MODEL SUMMARY ===\n")
print(summary(freq_model))
cat("\n\n=== ZERO-INFLATION ANALYSIS ===\n")
print(summary(zi_model))
cat("\n\n=== BODY PART DISTRIBUTION ===\n")
print(body_freq)
sink()