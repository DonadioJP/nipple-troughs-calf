# 1. SETUP ----
source("scripts/00_config.R")
library(MASS)       # Negative binomial models
library(ggeffects)  # Marginal effects

# 2. DATA PREP ----
feed <- cleaned_data$feed |> 
  mutate(
    # Temporal features
    Hour = factor(hour(ymd_hms(paste(Date, Time))),
                  # Visit characteristics
                  Duration_min = Duration/60,
                  # Categorical periods
                  Period = case_when(
                    hour(ymd_hms(paste(Date, Time))) %in% 6:12 ~ "Morning",
                    hour(ymd_hms(paste(Date, Time))) %in% 13:18 ~ "Afternoon",
                    TRUE ~ "Night"
                  )
    ) |> 
      # Remove outliers (>2h visits)
      filter(Duration_min < 120)  
    
    # 3. ANALYSIS ----
    ## 3.1 Visit Frequency ----
    feed_visit_model <- glmer.nb(
      Visit_Count ~ Treatment * Week + (1 | id) + (1 | Hour),
      data = feed |> 
        group_by(id, Date, Treatment, Week) |> 
        tally(name = "Visit_Count")
    )
    
    ## 3.2 Duration Analysis ----
    duration_model <- lmer(
      log(Duration_min) ~ Treatment * Period + (1 | id),
      data = feed
    )
    
    ## 3.3 Temporal Patterns ----
    temporal_model <- glmer(
      Visit_Count ~ Treatment * Hour + (1 | id),
      data = feed |> 
        group_by(id, Treatment, Hour) |> 
        tally(name = "Visit_Count"),
      family = poisson
    )
    
    # 4. DIAGNOSTICS ----
    ## 4.1 Overdispersion check
    performance::check_overdispersion(feed_visit_model)
    
    ## 4.2 Temporal autocorrelation
    acf(resid(duration_model))
    
    # 5. VISUALIZATION ----
    ## 5.1 Visit Frequency Trends ----
    visit_plot <- ggpredict(feed_visit_model, terms = c("Week", "Treatment")) |> 
      plot() +
      labs(title = "Feed Visit Frequency by Week",
           x = "Week", y = "Predicted Visits/Day") +
      scale_color_manual(values = treatment_palette) +
      theme_custom()
    
    ## 5.2 Duration Distribution ----
    duration_plot <- ggplot(feed, aes(Duration_min, fill = Treatment)) +
      geom_density(alpha = 0.6) +
      scale_fill_manual(values = treatment_palette) +
      labs(title = "Feed Visit Duration Distribution",
           x = "Duration (minutes)", y = "Density") +
      theme_custom() +
      theme(legend.position = "top")
    
    ## 5.3 Hourly Patterns ----
    hourly_plot <- ggpredict(temporal_model, terms = c("Hour", "Treatment")) |> 
      plot() +
      labs(title = "Hourly Feed Visit Patterns",
           x = "Hour of Day", y = "Predicted Visits") +
      scale_color_manual(values = treatment_palette) +
      theme_custom()
    
    # 6. SAVE OUTPUTS ----
    saveRDS(feed_visit_model, "outputs/feed/visit_frequency_model.rds")
    ggsave("figures/feed/visit_frequency.png", visit_plot, width = 8, height = 5)
    ggsave("figures/feed/hourly_patterns.png", hourly_plot, width = 7, height = 5)
    
    # 7. REPORTING ----
    sink("outputs/feed/analysis_summary.txt")
    cat("=== FEED VISIT FREQUENCY MODEL ===\n")
    print(summary(feed_visit_model))
    cat("\n\n=== DURATION ANALYSIS ===\n")
    print(summary(duration_model))
    cat("\n\n=== TEMPORAL PATTERNS ===\n")
    print(ggemmeans(temporal_model, terms = c("Hour", "Treatment")))
    sink()