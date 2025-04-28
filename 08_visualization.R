# nipple-troughs-calf-welfare/scripts/08_visualization.R

source("scripts/00_config.R")
cleaned_data <- readRDS("data/processed/cleaned_data.rds")

# Enhanced saving function with flexible options
save_plot <- function(plot_obj, filename, width = NULL, height = NULL, dpi = 300) {
  # Convert inches to mm for ggplot (1in = 25.4mm)
  if (!is.null(width)) width <- width * 25.4
  if (!is.null(height)) height <- height * 25.4
  
  ggsave(
    filename = here::here("figures/manuscript", filename),
    plot = plot_obj,
    width = width,
    height = height,
    dpi = dpi,
    bg = "white"
  )
}

#--------------------------------------------------------------
# MANUSCRIPT FIGURE 1: PADDOCK SCHEMATIC (EXTERNAL IMAGE)
#--------------------------------------------------------------

include_external_figure <- function(fig_number, description, filename) {
  fig_dir <- here::here("figures/external")
  if (!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)
  
  # Copy the external image to our figures directory
  file.copy(
    from = here::here("manuscript/assets", filename),
    to = here::here(fig_dir, filename),
    overwrite = TRUE
  )
  
  # Create a record in the figure log
  fig_log <- data.frame(
    figure_number = fig_number,
    type = "external",
    description = description,
    file_path = here::here(fig_dir, filename)
  )
  
  if (file.exists(here::here("figures/figure_log.csv"))) {
    write.table(fig_log, here::here("figures/figure_log.csv"), 
                append = TRUE, sep = ",", col.names = FALSE)
  } else {
    write.csv(fig_log, here::here("figures/figure_log.csv"), row.names = FALSE)
  }
  
  return(invisible(NULL))
}

# Register Figure 1 (schematic)
include_external_figure(
  fig_number = 1,
  description = "Paddock allocation schematic",
  filename = "paddock_schematic.png"
)

#--------------------------------------------------------------
# MANUSCRIPT FIGURE 2: BODY PART FREQUENCY (CIRCLE PLOT)
#--------------------------------------------------------------

plot_body_part_frequency <- function(cs_data) {
  # Calculate frequencies (matches your original analysis)
  body_part_freq <- cs_data %>%
    count(Body_part, Treatment) %>%
    group_by(Treatment) %>%
    mutate(
      percentage = n/sum(n)*100,
      radius = sqrt(percentage/max(percentage))*2  # Scale for visibility
    )
  
  # Create circle plot with treatment facets
  ggplot(body_part_freq) +
    ggforce::geom_circle(
      aes(x0 = 0, y0 = 0, r = radius, fill = Body_part),
      color = "white", size = 0.5
    ) +
    facet_wrap(~ Treatment) +
    coord_fixed() +
    scale_fill_viridis_d(option = "D") +
    labs(title = "Cross-sucking Frequency by Body Part",
         subtitle = "Circle size represents relative frequency (%)") +
    theme_void() +
    theme(
      legend.position = "bottom",
      strip.text = element_text(size = 12, face = "bold"),
      plot.title = element_text(hjust = 0.5, size = 14),
      plot.subtitle = element_text(hjust = 0.5, size = 10)
    )
}

#--------------------------------------------------------------
# MANUSCRIPT FIGURE 3: HEATMAPS (CS AND WATER VISITS)
#--------------------------------------------------------------

plot_combined_heatmaps <- function(cs_data, water_data) {
  # Panel A: Cross-sucking heatmap
  cs_heat <- cs_data %>%
    count(Treatment, Hour) %>%
    ggplot(aes(x = Hour, y = Treatment, fill = n)) +
    geom_tile(color = "white") +
    scale_fill_viridis_c(option = "B") +
    labs(title = "A) Cross-sucking Frequency",
         x = "Hour of Day", y = "") +
    theme_custom() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Panel B: Water visit heatmap
  water_heat <- water_data %>%
    count(Treatment, Hour) %>%
    ggplot(aes(x = Hour, y = Treatment, fill = n)) +
    geom_tile(color = "white") +
    scale_fill_viridis_c(option = "C") +
    labs(title = "B) Water Trough Visits",
         x = "Hour of Day", y = "") +
    theme_custom() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Combine panels
  (cs_heat / water_heat) + 
    plot_layout(heights = c(1, 1)) +
    plot_annotation(title = "Hourly Behavioral Frequencies",
                    theme = theme(plot.title = element_text(hjust = 0.5, size = 16)))
}

#--------------------------------------------------------------
# MANUSCRIPT FIGURE 4: BODY MEASUREMENTS GRID
#--------------------------------------------------------------

plot_body_measurements <- function(growth_data) {
  # Prepare measurements data
  measures <- growth_data %>%
    select(id, Treatment, Week, CC, WH, CH, CW, CL, BL) %>%
    pivot_longer(cols = c(CC, WH, CH, CW, CL, BL),
                 names_to = "measure", values_to = "value") %>%
    mutate(
      measure = factor(measure,
                       levels = c("CC", "WH", "CH", "CW", "CL", "BL"),
                       labels = c("Chest Girth", "Withers Height", "Rump Height",
                                  "Rump Width", "Rump Length", "Body Length"))
    )
  
  # Create faceted plot
  ggplot(measures, aes(x = Week, y = value, color = Treatment)) +
    geom_line(aes(group = id), alpha = 0.3) +
    stat_summary(fun = mean, geom = "line", size = 1.5) +
    facet_wrap(~ measure, scales = "free_y", ncol = 3) +
    scale_color_manual(values = treatment_palette) +
    labs(title = "Body Measurements Development",
         x = "Week", y = "Measurement (cm)") +
    theme_custom() +
    theme(legend.position = "bottom")
}

#--------------------------------------------------------------
# GENERATE ALL MANUSCRIPT FIGURES
#--------------------------------------------------------------

# Create directory structure
dir.create(here::here("figures/manuscript"), showWarnings = FALSE, recursive = TRUE)

# Generate manuscript figures
manuscript_figures <- list(
  fig2 = plot_body_part_frequency(cleaned_data$cross_suck),
  fig3 = plot_combined_heatmaps(cleaned_data$cross_suck, cleaned_data$water_events),
  fig4 = plot_body_measurements(cleaned_data$growth)
)

# Save with exact dimensions from manuscript
save_plot(manuscript_figures$fig2, "fig2_body_part_frequency.png", 
          width = 6.3, height = 2.7)
save_plot(manuscript_figures$fig3, "fig3_behavior_heatmaps.png", 
          width = 6.5, height = 4.5)  # Combined height of both panels
save_plot(manuscript_figures$fig4, "fig4_body_measurements.png", 
          width = 9.7, height = 5.0)

# Create figure metadata
figure_metadata <- tibble(
  figure = c("fig1", "fig2", "fig3", "fig4"),
  description = c(
    "Paddock allocation schematic",
    "Cross-sucking frequency by body part",
    "Hourly behavior heatmaps (cross-sucking and water visits)",
    "Body measurements development"
  ),
  generated_in_r = c(FALSE, TRUE, TRUE, TRUE),
  script = c(NA, "08_visualization.R", "08_visualization.R", "08_visualization.R")
)

write_csv(figure_metadata, here::here("figures/manuscript/figure_metadata.csv"))