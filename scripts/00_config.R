# Package Installation (run once)
required_packages <- c(
  "tidyverse", "lme4", "lmerTest", "emmeans", "car", 
  "ggplot2", "ggpubr", "viridis", "multcomp", "glmmTMB",
  "readxl", "performance", "cowplot", "knitr", "broom.mixed"
)

# Install if needed
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# Load packages
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(car)
library(ggplot2)
library(ggpubr)
library(viridis)
library(multcomp)
library(readxl)
library(performance)
library(cowplot)

# Set global options
options(
  contrasts = c("contr.sum", "contr.poly"), # For Type III SS
  na.action = "na.omit",
  stringsAsFactors = FALSE
)

# Custom ggplot theme
theme_custom <- function() {
  theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "gray90"),
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "bottom"
    )
}

# Color palette
treatment_palette <- c("Nipple-WT" = "#1f78b4", "Open-WT" = "#33a02c")

# Define data paths
data_paths <- list(
  growth = here("data", "raw", "01_growth", "growth_measures.xlsx"),
  behavior_cs = here("data", "raw", "02_behavior", "behavior_cross_sucking.xlsx"),
  behavior_scan = here("data", "raw", "02_behavior", "behavior_diurnal_scan.xlsx"),
  behavior_milk = here("data", "raw", "02_behavior", "behavior_post_milk_heatmap.xlsx"),
  thi_records = here("data", "raw", "03_environment", "environment_thi_records.xlsx"),
  thi_daily = here("data", "raw", "03_environment", "environment_daily_thi.xlsx"),
  water_vol = here("data", "raw", "04_feeding", "feeding_water_intake.xlsx"),
  water_visits = here("data", "raw", "04_feeding", "feeding_water_visits.xlsx"),
  milk = here("data", "raw", "04_feeding", "feeding_milk_intake.xlsx"),
  feed = here("data", "raw", "04_feeding", "feeding_feed_visits.xlsx")
)

