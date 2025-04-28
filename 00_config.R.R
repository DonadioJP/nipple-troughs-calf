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