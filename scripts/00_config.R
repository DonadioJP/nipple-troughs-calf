# Load packages
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(car)
library(ggplot2)
library(ggpubr)
library(ggeffects)
library(viridis)
library(multcomp)
library(readxl)
library(performance)
library(cowplot)
library(here)
library(lme4)       
library(emmeans)    
library(broom.mixed)
library(see)
library(dplyr)
library(purrr)
library(broom)
library(rstatix)
library(glmmTMB)    
library(multcomp)   
library(DHARMa)     
library(ggforce)       
library(pscl)         
library(patchwork)
library(lubridate)
library(corrplot)
library(ggridges)
library(ggsignif)
library(ggdist)
library(ggfx)

# Set global options
options(
  contrasts = c("contr.sum", "contr.poly"),
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

