source("~/nipple-troughs-calf/scripts/00_config.R")

# Define file paths
list.files("~/nipple-troughs-calf/data/raw")

data_paths <- list(
  # Growth data
  growth = "~/nipple-troughs-calf/data/raw/01_growth/growth_measures.xlsx",
  # Behavior data
  behavior = "~/nipple-troughs-calf/data/raw/02_behavior/behavior_diurnal_scan.xlsx",
  behavior_heatmap = "~/nipple-troughs-calf/data/raw/02_behavior/behavior_post_milk_heatmap.xlsx",
  cross_suck = "~/nipple-troughs-calf/data/raw/02_behavior/behavior_cross_sucking.xlsx",
  # Environment data
  climate = "~/nipple-troughs-calf/data/raw/03_environment/environment_thi_records.xlsx",
  climate_daily = "~/nipple-troughs-calf/data/raw/03_environment/environment_daily_thi.xlsx",
  # Feeding data
  feed = "~/nipple-troughs-calf/data/raw/04_feeding/feeding_feed_visits.xlsx",
  milk = "~/nipple-troughs-calf/data/raw/04_feeding/feeding_milk_intake.xlsx",
  water_intake = "~/nipple-troughs-calf/data/raw/04_feeding/feeding_water_intake.xlsx",
  water_visits = "~/nipple-troughs-calf/data/raw/04_feeding/feeding_water_visits.xlsx"
)

group_mapping <- c(
  "Verde" = "First",
  "Amarelo" = "Second",
  "Laranja" = "Third",
  "Roxo" = "Fourth",
  "Azul" = "Fifth",
  "Vermelho" = "Sixth",
  "Lilás" = "Seventh",
  "Ouro" = "Eighth"
)


# Function to clean all datasets
clean_data <- function() {
  # Growth data
  growth <- read_excel(data_paths$growth) %>%
    filter(!id %in% c(1442, 35, 1449)) %>%
    mutate(Group = recode_factor(Group, !!!group_mapping)) %>%
    mutate(
      Treatment = case_when(
        Treatment == "Bico" ~ "Nipple-WT",
        Treatment == "Controle" ~ "Open-WT",
        TRUE ~ Treatment
      ),
      across(where(is.character), as.factor)
    )
  # Behavior data
  behavior <- read_excel(data_paths$behavior) %>%
    filter(!id %in% c(1442, 35, 1449),
           Week != 9) %>% 
    mutate(Group = recode_factor(Group, !!!group_mapping)) %>%
    mutate(
      Treatment = case_when(
        Treatment == "Bico" ~ "Nipple-WT",
        Treatment == "Controle" ~ "Open-WT",
        TRUE ~ Treatment
      ),
      across(where(is.character), as.factor)
    )
  # Cross suck data
  cross_suck <- read_excel(data_paths$cross_suck) %>%
    mutate(Group = recode_factor(Group, !!!group_mapping)) %>%
    mutate(
      Treatment = case_when(
        Treatment == "Bico" ~ "Nipple-WT",
        Treatment == "Controle" ~ "Open-WT",
        TRUE ~ Treatment
      ),
      across(where(is.character), as.factor)
    )
  # Behavior heatmap data
  behavior_heatmap <- read_excel(data_paths$behavior_heatmap) %>%
    mutate_if(is.character, as.factor)
  # Environment data
  climate <- read_excel(data_paths$climate) %>%
    mutate(
      across(where(is.character), as.factor)
    )
  climate_daily <- read_excel(data_paths$climate_daily) %>%
    mutate(
      across(where(is.character), as.factor)
      )
  # Feed data 
  feed <- read_excel(data_paths$feed) %>%
    mutate(Group = recode_factor(Group, !!!group_mapping)) %>%
    filter(!id %in% c(1442, 35, 1449),
           Week != 9) %>%
    mutate(
      Treatment = case_when(
        Treatment == "Bico" ~ "Nipple-WT",
        Treatment == "Controle" ~ "Open-WT",
        TRUE ~ Treatment
      ),
      across(where(is.character), as.factor)
    )
  # Milk data
  milk <- read_excel(data_paths$milk) %>%
    filter(!id %in% c(1442, 35, 1449)) %>%
    mutate(Group = recode_factor(Group, !!!group_mapping)) %>%
    mutate(
      Treatment = case_when(
        Treatment == "Bico" ~ "Nipple-WT",
        Treatment == "Controle" ~ "Open-WT",
        TRUE ~ Treatment
      ),
      across(where(is.character), as.factor)
    )
  # Water intake data
  water_intake <- read_excel(data_paths$water_intake) %>%
    mutate(Group = recode_factor(Group, !!!group_mapping)) %>%
    mutate(
      Treatment = case_when(
        Treatment == "Bico" ~ "Nipple-WT",
        Treatment == "Controle" ~ "Open-WT",
        TRUE ~ Treatment
      ),
      across(where(is.character), as.factor)
    )
  # Water visits data
  water_visits <- read_excel(data_paths$water_visits) %>%
    mutate(Group = recode_factor(Group, !!!group_mapping)) %>%
    filter(!id %in% c(1442, 35, 1449),
           Week != 9) %>% 
    mutate(
      Treatment = case_when(
        Treatment == "Bico" ~ "Nipple-WT",
        Treatment == "Controle" ~ "Open-WT",
        TRUE ~ Treatment
      ),
      across(where(is.character), as.factor)
    )
  
  # Return cleaned data
  list(
    growth = growth,
    behavior = behavior,
    behavior_heatmap = behavior_heatmap,
    cross_suck = cross_suck,
    climate = climate,
    climate_daily = climate_daily,
    feed = feed,
    milk = milk,
    water_intake = water_intake,
    water_visits = water_visits
  )
}

# Save cleaned data
cleaned_data <- clean_data()
saveRDS(cleaned_data, "~/nipple-troughs-calf/data/processed/cleaned_data.rds")

