source("scripts/00_config.R")

# Define file paths
data_paths <- list(
  climate = "data/raw/16_Climate.xlsx",
  growth = "data/raw/1_Desempenho.xlsx",
  behavior = "data/raw/14_Comp_hora.xlsx",
  water = "data/raw/4_Eventos_Agua.xlsx",
  feed = "data/raw/7_Eventos_Raçao.xlsx",
  cross_suck = "data/raw/10_Cross-sucking.xlsx"
)

# Function to clean all datasets
clean_data <- function() {
  
  # Climate data
  climate <- read_excel(data_paths$climate) %>%
    mutate(Date = as.Date(Date),
           Hour = as.factor(Hour))
  
  # Growth data
  growth <- read_excel(data_paths$growth) %>%
    filter(!id %in% c(1442, 35, 1449)) %>%
    mutate(
      Treatment = recode(Treatment, "Bico" = "Nipple-WT", "Controle" = "Open-WT"),
      across(c(Group, id, Sex, Breed), as.factor),
      Week = as.numeric(Week)
    )
  
  # Behavior data
  behavior <- read_excel(data_paths$behavior) %>%
    filter(!id %in% c(1442, 35, 1449)) %>%
    mutate(
      Treatment = as.factor(Treatment),
      Hour = as.factor(Hour)
    )
  
  # Return cleaned data
  list(
    climate = climate,
    growth = growth,
    behavior = behavior,
    water = water,
    feed = feed,
    cross_suck = cross_suck
  )
}

# Save cleaned data
cleaned_data <- clean_data()
saveRDS(cleaned_data, "data/processed/cleaned_data.rds")
