########################################################
#
# Create a lookup table for the Sumaila et al. (2019) data
# The heiarchy is pretty simple, but we generate this to clean up some of the names
#
########################################################

library(janitor)
library(here)
library(stringr)
library(readxl)
library(tidyverse)

# Directories for the output file
output_dir <- here::here("data", "lookup-tables/")
if (dir.exists(output_dir) == F) {
  dir.create(output_dir, recursive = T)
}

# Where is the raw OECD FSE data stored? 
sumaila_subsidies_path <- here::here("data", "raw", "Sumaila-et-al-2019-subsidies", "Subsidies_PEW_Final.xlsx")

# Load and wrangle 2019 data
sumaila_2019_dat <- read_xlsx(sumaila_subsidies_path) %>%
  clean_names(case = "snake") %>%
  rename(type = class, type_name = type, category_name = category) %>%
  mutate(category = str_replace(type, "[0-9]", ""))

# Annoying, but we want to clean up some of the spelling of Sumaila's subsidy types
sumaila_types_table <- sumaila_2019_dat %>%
  distinct(category, category_name, type, type_name) 

# Lookup table
sumaila_lookup <- sumaila_types_table

# New spellings
sumaila_lookup$type_name[sumaila_lookup$type == "A1"] <- "Fisheries management"
sumaila_lookup$type_name[sumaila_lookup$type == "A2"] <- "Fishery research and development"
sumaila_lookup$type_name[sumaila_lookup$type == "A3"] <- "Marine protected areas"
sumaila_lookup$type_name[sumaila_lookup$type == "B1"] <- "Boat construction and renovation"
sumaila_lookup$type_name[sumaila_lookup$type == "B2"] <- "Fisheries development projects"
sumaila_lookup$type_name[sumaila_lookup$type == "B3"] <- "Fishing port development"
sumaila_lookup$type_name[sumaila_lookup$type == "B4"] <- "Marketing and storage infrastructure"
sumaila_lookup$type_name[sumaila_lookup$type == "B5"] <- "Non-fuel tax exemption"
sumaila_lookup$type_name[sumaila_lookup$type == "B6"] <- "Fishing access agreements"
sumaila_lookup$type_name[sumaila_lookup$type == "B7"] <- "Fuel subsidies"
sumaila_lookup$type_name[sumaila_lookup$type == "C1"] <- "Fisher assistance"
sumaila_lookup$type_name[sumaila_lookup$type == "C2"] <- "Vessel buyback programs"
sumaila_lookup$type_name[sumaila_lookup$type == "C3"] <- "Rural fisher communities"

# Save
write_csv(sumaila_lookup, paste0(output_dir, "sumaila_subsidies_classification_lookup_table.csv"))

