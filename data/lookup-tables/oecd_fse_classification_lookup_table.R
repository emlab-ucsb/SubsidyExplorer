########################################################
#
# Create a lookup table for the OECD FSE data
# The heiarchy of support types is somewhat complicated and not logical, therefore we need a lookup table
#
########################################################

library(janitor)
library(here)
library(stringr)
library(tidyverse)

# Directories for the output file
output_dir <- here::here("data", "lookup-tables/")
if (dir.exists(output_dir) == F) {
  dir.create(output_dir, recursive = T)
}

# Where is the raw OECD FSE data stored? 
oecd_fse_path <- here::here("data", "raw", "OECD-2019-fse", "FISH_FSE_10032020183349581.csv")

# Load and wrangle OECD data (2009 - 2017)
oecd_fse_dat <- read_csv(oecd_fse_path) %>%
  clean_names(case = "snake") %>%
  dplyr::select(iso3 = country, country = country_2, code = variable, name = variable_2, measure, year, units = unit_code, value) %>%
  dplyr::filter(measure == "USD", # only pull estimates in USD for consistancy
                iso3 != "OECD") # remove totals

# Annoying, but the OECD classification scheme is a little more complicated and the type codes don't tell us anything about the classification scheme just by looking at them so we are going to manually assign more meaningful ones to make sure we're not double counting. 
oecd_types_table <- oecd_fse_dat %>%
  distinct(code, name) 

# Create blank lookup table
oecd_lookup <- tibble(category = character(0),
                      category_name = character(0))

# Categories
categories <- enframe(
  c("0" = "TRANSFERS TO INDIVIDUAL FISHERS - Non Budgetary",
    "1" = "TRANSFERS TO INDIVIDUAL FISHERS - Budgetary",
    "2" = "GENERAL SERVICE SUPPORT ESTIMATE",
    "3" = "COST RECOVERY CHARGES"), name = "category", value = "category_name")

oecd_lookup <- oecd_lookup %>%
  bind_rows(categories)

# Non budgetary transfers to individual fishers
types <- enframe(
  c("0A" = "Market price support",
    "0B" = "Fuel tax concessions",
    "1A" = "Transfers based on input use",
    "1B" = "Transfers based on fishers income",
    "1C" = "Transfers based on the reduction of productive capacity",
    "1D" = "Miscellaneous transfers to fishers",
    "2A" = "Access to other countries’ waters",
    "2B" = "Provision of infrastructure",
    "2C" = "Marketing and promotion",
    "2D" = "Support to fishing communities",
    "2E" = "Education and training",
    "2F" = "Research and development",
    "2G" = "Management of resources",
    "2H" = "Miscellaneous transfers to general services",
    "3A" = "Cost Recovery Charges, for resource access rights",
    "3B" = "Cost Recovery Charges, for infrastructure access",
    "3C" = "Cost Recovery Charges, for management, research and enforcement",
    #"3-D" = "Resource rent taxes and charges", #doesn't appear in this data set?
    "3E" = "Cost Recovery Charges, other"), name = "type", value = "type_name") %>%
  mutate(category = str_replace(type, "[A-Z]", ""))

oecd_lookup <- oecd_lookup %>%
  left_join(types, by = "category")

# Subtypes
subtypes <- enframe(
  c("1A-1" = "Transfers based on variable input use",
    "1A-2" = "Transfers based on fixed capital formation",
    "1B-1" = "Income support",
    "1B-2" = "Special insurance system for fishers",
    "2B-1" = "Capital expenditures",
    "2B-2" = "Subsidized access to infrastructure",
    "2G-1" = "Management expenditures",
    "2G-2" = "Stock enhancement programs",
    "2G-3" = "Enforcement expenditures"), name = "subtype", value = "subtype_name") %>%
  mutate(type = str_replace(subtype, "-[^-]*$", ""))

oecd_lookup <- oecd_lookup %>%
  left_join(subtypes, by = "type")

# Sub-subtypes
ssubtypes <- enframe(
  c("1A-2a" = "Support to vessel construction/purchase",
    "1A-2b" = "Support to modernisation",
    "1A-2c" = "Support to other fixed costs"), name = "ssubtype", "ssubtype_name") %>%
  mutate(subtype = str_replace(ssubtype, "[a-z]$", ""))

oecd_lookup <- oecd_lookup %>%
  left_join(ssubtypes, by = "subtype")

# Now rejoin to get a better sense of organization
oecd_lookup <- oecd_lookup %>%
  left_join(oecd_types_table, by = c("category_name" = "name")) %>%
  rename(category_code = code) %>%
  left_join(oecd_types_table, by = c("type_name" = "name")) %>%
  rename(type_code = code) %>%
  left_join(oecd_types_table, by = c("subtype_name" = "name")) %>%
  rename(subtype_code = code) %>%
  left_join(oecd_types_table, by = c("ssubtype_name" = "name")) %>%
  rename(ssubtype_code = code) %>%
  dplyr::select(category, category_code, category_name, type, type_code, type_name, subtype, subtype_code, subtype_name, ssubtype, ssubtype_code, ssubtype_name)

# Make category names sentance case
oecd_lookup <- oecd_lookup %>%
  mutate(category_name = str_to_sentence(category_name),
         type_name = str_to_sentence(type_name),
         subtype_name = str_to_sentence(subtype_name))
  
# Save
write_csv(oecd_lookup, paste0(output_dir, "oecd_fse_classification_lookup_table.csv"))

