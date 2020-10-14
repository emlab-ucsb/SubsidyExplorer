
### The purpose of this script is to replicate the combined EEZ v10 and FAO region shapefile such that it is presented three times. 
# New extent will be (-540, 540)

library(sf)
library(rmapshaper)
library(mapview)
library(tidyverse)

eez_fao <- read_sf(dsn = "./app/data/shapefiles/eez-v10-fao-combined-simple", layer="eez_v10_fao_combined_simple") %>%
  mutate(eez_hs_code = ifelse(!is.na(zone), paste0("HS-",zone), as.character(mrgid))) %>%
  st_set_crs(4326)

# eez_fao_simple <- rmapshaper::ms_simplify(eez_fao, keep = 0.1, keep_shapes = T) %>%
#   st_set_crs(4326)
#   

# Make left duplicate (-540, -180)
eez_fao_left <- (st_geometry(eez_fao) - c(360, 0)) %>%
  st_set_crs(4326)

# Make right duplicate (180, 540)
eez_fao_right <- (st_geometry(eez_fao) + c(360, 0)) %>%
  st_set_crs(4326)

# Assign geometry
eez_fao_left <- st_set_geometry(eez_fao, eez_fao_left)
eez_fao_right <- st_set_geometry(eez_fao, eez_fao_right)

# EEZ fao all <
eez_fao_all <- eez_fao %>%
  rbind(eez_fao_left) %>% 
  rbind(eez_fao_right) %>%
  st_make_valid() %>%
  group_by(eez_hs_code) %>%
  summarize(a = 1) %>%
  ungroup() %>%
  dplyr::select(-a)

# Keep only multipolygons
eez_fao_polygon <- st_collection_extract(
  eez_fao_all,
  type = c("POLYGON"),
  warn = FALSE
)

#plot(eez_fao_polygon)

st_write(eez_fao_polygon, paste0(here::here("app", "data", "shapefiles/"), "eez-fao-triplicate"),
         layer = "eez-fao-triplicate",
         overwrite = T,
         driver = "ESRI Shapefile")

