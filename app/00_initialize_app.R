### -------------------------------------------------------------------
### SubsidyExplorer
### An interactive toolkit to explore the tradeoffs across proposed fisheries subsidies disciplines
### 
### Creator(s): Kat Millage and Vienna Saccomanno
### Release date (v1): July 2019
### Release date (v2): 
### 
### This script loads data needed for the app and performs some final data wrangling
### --------------------------------------------------------------------

### --------------
### Text ---------
### --------------

# This file contains all of the button, widget label, tab, and header text
text <- read_csv("./text/00_button_and_widget_text.csv")

# This file contains the values and display names for the widgets
wid <- read_csv("./text/00_widget_values.csv") %>%
  group_by(tab_num, tab_id, item_id) %>%
  summarize(choices = paste0(choice, collapse = ", "),
            choice_names = paste0(choice_name, collapse = ", "),
            selected = paste0(choice[selected == TRUE], collapse = ", "),
            min = unique(min),
            max = unique(max), 
            value = unique(value),
            step = unique(step)) %>%
  group_by(tab_num, tab_id, item_id) %>%
  mutate(choices = list(setNames(unlist(str_split(choices, ", ")),
                                 unlist(str_split(choice_names, ", ")))),
         selected = list(unlist(str_split(selected, ", "))))

### ----------------------------------
### Country/Territory Naming ---------
### ----------------------------------

# Load list of WTO Member states and their dependencies with chosen display names
country_lookup <- read_csv("./data/country_lookup.csv") %>%
  mutate(display_name = case_when(!is.na(WTO_name) ~ WTO_name,
                                  TRUE ~ countrycode(iso3, "iso3c", "country.name"))) %>%
  arrange(sovereign_iso3) 

# Vector of all WTO Members and Observers for use in the app
wto_members_and_observers <- country_lookup$iso3[country_lookup$WTO_member_status %in% c("Member", "Observer")]
names(wto_members_and_observers) <- country_lookup$display_name[country_lookup$WTO_member_status %in% c("Member", "Observer")]

# EU states
eu_countries <- country_lookup$iso3[country_lookup$iso3 != "EU" & country_lookup$is_EU]

# List of all EU overseas territories - vessels flagged here should be considered WTO Members
eu_territories <- country_lookup$iso3[country_lookup$is_overseas_territory & country_lookup$sovereign_iso3 %in% eu_countries]

# List of all US overseas territories - vessels flagged here should be considered WTO Members
us_territories <- country_lookup$iso3[country_lookup$is_overseas_territory & country_lookup$sovereign_iso3 == "USA"]

# List of all Norwegian overseas territories - vessels flagged here should be considered WTO Members
norwegian_territories <- country_lookup$iso3[country_lookup$is_overseas_territory & country_lookup$sovereign_iso3 == "NOR"]

# ### --------------------
# ### Shapefiles ---------
# ### --------------------

# 1) World map ---
world <- read_sf("./data/shapefiles/ne_50m_admin_SubsidyExplorer", layer = "land_50m")

# Identify small countries for which we are going to add little dots on the map for easier viewing
world_small_countries <- world  %>%
  dplyr::filter(area_km < 300) %>%
  mutate(center = st_centroid(geometry))

# # 2) EEZs/FAO regions ---
# eez_fao <- read_sf(dsn = "./data/shapefiles_edit/eez_v10_fao_combined_simple", layer="eez_v10_fao_combined_simple") %>% 
#   mutate(eez_hs_code = ifelse(!is.na(zone), paste0("HS-",zone), as.character(mrgid)))
# 
# ### --------------------
# ### Data ---------------
# ### --------------------

### SUBSIDY DATA -----------------------------------------------------------------------------------

# 1) Subsidy estimates (Sumaila et al. 2019) ---
subsidy_dat_sumaila_raw <- read_csv("./data/sumaila_et_al_2019_subsidies_tidy.csv") %>%
  arrange(iso3)

# 2) Subsidy estimates (OECD 2019) ---
subsidy_dat_oecd_raw <- read_csv("./data/oecd_2019_fse_tidy.csv") %>%
  arrange(iso3) %>%
  mutate(category = as.character(category)) %>%
  group_by(iso3, type) %>%
  dplyr::filter(year == max(year)) %>%
  ungroup()

# Organize subsidy types as defined by Sumaila et al. (2019) for consistent plotting throughout
subsidy_classification_sumaila <- subsidy_dat_sumaila_raw %>%
  distinct(category, category_name, type, type_name) %>%
  arrange(type)

subsidy_categories_sorted_sumaila <- unique(subsidy_classification_sumaila$category)
names(subsidy_categories_sorted_sumaila) <- unique(subsidy_classification_sumaila$category_name)

subsidy_types_sorted_sumaila <- subsidy_classification_sumaila$type
names(subsidy_types_sorted_sumaila) <- subsidy_classification_sumaila$type_name

# Organize subsidy types as defined by the OECD for consistent plotting throughout
subsidy_classification_oecd <- subsidy_dat_oecd_raw %>%
  distinct(category, category_name, type, type_name) %>%
  arrange(type)

subsidy_categories_sorted_oecd <- unique(subsidy_classification_oecd$category)
names(subsidy_categories_sorted_oecd) <- unique(subsidy_classification_oecd$category_name)

subsidy_types_sorted_oecd <- subsidy_classification_oecd$type
names(subsidy_types_sorted_oecd) <- subsidy_classification_oecd$type_name

# Merge subsidy datasets 
subsidy_dat <- subsidy_dat_sumaila_raw %>%
  bind_rows(subsidy_dat_oecd_raw) %>%
  mutate(category = factor(category, levels = c(subsidy_categories_sorted_sumaila, subsidy_categories_sorted_oecd), ordered = T),
         category_name = factor(category_name, levels = c(names(subsidy_categories_sorted_sumaila), names(subsidy_categories_sorted_oecd)), ordered = T),
         type = factor(type, levels = c(subsidy_types_sorted_sumaila, subsidy_types_sorted_oecd), ordered = T),
         type_name = factor(type_name, levels = c(names(subsidy_types_sorted_sumaila), names(subsidy_types_sorted_oecd)), ordered = T)) %>%
  left_join(country_lookup %>% dplyr::select(iso3, display_name), by = "iso3")
  
# Create color palettes for subsidy categories and types
goodColors <- rev(brewer.pal(3,"Blues"))
names(goodColors) <- names(subsidy_types_sorted_sumaila)[1:3]

badColors <- rev(brewer.pal(7, "Reds"))
names(badColors) <- names(subsidy_types_sorted_sumaila)[4:10]

ambigColors <- c("purple", "mediumorchid", "violet")
names(ambigColors) <- names(subsidy_types_sorted_sumaila)[11:13]

oecdColors <- rev(colorRampPalette(c("white", "black"), interpolate = "linear")(length(subsidy_types_sorted_oecd)+5))
oecdColors <- oecdColors[1:length(subsidy_types_sorted_oecd)]
names(oecdColors) <- names(subsidy_types_sorted_oecd)

totColor <- "#3c8dbc"
names(totColor) <- "Total"

myColors <- c(goodColors, badColors, ambigColors, totColor, oecdColors)

categoryColors <- c(goodColors[1], badColors[1], ambigColors[1])
names(categoryColors) <- names(subsidy_categories_sorted_sumaila)

### DEMOGRAPHIC DATA -----------------------------------------------------------------------------------

# 1) GDP and Population (World Bank WDI database 2020) ---
demo_dat_world_bank <- read_csv("./data/world_bank_2020_wdi_tidy.csv") %>%
  arrange(iso3)

# 2) Number of fishers (FAO Yearbook 2017) ---
fisher_dat_fao <- read_csv("./data/fao_2017_yearbook_fishers_tidy.csv") %>%
  arrange(iso3)

# 3) Full-time fisheries employment (Teh and Sumaila 2013) ---
fisher_dat_teh_and_sumaila <- read_csv("./data/teh_and_sumaila_2013_fte_tidy.csv") %>%
  arrange(iso3)

# Aggregate
demographic_dat <- demo_dat_world_bank %>%
  bind_rows(fisher_dat_fao) %>%
  bind_rows(fisher_dat_teh_and_sumaila)

### FISHERIES DATA -----------------------------------------------------------------------------------

# 1) FAO Cature Production by ISSCAAP Group (2000-2017)

capture_production_dat_fao <- read_csv("./data/fao_2019_capture_production_isscaap_groups_tidy.csv") %>%
  group_by(iso3, year) %>%
  mutate(prop_annual_total = value/sum(value)) %>%
  ungroup()

capture_production_dat_tot <- capture_production_dat_fao %>%
  group_by(iso3, year, variable, units, source) %>%
  summarize(value = sum(value, na.rm = T))

# 2) Landed value by ISSCAAP Group (2000-2017)
landed_value_dat <- read_csv("./data/estimated_landed_value_isscaap_groups_tidy.csv") %>%
  group_by(iso3, year) %>%
  mutate(prop_annual_total = value/sum(value)) %>%
  ungroup()

landed_value_dat_tot <- landed_value_dat %>%
  group_by(iso3, year, variable, units, source) %>%
  summarize(value = sum(value, na.rm = T))

# 3) GFW Vessel list (2018)
# pro_rate_subsidies <- F
# 
# vessel_dat <- read.csv("./data/vessel_list_2018_final.csv", stringsAsFactors = F)
# 
# if(pro_rate_subsidies == T){
#   
#   vessel_dat <- vessel_dat %>%
#     mutate(B1_subs = B1_subs * 0.54, # boat construction/renovation - matched to payments based on vessels
#            B2_subs = B2_subs * 1, # fishery development projects/support services - matched to payments based on variable use
#            B3_subs = B3_subs * 0.87, # port construction and renovation - matched to payments based on vessels
#            B4_subs = B4_subs * 0.87, # price/marketing support, processing infrastructure - matched to payments based on output
#            B5_subs = B5_subs * 0.76, # non-fuel tax exemptions - matched to payments based on fishers income
#            B6_subs = B6_subs * 0.56, # foreign access agreements - matched to payments based on fishers own capital
#            B7_subs = B7_subs * 0.84, # fuel - matched to payments based on fuel use
#            bad_subs = (B1_subs + B2_subs + B3_subs + B4_subs + B5_subs + B6_subs + B7_subs))
#   
# }
# 
# vessel_dat <- vessel_dat %>%
#   mutate(eez_hs_code = case_when(eez_id == 0 ~ paste0("HS-", fao_region),
#                                  TRUE ~ as.character(eez_id)))

# 4) Biological parameters for the model
bio_dat <- read.csv("./data/regional_model_parameters.csv")

### POLICY DATA -----------------------------------------------------------------------------------

# 1) Cap/tier data (from US proposal) ---
cap_tier_dat <- read_csv("./data/USA_cap_tier_tidy.csv") %>%
  arrange(iso3)

# 2) Proposal settings
# proposal_settings <- read.csv("./data/wto-proposal-settings.csv", stringsAsFactors = F)
# default_settings <- proposal_settings %>% dplyr::filter(proposal == "Default")

# # Proposal names
# proposal_names <- proposal_settings %>%
#   mutate(display_name = case_when(proposal == "Default" ~ "None",
#                                   TRUE ~ paste0(title, " (", proposal, ")"))) %>%
#   dplyr::select(proposal, display_name)
# proposal_choices <- proposal_names$proposal
# names(proposal_choices) <- proposal_names$display_name
# 