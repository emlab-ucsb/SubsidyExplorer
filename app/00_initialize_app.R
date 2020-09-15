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
text <- read_csv("./text/00_button_and_widget_text.csv") %>%
  dplyr::filter(!is.na(tab_num))

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
  ungroup() %>%
  group_by(tab_num, tab_id, item_id) %>%
  mutate(choices = list(setNames(unlist(str_split(choices, ", ")),
                                 unlist(str_split(choice_names, ", ")))),
         selected = list(unlist(str_split(selected, ", ")))) %>%
  ungroup()

### ----------------------------------
### Country/Territory Naming ---------
### ----------------------------------

# Load list of WTO Member states and their dependencies with chosen display names
country_lookup <- read_csv("./data/country_dependencies.csv") %>%
  mutate(display_name = case_when(!is.na(WTO_name) ~ WTO_name,
                                  iso3 == "ANT" ~ "Netherlands Antilles",
                                  iso3 == "ASC" ~ "Ascension Island", 
                                  iso3 == "CPT" ~ "Clipperton Island",
                                  iso3 == "TAA" ~ "Tristan da Cunha",
                                  TRUE ~ countrycode(iso3, "iso3c", "country.name"))) %>%
  arrange(sovereign_iso3) 

# Need to add an entry for the "EU" as an aggregate 
eu_entry <- tibble(iso3 = "EU", sovereign_iso3 = "EU", WTO_name = "European Union", WTO_status = "Member", is_EU = T, is_overseas_territory = F, is_permanently_inhabited = T, development_status = "Developed", is_WTO = T, display_name = "European Union")

country_lookup <- country_lookup %>%
  bind_rows(eu_entry)

# Vector of all WTO Members and Observers for use in the app [does not include the overseas territories associated with them]
wto_members_and_observers <- country_lookup$iso3[country_lookup$is_WTO & !country_lookup$is_overseas_territory]
names(wto_members_and_observers) <- country_lookup$display_name[country_lookup$iso3 %in% wto_members_and_observers]

# EU states
eu_countries <- country_lookup$iso3[country_lookup$is_EU]
names(eu_countries) <- country_lookup$display_name[country_lookup$iso3 %in% eu_countries]

# List of all EU overseas territories - vessels flagged here should be considered WTO Members
eu_territories <- country_lookup$iso3[country_lookup$is_overseas_territory & country_lookup$sovereign_iso3 %in% eu_countries]
names(eu_territories) <- country_lookup$display_name[country_lookup$iso3 %in% eu_territories]

# All WTO territories (with their sovereign states) -  vessels flagged here should be considered WTO Members
territories <- country_lookup %>%
  dplyr::filter(is_overseas_territory & is_WTO) %>%
  distinct(iso3, sovereign_iso3)

# ### --------------------
# ### Shapefiles ---------
# ### --------------------

# 1) World map ---
world <- read_sf("./data/shapefiles/ne_50m_admin_SubsidyExplorer", layer = "land_50m")

# Identify small countries for which we are going to add little dots on the map for easier viewing
world_small_countries <- world  %>%
  dplyr::filter(area_km < 300) %>%
  mutate(center = st_centroid(geometry))

# 2) EEZs/FAO regions ---
eez_fao <- read_sf(dsn = "./data/shapefiles/eez-v10-fao-combined-simple", layer="eez_v10_fao_combined_simple") %>%
  mutate(eez_hs_code = ifelse(!is.na(zone), paste0("HS-",zone), as.character(mrgid)))
 
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
  left_join(country_lookup %>% dplyr::select(iso3, display_name), by = "iso3") %>%
  arrange(iso3)

# 2) Number of fishers (FAO Yearbook 2017) ---
fisher_dat_fao <- read_csv("./data/fao_2017_yearbook_fishers_tidy.csv") %>%
  left_join(country_lookup %>% dplyr::select(iso3, display_name), by = "iso3") %>%
  arrange(iso3)

# 3) Full-time fisheries employment (Teh and Sumaila 2013) ---
fisher_dat_teh_and_sumaila <- read_csv("./data/teh_and_sumaila_2013_fte_tidy.csv") %>%
  left_join(country_lookup %>% dplyr::select(iso3, display_name), by = "iso3") %>%
  arrange(iso3)

# Aggregate
demographic_dat <- demo_dat_world_bank %>%
  bind_rows(fisher_dat_fao) %>%
  bind_rows(fisher_dat_teh_and_sumaila)

### FISHERIES DATA -----------------------------------------------------------------------------------

# 1) FAO Cature Production by ISSCAAP Group (2000-2017) - only for display purposes
capture_production_dat_fao <- read_csv("./data/fao_2020_capture_production_isscaap_groups_tidy.csv") %>%
  group_by(iso3, year) %>%
  mutate(prop_annual_total = value/sum(value)) %>%
  ungroup() %>%
  dplyr::filter(!(iso3 %in% unique(territories$sovereign_iso3))) %>% # remove entries for sovereign states without the data cooresponding to their territories 
  mutate(iso3 = str_replace(iso3, "-T", "")) %>% # rename entries for sovereign states that include data cooresponding to their territories
  left_join(country_lookup %>% dplyr::select(iso3, display_name), by = "iso3")

capture_production_dat_tot <- capture_production_dat_fao %>%
  group_by(iso3, year, variable, units, source, display_name) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  ungroup()

# 2) Landed value by ISSCAAP Group (2000-2017)
landed_value_dat <- read_csv("./data/estimated_landed_value_isscaap_groups_tidy.csv") %>%
  group_by(iso3, year) %>%
  mutate(prop_annual_total = value/sum(value)) %>%
  ungroup() %>%
  dplyr::filter(!(iso3 %in% unique(territories$sovereign_iso3))) %>% # remove entries for sovereign states without the data cooresponding to their territories 
  mutate(iso3 = str_replace(iso3, "-T", "")) %>% # rename entries for sovereign states that include data cooresponding to their territories
  left_join(country_lookup %>% dplyr::select(iso3, display_name), by = "iso3")

landed_value_dat_tot <- landed_value_dat %>%
  group_by(iso3, year, variable, units, source, display_name) %>%
  summarize(value = sum(value, na.rm = T)) %>%
  ungroup()
  
# 3) GFW Vessel list (2018)

vessel_dat <- read.csv("./data/vessel_list_2018_final.csv", stringsAsFactors = F) %>%
  left_join(country_lookup %>% dplyr::select(iso3, is_WTO), by = c("flag_iso3" = "iso3"))

# Load relative subsidy inpacts calculated by the OECD
rel <- read_csv("./data/oecd_relative_effects_lookup_table.csv")

# Apply "effective" subsidies
vessel_dat <- vessel_dat %>%
  mutate(B1_subs = B1_subs * rel$rel_effect_effort[rel$type == "B1"], 
         B2_subs = B2_subs * rel$rel_effect_effort[rel$type == "B2"],
         B3_subs = B3_subs * rel$rel_effect_effort[rel$type == "B3"],
         B4_subs = B4_subs * rel$rel_effect_effort[rel$type == "B4"],
         B5_subs = B5_subs * rel$rel_effect_effort[rel$type == "B5"], 
         B6_subs = B6_subs * rel$rel_effect_effort[rel$type == "B6"],
         B7_subs = B7_subs * rel$rel_effect_effort[rel$type == "B7"], 
         C1_subs = C1_subs * rel$rel_effect_effort[rel$type == "C1"], 
         C2_subs = C2_subs * rel$rel_effect_effort[rel$type == "C2"],
         C3_subs = C3_subs * rel$rel_effect_effort[rel$type == "C3"]) %>%
  ungroup() %>%
  mutate(bad_subs = rowSums(select(., one_of(paste0(subsidy_types_sorted_sumaila[4:10], "_subs")))),
         ugly_subs = rowSums(select(., one_of(paste0(subsidy_types_sorted_sumaila[11:13], "_subs")))),
         good_subs = rowSums(select(., one_of(paste0(subsidy_types_sorted_sumaila[1:3], "_subs")))),
         bad_ugly_subs = rowSums(select(., one_of(paste0(subsidy_types_sorted_sumaila[4:13], "_subs")))))

# Creat high seas/eez code
vessel_dat <- vessel_dat %>%
  mutate(eez_hs_code = case_when(eez_id == 0 ~ paste0("HS-", fao_region),
                                 TRUE ~ as.character(eez_id)))

# 4) Biological parameters for the model
bio_dat <- read.csv("./data/model_parameters_regional_3_regions.csv")

# Regional parameter list
bio_dat_list <- bio_dat %>%
  gather(region, value, -c(1:2)) %>%
  group_by(region) %>%
  group_split()

### POLICY DATA -----------------------------------------------------------------------------------

# # 1) Cap/tier data (from US proposal) ---
# cap_tier_dat <- read_csv("./data/USA_cap_tier_tidy.csv") %>%
#   arrange(iso3)

# 1) Cap/tier lookup table
cap_tier_lookup_table <- read_csv("./data/cap_tier_lookup_table.csv")

# 2) Proposal settings
proposal_settings <- read.csv("./data/wto_proposal_settings.csv", stringsAsFactors = F)

# Proposal names
included_proposals <- proposal_settings %>%
  dplyr::filter(include == "Yes") %>%
  mutate(display_name = case_when(proposal == "Default" ~ "None",
                                  TRUE ~ paste0(title_tool, " (", proposal, ")"))) %>%
  dplyr::select(category, proposal, display_name) %>%
  arrange(category)

proposal_choices <- included_proposals$proposal
names(proposal_choices) <- included_proposals$display_name

proposal_categories <- unique(included_proposals$category)[unique(included_proposals$category) != "Default"]

# 3) Management cutoff 
managed_cutoff <- 0.5

# 4) Default end year 
end_year <- 2060

### OTHER DATA -------------------------------------------------------------------------------------------

# Relative subsidies data
relative_subs_dat <- read_csv("./data/relative_subsidy_metrics_tidy.csv") %>%
  left_join(country_lookup %>% dplyr::select(iso3, display_name), by = "iso3") %>%
  arrange(iso3) %>%
  mutate(category = factor(category, levels = c(subsidy_categories_sorted_sumaila, subsidy_categories_sorted_oecd), ordered = T),
         category_name = factor(category_name, levels = c(names(subsidy_categories_sorted_sumaila), names(subsidy_categories_sorted_oecd)), ordered = T),
         type = factor(type, levels = c(subsidy_types_sorted_sumaila, subsidy_types_sorted_oecd), ordered = T),
         type_name = factor(type_name, levels = c(names(subsidy_types_sorted_sumaila), names(subsidy_types_sorted_oecd)), ordered = T))

# Create combined dataset for use in the "compare fishery stats" page
combined_fishery_stats_dat <- subsidy_dat %>%
  dplyr::filter(variable == "subsidies_Sumaila") %>%
  bind_rows(capture_production_dat_tot %>% dplyr::filter(year == 2017)) %>%
  bind_rows(landed_value_dat_tot %>% dplyr::filter(year == 2017)) %>%
  bind_rows(relative_subs_dat) %>%
  arrange(iso3, variable, type)

### GET MOST AMBITIOUS RESULTS --------------------------------------------------------------

# Create fleet (vessels)
remove_all_bad_fleet_vessels <- vessel_dat %>%
  mutate(fleet = case_when(is_WTO & fmi_best >= managed_cutoff & bad_ugly_subs > 0 ~ "affected_managed",
                           is_WTO & fmi_best < managed_cutoff & bad_ugly_subs > 0 ~ "affected_oa",
                           (!is_WTO & fmi_best >= managed_cutoff) ~ "unaffected_managed",
                           (!is_WTO & fmi_best < managed_cutoff) ~ "unaffected_oa",
                           TRUE ~ "unaffected_oa"))

# Create fleet (summary)
remove_all_bad_fleet_summary <- remove_all_bad_fleet_vessels %>%
  group_by(region, fleet) %>%
  summarize(catch = sum(catch, na.rm = T),
            bad_subs = sum(bad_ugly_subs, na.rm = T),
            fishing_KWh = sum(fishing_KWh_eez_fao_ter, na.rm = T),
            removed_subs = sum(bad_ugly_subs, na.rm = T)) %>%
  ungroup()

# Turn fleet summary into list by region
remove_all_bad_list <- remove_all_bad_fleet_summary %>%
  group_by(region) %>%
  group_split()
names(remove_all_bad_list) <- colnames(bio_dat)[-c(1:2)]

# Run model
remove_all_bad_results <- pmap_df(list(fleet = remove_all_bad_list, 
                                       region = names(remove_all_bad_list),
                                       bio_param = bio_dat_list),
                                  BioEconModel,
                                  end_year = 2100,
                                  return = "all")

# Extract results timeseries
remove_all_bad_results_full <- remove_all_bad_results %>%
  dplyr::filter(Year > 2018) %>%
  dplyr::filter(Variable %in% c("biomass", "catches_total", "revenue_total")) %>%
  group_by(Year, Variable, Fleet) %>%
  mutate(Diff = case_when(BAU != 0 ~ (Reform - BAU)/abs(BAU),
                          TRUE ~ 0),
         BAU_global = sum(BAU, na.rm = T),
         Reform_global = sum(Reform, na.rm = T),
         Diff_value_global = (Reform_global - BAU_global),
         Diff_global = case_when(BAU_global != 0 ~ (Reform_global - BAU_global)/abs(BAU_global),
                                 TRUE ~ 0)) %>%
  ungroup() %>%
  mutate(Id = "A",
         Name = "Most ambitious scenario",
         Type = "Reference",
         Description = "Complete removal of all capacity-enhancing subsidies (for comparison)")

# Extract ending results (difference only)
remove_all_bad_results_last <- remove_all_bad_results_full %>%
  dplyr::filter(Year == end_year) %>%
  group_by(Year, Variable, Id, Name, Type, Description) %>%
  summarize(Percent = unique(Diff_global)*100,
            Value = unique(Diff_value_global)) %>%
  ungroup() 


biomass_end_percent <- round(remove_all_bad_results_last$Percent[remove_all_bad_results_last$Variable == "biomass"])
biomass_end_value <- round(remove_all_bad_results_last$Value[remove_all_bad_results_last$Variable == "biomass"]/1e6)
catch_end_percent <- round(remove_all_bad_results_last$Percent[remove_all_bad_results_last$Variable == "catches_total"])
catch_end_value <- round(remove_all_bad_results_last$Value[remove_all_bad_results_last$Variable == "catches_total"]/1e6)


remove_all_bad_results_last <- remove_all_bad_results_last%>%
  spread(Variable, Percent) %>%
  rename(Biomass = biomass,
         Catches = catches_total,
         Revenue = revenue_total)

# Create data frame entry
best_result <- tibble(id = "A",
                      name = "Most ambitious scenario",
                      iuu = list("NA"),
                      oa = list("NA"),
                      overcap = list("NA"),
                      cap_tier = list("NA"),
                      policy_description = list(
                        paste0(
                          "<b>", "Name: ", "</b>", "Most ambitious scenario", "</br>",
                          "<b>", "Summary: ", "</b>", "Complete removal of capacity-enhancing subsidies", "</br>")
                      ),
                      fleet_summary = list(remove_all_bad_fleet_summary),
                      results_timeseries = list(remove_all_bad_results_full),
                      results_last = list(remove_all_bad_results_last))


