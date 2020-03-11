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

button_text <- read_csv("./text/00_button_text.csv")

tab_text <- read_csv("./text/00_tab_text.csv")

widget_text <- read_csv("./text/00_widget_text.csv")

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

### SUBSIDIES -----------------------------------------------------------------------------------

# 1) Subsidy estimates (Sumaila et al. 2019) ---
subsidy_dat_sumaila_raw <- read_csv("./data/sumaila_et_al_2019_subsidies_tidy.csv") %>%
  arrange(iso3) %>%
  mutate(variable = "subsidies_Sumaila")

# 2) Subsidy estimates (OECD 2019) ---
subsidy_dat_oecd_raw <- read_csv("./data/oecd_2019_fse_tidy.csv") %>%
  arrange(iso3) %>%
  mutate(variable = "subsidies_OECD") %>%
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
names(goodColors) <- levels(subsidy_dat_sumaila$type_name)[1:3]

badColors <- rev(brewer.pal(7, "Reds"))
names(badColors) <- levels(subsidy_dat_sumaila$type_name)[4:10]

ambigColors <- c("purple", "mediumorchid", "violet")
names(ambigColors) <- levels(subsidy_dat_sumaila$type_name)[11:13]

oecdColors <- rev(colorRampPalette(c("white", "black"), interpolate = "linear")(length(subsidy_types_sorted_oecd)+5))
oecdColors <- oecdColors[1:length(subsidy_types_sorted_oecd)]
names(oecdColors) <- names(subsidy_types_sorted_oecd)

totColor <- "#3c8dbc"
names(totColor) <- "Total"

myColors <- c(goodColors, badColors, ambigColors, totColor, oecdColors)

categoryColors <- c(goodColors[1], badColors[1], ambigColors[1])
names(categoryColors) <- names(subsidy_categories_sorted_sumaila)

# # 2) Country profiles ---
# profile_dat_raw <- read.csv("./data/country-profiles-tidy.csv", stringsAsFactors = F)
# profile_dat_raw$subtype[profile_dat_raw$type == "B1"] <- "Boat construction and renovation"
# 
# # Get unique types so we can order them as factors
# types <- unique(profile_dat_raw$type)
# names(types) <- unique(profile_dat_raw$subtype)
# types_sorted <- sort(types) 
# 
# # Now put them in the order we want
# types_arranged <- c(types_sorted[26:38], types_sorted[39], types_sorted[1:25])
# subtypes_arranged <- names(types_arranged)
# categories_arranged <- c("Beneficial", "Capacity-enhancing", "Ambiguous", "Total", "Non Budgetary Transfers", "Budgetary Transfers", "General Service Support", "Cost Recovery Charges")
# 
# # Add back in to profile dat and order factors to aid with plotting throughout
# profile_dat <- profile_dat_raw %>%
#   mutate(type = factor(type, levels = types_arranged, ordered = T),
#          category = factor(category, levels = categories_arranged, ordered = T),
#          subtype = factor(subtype, levels = subtypes_arranged, ordered = T)) %>%
#   mutate(country = case_when(iso3 == "EU" ~ "European Union",
#                              iso3 == "TWN" ~ "Chinese Taipei",
#                              TRUE ~ countrycode(iso3, "iso3c", "country.name")))
# 
# # 3) Vessel list ---
# pro_rate_subsidies <- F
# 
# vessel_dat <- read.csv("./data/vessel-list-2018-final.csv", stringsAsFactors = F)
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
# 
# # 4) Cap/tier data (from US proposal) ---
# cap_tier_dat <- read_csv("./data/US_cap_tier_data.csv") %>%
#   mutate(iso3 = countrycode(country, "country.name", "iso3c"))
# cap_tier_dat$iso3[cap_tier_dat$country == "Eswatini"] <- "SWZ"
# cap_tier_dat$iso3[cap_tier_dat$country == "EU"] <- "EU"  
# 
# 
# # 5) FAO Marine Capture Production Data (by ISSCAAP group) ---
# landing_dat <- read_csv("./data/fao-landings-group-2000-2017.csv") %>%
#   group_by(year, iso3, isscaap_group) %>%
#   summarize(landings = sum(value, na.rm = T)) %>%
#   ungroup() %>%
#   complete(nesting(iso3, isscaap_group), year, fill = list(landings = 0)) %>%
#   group_by(year, iso3) %>%
#   mutate(tot_landings = sum(landings, na.rm = T)) %>%
#   ungroup() %>%
#   mutate(prop_landings = landings/tot_landings)
# 
# 
# # 6) Biological parameters for the model 
# bio_dat <- read.csv("./data/regional-model-parameters.csv")
# 
# # 7) Proposal settings
# proposal_settings <- read.csv("./data/wto-proposal-settings.csv", stringsAsFactors = F)
# default_settings <- proposal_settings %>% dplyr::filter(proposal == "Default")
# 
# ### -----------------------
# ### Color Palette ---------
# ### -----------------------
# 
# # Subsidy subtypes - includes option for "Total"
# goodColors <- rev(brewer.pal(3,"Blues"))
# names(goodColors) <- levels(profile_dat$subtype)[1:3]
# 
# badColors <- rev(brewer.pal(7, "Reds"))
# names(badColors) <- levels(profile_dat$subtype)[4:10]
# 
# ambigColors <- c("purple", "mediumorchid", "violet")
# names(ambigColors) <- levels(profile_dat$subtype)[11:13]
# 
# totColor <- "#3c8dbc"
# names(totColor) <- levels(profile_dat$subtype)[14] 
# 
# oecdColors <- rev(colorRampPalette(c("white", "black"), interpolate = "linear")(28))
# oecdColors <- oecdColors[1:25]
# names(oecdColors) <- levels(profile_dat$subtype)[15:39]
# 
# myColors <- c(goodColors, badColors, ambigColors, totColor, oecdColors)
# 
# ### -----------------------------------------------
# ### Widget Choices that Depend on Data ---------
# ### -----------------------------------------------
# 
# # Vector of country names: values are ISO3 codes, names are full english names
# country_choices <- unique(profile_dat$iso3)
# names(country_choices) <- unique(profile_dat$country)
# country_choices <- country_choices[order(names(country_choices))]
# 
# # Vector of subsidy types: values are Named vector of subsidy types
# subsidy_types_all <- setNames(as.character(subsidy_type_Sumaila$type), subsidy_type_Sumaila$subtype)
# 
# # Proposal names
# proposal_names <- proposal_settings %>%
#   mutate(display_name = case_when(proposal == "Default" ~ "None",
#                                   TRUE ~ paste0(title, " (", proposal, ")"))) %>%
#   dplyr::select(proposal, display_name)
# proposal_choices <- proposal_names$proposal
# names(proposal_choices) <- proposal_names$display_name
# 
# ### -----------------------------------------------
# ### Widget Choices that are Text Heavy ---------
# ### -----------------------------------------------
# 
# # IUU
# iuu_definitions <- c(
#   "Currently listed as having engaged in IUU fishing activities by an RFMO or international agreement" = "iuu1",
#   "Found to have engaged in IUU fishing activities by a coastal Member state" = "iuu2",
#   "Found to have engaged in IUU fishing activities by the flag Member state" = "iuu3",
#   "Found to have engaged in IUU fishing activities by the subsidizing Member state" = "iuu4"
# )
# 
# iuu_scope <- c(
#   "All Members" = "all",
#   "Select Members..." = "select"
# )
# 
# # All others
# oa_definitions <- c(
#   "Is considered to be overfished (B/Bmsy < 0.8) as determined by the most recent stock assessment in the RAM Legacy Stock Assessment database" = "OA1",
#   "Is considered to be overfished (B/Bmsy < 0.8) as determined by the data-limited assessment done by Costello et al. (2016)" = "OA2"
# )
# 
# overcap_definitions <- subsidy_types_all[4:10]
# 
# scope <- c(
#   "All Members" = "all",
#   "Only Member-flagged vessels fishing in areas beyond national jurisdiction (on the “high seas”)" = "HS",
#   "Only Member-flagged vessels fishing in the EEZs of other coastal states" = "DW",
#   "Only Member-flagged vessels fishing on the high seas or in the EEZs of other coastal states" = "OUT",
#   "Only vessels flagged to the 10 Member states responsible for providing the greatest amount of capacity-enhancing fisheries subsidies" = "SUB",
#   "Only vessels over a certain size" = "LENGTH",
#   "Only vessels fishing in disputed areas" = "disputed",
#   "Only vessels meeting at least two of the specified length, gross tonnage, and engine power characteristics" = "LTE",
#   "Select Members..." = "select"
# )
# 
# sdt_who <- c(
#   "LDCs only" = "ldc",
#   "Both developing countries and LDCs" = "developing",
#   "Small, vulnerable economies (SVEs) only" = "sve"
# )
# 
# sdt_what <- c(
#   #"Time delay allowed for implementation" = "time",
#   "All Member-flagged vessels are exempted" = "all",
#   "Member-flagged vessels fishing soley within their own EEZs are exempted" = "domestic",
#   "Member-flagged vessels fishing in areas beyond national jurisdiction (on the “high seas”) are exempted" = "HS"
# )
# 
# # Cap/tier
# tiering_options <- c(
#   "% of global marine capture production" = "capture",
#   "% of global seafood exports" = "exports",
#   "% of global fisheries subsidies" = "subs",
#   "development status" = "development"
# )
# 
# cap_set_methods <- c(
#   "i) an absolute amount" = "value",
#   "ii) % of existing subsides" = "percent_subs",
#   "iii) % of landed value" = "percent_revenue",
#   "iv) an amount multiplied by the number of fishers" = "fishers",
#   "v) the highest value of options ii-iv above" = "best"
# )