########################################################
#
# Create a lookup converting the effective dollar value of a subsidy based on its type
# Creation of this lookup table was based upon a suggestion made directly by Roger Martini to Chris Costello
# The OECD has done quite a bit of modeling looking at the relative effects of different types of subsidies on effort - This offers a way of creating an "effective" dollar of subsidies for better comparison across types. 
#
########################################################

### This is largely based upon the following report: Martini, R. and J. Innes (2018), “Relative Effects of Fisheries Support Policies”, OECD Food, Agriculture and Fisheries Papers, No. 115, OECD Publishing, Paris. http://dx.doi.org/10.1787/bd9b0dc3-en

# The effects of six different types of support policies are given relative to one another: Inputs, Outputs, Fuel, Income, Capital, Vessels

# For this model, we're interested in the relative effects of different subsidy types on effort.
oecd_relative_effects <- data_frame(
  category = c("inputs", "outputs", "fuel", "income", "capital", "vessels"),
  rel_effect_effort = c(1, 0.87, 0.84, 0.76, 0.57, 0.55)
)

### Sumaila matching subsidy types for each category
# Inputs: Fishing port construction and renovation programs (B2); Fishery development projects and support services (B4); Foreign access agreements (B6)
input_types <- "B2; B4; B6"
oecd_relative_effects$sumaila_subsidy_types[oecd_relative_effects$category == "inputs"] <- input_types

# Outputs: Price and marketing support, processing and storage infrastructure programs (B3)
output_types <- "B3"
oecd_relative_effects$sumaila_subsidy_types[oecd_relative_effects$category == "outputs"] <- output_types

# Fuel: Fuel subsidies (B7)
fuel_types <- "B7"
oecd_relative_effects$sumaila_subsidy_types[oecd_relative_effects$category == "fuel"] <- fuel_types

# Income: Non-fuel tax exemptions (B5); Fisher assistance programs (C1); Rural fishers’ community development programs (C3)
income_types <- "B5; C1; C3"
oecd_relative_effects$sumaila_subsidy_types[oecd_relative_effects$category == "income"] <- income_types

# Capital: Non-fuel tax exemptions (B5); Fisher assistance programs (C1); Rural fishers’ community development programs (C3)
capital_types <- "B5; C1; C3"
oecd_relative_effects$sumaila_subsidy_types[oecd_relative_effects$category == "capital"] <- capital_types

# Vessels: Boat construction, renewal and modernization programs (B1); Vessel buyback programs (C2)
vessel_types <- "B1; C2"
oecd_relative_effects$sumaila_subsidy_types[oecd_relative_effects$category == "vessels"] <- vessel_types

### Gather
oecd_relative_effects <- oecd_relative_effects %>%
  mutate(sumaila_subsidy_types = str_split(sumaila_subsidy_types, "; ")) %>%
  unnest(cols = c(sumaila_subsidy_types)) %>%
  group_by(sumaila_subsidy_types) %>%
  summarize(rel_effect_effort = median(rel_effect_effort)) %>%
  rename(type = sumaila_subsidy_types)

write_csv(oecd_relative_effects, paste0(here::here("data", "lookup-tables", "oecd_relative_effects_lookup_table.csv")))

