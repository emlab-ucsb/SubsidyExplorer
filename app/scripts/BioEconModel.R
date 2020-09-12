### -------------------------------------------------------------------
### SubsidyExplorer
### An interactive toolkit to explore the tradeoffs across proposed fisheries subsidies disciplines
### 
### Creator(s): Kat Millage and Vienna Saccomanno
### 
### This script contains the bioeconomic model 
### --------------------------------------------------------------------

BioEconModel <- function(fleet,
                         region = "global",
                         bio_param,
                         end_year = 2050,
                         biomass_scalar = 1,
                         return = "last"){

  ### Time ----------------------------
  year_range <- seq(2018, end_year, by = 1)
  stop_time <- length(year_range) 
  
  ### Fleets ----------------------------
  nf <- length(fleet$fleet)
  fleet_names <- fleet$fleet
  
  # Get affected/managed, affected/open access, unaffected/managed, and unaffected/open access positions
  aff_man <- which(fleet_names == "affected_managed")
  aff_oa <- which(fleet_names == "affected_oa")
  un_man <- which(fleet_names == "unaffected_managed")
  un_oa <- which(fleet_names == "unaffected_oa")

  ### Fixed biological parameters ----------------------------
  b0 <- bio_param$value[bio_param$parameter == "b0"]
  K <- bio_param$value[bio_param$parameter == "k"]
  r <- bio_param$value[bio_param$parameter == "g"]
  phi <- bio_param$value[bio_param$parameter == "phi"]
  
  ### Fixed harvest/fleet parameters ----------------------------
  #H0 <- bio_param$value[bio_param$parameter == "h0"]
  #E0 <- bio_param$value[bio_param$parameter == "e0"]
  beta <- bio_param$value[bio_param$parameter == "beta"]
  eta <- bio_param$value[bio_param$parameter == "eta"]
  omega <- bio_param$value[bio_param$parameter == "omega"]

  ### Fixed demand parameters ----------------------------
  p0 <- bio_param$value[bio_param$parameter == "p0"]
  delta <- bio_param$value[bio_param$parameter == "delta"]
  epsilon <- bio_param$value[bio_param$parameter == "epsilon"]
  
  ### ----------------------------------------------------------------------------------  
  ### Step 1: BAU ----------------------------------------------------------------------
  ### ----------------------------------------------------------------------------------  

  ## Set up storage bins to track important things for our BAU run
  biomass_bau <- matrix(0, nrow = stop_time, ncol = 1)
  rownames(biomass_bau) <- year_range

  price_bau <- matrix(0, nrow = stop_time, ncol = 1)
  rownames(price_bau) <- year_range
  
  catches_fleet_bau <- matrix(0, nrow = stop_time, ncol = length(fleet_names))
  rownames(catches_fleet_bau) <- year_range
  colnames(catches_fleet_bau) <- fleet_names
  
  catches_total_bau <- matrix(0, nrow = stop_time, ncol = 1)
  rownames(catches_total_bau) <- year_range
  
  effort_fleet_bau <- matrix(0, nrow = stop_time, ncol = length(fleet_names))
  rownames(effort_fleet_bau) <- year_range
  colnames(effort_fleet_bau) <- fleet_names
  
  effort_total_bau <- matrix(0, nrow = stop_time, ncol = 1)
  rownames(effort_total_bau) <- year_range
  
  revenue_fleet_bau <- matrix(0, nrow = stop_time, ncol = length(fleet_names))
  rownames(revenue_fleet_bau) <- year_range
  colnames(revenue_fleet_bau) <- fleet_names
  
  revenue_total_bau <- matrix(0, nrow = stop_time, ncol = 1)
  rownames(revenue_total_bau) <- year_range
  
  costs_fleet_bau <- matrix(0, nrow = stop_time, ncol = length(fleet_names))
  rownames(costs_fleet_bau) <- year_range
  colnames(costs_fleet_bau) <- fleet_names
  
  subsidies_fleet_bau <- matrix(0, nrow = stop_time, ncol = length(fleet_names))
  rownames(subsidies_fleet_bau) <- year_range
  colnames(subsidies_fleet_bau) <- fleet_names
  
  profits_fleet_bau <- matrix(0, nrow = stop_time, ncol = length(fleet_names))
  rownames(profits_fleet_bau) <- year_range
  colnames(profits_fleet_bau) <- fleet_names
  
  profits_total_bau <- matrix(0, nrow = stop_time, ncol = 1)
  rownames(profits_total_bau) <- year_range

  ## Fill in initial values
  biomass_bau[1] <- b0
  price_bau[1] <- p0
  catches_fleet_bau[1, ] <- fleet$catch
  catches_total_bau[1] <- sum(catches_fleet_bau[1, ])
  effort_fleet_bau[1, ] <- fleet$fishing_KWh
  effort_total_bau[1] <- sum(effort_fleet_bau[1, ])
  revenue_fleet_bau[1, ] <- fleet$catch * p0
  revenue_total_bau[1] <- sum(revenue_fleet_bau[1, ])
  subsidies_fleet_bau[1, ] <- fleet$bad_subs / fleet$fishing_KWh
  subsidies_fleet_bau[1,][is.nan(subsidies_fleet_bau[1,])] <- 0

  ## Solve for cost coefficients - same used throughout
  cost_coeff <- (revenue_fleet_bau[1,]+(subsidies_fleet_bau[1,]*effort_fleet_bau[1,]))/(effort_fleet_bau[1,]^beta)
  cost_coeff[is.nan(cost_coeff)] <- 0

  # Calculate initial costs and profits
  costs_fleet_bau[1,] <- cost_coeff*effort_fleet_bau[1,]^beta
  profits_fleet_bau[1,] <- round(revenue_fleet_bau[1,] - costs_fleet_bau[1,] + (subsidies_fleet_bau[1,]*effort_fleet_bau[1,]))
  profits_total_bau[1] <- sum(profits_fleet_bau[1,])

  ## Solve for catchability
  q <- catches_fleet_bau[1,]/(biomass_bau[1]*effort_fleet_bau[1,])
  q[is.nan(q)] <- 0

# Solve for delta
#delta <- catches_total_bau[1]/(price_bau[1]^epsilon)

## Time loopy loopy loop

t <- 1 # start off time loop

while (t < stop_time) {
  
  # Biomass 
  biomass_bau[t+1] <- max(0, (biomass_bau[t] + ((phi + 1)/phi)*r*biomass_bau[t]*(1-((biomass_bau[t]/K))^phi) - sum(catches_fleet_bau[t,])))

  # Effort 
  
  # Fleets with capped effort (managed)
  all_man <- c(aff_man, un_man)
    
  #effort_fleet_bau[t+1,all_man] <- effort_fleet_bau[t,all_man]
  effort_fleet_bau[t+1,all_man] <- pmax(0, pmin(effort_fleet_bau[1,all_man], (omega*profits_fleet_bau[t,all_man] + effort_fleet_bau[t,all_man])))
    
  # Open access fleets
  all_oa <- c(aff_oa, un_oa)
  
  effort_fleet_bau[t+1,all_oa] <- pmax(0, (eta*profits_fleet_bau[t,all_oa] + effort_fleet_bau[t,all_oa]))
    
  # Total effort
  effort_total_bau[t+1] <- sum(effort_fleet_bau[t+1,])
  
  # Catches
  catches_fleet_bau[t+1,] <- pmax(0, (q*biomass_bau[t]*effort_fleet_bau[t+1,]))

  catches_total_bau[t+1] <- sum(catches_fleet_bau[t+1,])
  
  # Price
  price_bau[t+1] = max(0, ((1/delta)^(1/epsilon))*(sum(catches_fleet_bau[t+1,])^(1/epsilon)))
  
  # price_bau[t+1,2] = max(0, ((1/delta)^(1/epsilon))*(sum(catches_fleet_bau[t+1,nf])^(1/epsilon)))
  # price_bau[t+1,][is.infinite(price_bau[t+1,])] <- 0
  
  # Profits 
  revenue_fleet_bau[t+1,] <- price_bau[t+1]*catches_fleet_bau[t+1,]
  
  revenue_total_bau[t+1] <- sum(revenue_fleet_bau[t+1,])

  costs_fleet_bau[t+1,] <- cost_coeff*effort_fleet_bau[t+1,]^beta
  
  subsidies_fleet_bau[t+1,] <- subsidies_fleet_bau[t,] 
  
  profits_fleet_bau[t+1,] <- revenue_fleet_bau[t+1,] - costs_fleet_bau[t+1,] + (subsidies_fleet_bau[t+1,]*effort_fleet_bau[t+1,])
  
  profits_total_bau[t+1] <- sum(profits_fleet_bau[t+1,])

  # Advance time
  t <- t+1
  
} # Close time loop

bau_out <- list(biomass = biomass_bau,
                price = price_bau,
                catches_fleet = catches_fleet_bau,
                catches_total = catches_total_bau,
                effort_fleet = effort_fleet_bau,
                effort_total = effort_total_bau,
                revenue_fleet = revenue_fleet_bau,
                revenue_total = revenue_total_bau,
                costs_fleet = costs_fleet_bau,
                subsidies_fleet = subsidies_fleet_bau,
                profits_fleet = profits_fleet_bau,
                profits_total = profits_total_bau)

### Subsidy Reform --------------------------------------------------------

## Set up storage bins to track important things for our subsidy removal run
biomass <- matrix(0, nrow = stop_time, ncol = 1)
rownames(biomass) <- year_range

price <- matrix(0, nrow = stop_time, ncol = 1)
rownames(price) <- year_range

catches_fleet <- matrix(0, nrow = stop_time, ncol = length(fleet_names))
rownames(catches_fleet) <- year_range
colnames(catches_fleet) <- fleet_names

catches_total <- matrix(0, nrow = stop_time, ncol = 1)
rownames(catches_total) <- year_range

effort_fleet <- matrix(0, nrow = stop_time, ncol = length(fleet_names))
rownames(effort_fleet) <- year_range
colnames(effort_fleet) <- fleet_names

effort_total <- matrix(0, nrow = stop_time, ncol = 1)
rownames(effort_total) <- year_range

revenue_fleet <- matrix(0, nrow = stop_time, ncol = length(fleet_names))
rownames(revenue_fleet) <- year_range
colnames(revenue_fleet) <- fleet_names

revenue_total <- matrix(0, nrow = stop_time, ncol = 1)
rownames(revenue_total) <- year_range

costs_fleet <- matrix(0, nrow = stop_time, ncol = length(fleet_names))
rownames(costs_fleet) <- year_range
colnames(costs_fleet) <- fleet_names

subsidies_fleet <- matrix(0, nrow = stop_time, ncol = length(fleet_names))
rownames(subsidies_fleet) <- year_range
colnames(subsidies_fleet) <- fleet_names

profits_fleet <- matrix(0, nrow = stop_time, ncol = length(fleet_names))
rownames(profits_fleet) <- year_range
colnames(profits_fleet) <- fleet_names

profits_total <- matrix(0, nrow = stop_time, ncol = 1)
rownames(profits_total) <- year_range

## Fill in initial values
biomass[1] <- biomass_bau[1]
price[1] <- price_bau[1]
catches_fleet[1,] <- catches_fleet_bau[1,]
catches_total[1] <- sum(catches_fleet[1,])
effort_fleet[1,] <- effort_fleet_bau[1,]
effort_total[1] <- sum(effort_fleet[1,])
revenue_fleet[1,] <- catches_fleet[1,]*price[1]
revenue_total[1] <- sum(revenue_total[1])
subsidies_fleet[1,] <- subsidies_fleet_bau[1,]
subsidies_fleet[1,][is.nan(subsidies_fleet[1,])] <- 0

# Calculate initial costs and profits
costs_fleet[1,] <- cost_coeff*effort_fleet[1,]^beta
profits_fleet[1,] <- round(revenue_fleet[1,] - costs_fleet[1,] + (subsidies_fleet[1,]*effort_fleet[1,]), 0)
profits_total[1] <- sum(profits_fleet[1,])

## Time loopy loopy loop

t <- 1 # start off time loop

while (t < stop_time) {
  
  # Biomass 
  biomass[t+1] <- max(0, (biomass[t] + ((phi + 1)/phi)*r*biomass[t]*(1-((biomass[t]/K))^phi) - sum(catches_fleet[t,])))
  
  # Effort
  # Managed fleets 
  all_man <- c(aff_man, un_man)
    
  effort_fleet[t+1,all_man] <- pmax(0, pmin(effort_fleet[1,all_man], (omega*profits_fleet[t,all_man] + effort_fleet[t,all_man])))
    
  # Fleets with unlimited effort (open effort)
  all_oa <- c(aff_oa, un_oa)
    
  effort_fleet[t+1,all_oa] <- pmax(0, (eta*profits_fleet[t,all_oa] + effort_fleet[t,all_oa]))
    
  effort_total[t+1] <- sum(effort_fleet[t+1,])
  
  # Catches
  catches_fleet[t+1,] <- pmax(0, (q*biomass[t]*effort_fleet[t+1,]))

  catches_total[t+1] <- sum(catches_fleet[t+1,])
  
  # Price
  price[t+1] = max(0, ((1/delta)^(1/epsilon))*(sum(catches_fleet[t+1,])^(1/epsilon)))
  
  ### Revenue 
  revenue_fleet[t+1,] <- price[t+1]*catches_fleet[t+1,]
  
  revenue_total[t+1] <- sum(revenue_fleet[t+1,])
  
  ### Costs
  costs_fleet[t+1,] <- cost_coeff*effort_fleet[t+1,]^beta

  ### Subsidies 
  all_aff <- c(aff_man, aff_oa)
    
    subsidies_fleet[t+1,all_aff] <- pmax(0, (fleet$bad_subs[all_aff] - fleet$removed_subs[all_aff]))/fleet$fishing_KWh[all_aff] # Account for new affected fleet subsidies
  
  all_un <- c(un_man, un_oa)
    
  subsidies_fleet[t+1,all_un] <- subsidies_fleet[t,all_un]
  
  subsidies_fleet[t+1,][is.nan(subsidies_fleet[t+1,])] <- 0
  
  ### Profits
  profits_fleet[t+1,] <- (revenue_fleet[t+1,] - costs_fleet[t+1,]) + (subsidies_fleet[t+1,]*effort_fleet[t+1,])
  profits_total[t+1] <- sum(profits_fleet[t+1,])
  
  # Next time step
  t <- t+1
  
} # Close time loop

reform_out <- list(biomass = biomass,
                   price = price,
                   catches_fleet = catches_fleet,
                   catches_total = catches_total,
                   effort_fleet = effort_fleet,
                   effort_total = effort_total,
                   revenue_fleet = revenue_fleet,
                   revenue_total = revenue_total,
                   costs_fleet = costs_fleet,
                   subsidies_fleet = subsidies_fleet,
                   profits_fleet = profits_fleet,
                   profits_total = profits_total)


### Difference --------------------------------------------------------

ExtractData <- function(x,
                        name,
                        nfleet){
  
      if(grepl("_fleet", name)){
    
        df <- as.data.frame(x) %>%
          mutate(Year = as.numeric(rownames(x)),
                Variable = name) %>%
          gather(Fleet, value, 1:nf)
    
      } else if(grepl("_total", name) | name == "biomass" | name == "price"){
        df <- as.data.frame(x) %>%
          mutate(Year = as.numeric(rownames(x)),
                 Variable = name,
                 Fleet = "Total") %>%
          rename(value = V1)
        
      } else{
       df <- as.data.frame(x) %>%
          mutate(Year = as.numeric(rownames(x)),
                Variable = name) %>%
         gather(Fleet, value, 1:2)
      }
  
  return(df)
  
}

bau_df <- map2_df(bau_out, names(bau_out), ExtractData, nfleet = length(fleet_names)) %>%
  rename(BAU = value) %>%
  mutate(Region = region)

reform_df <- map2_df(reform_out, names(reform_out), ExtractData, nfleet = length(fleet_names)) %>%
  rename(Reform = value) %>%
  mutate(Region = region)

diff_df <- bau_df %>%
  left_join(reform_df, by = c("Year", "Variable", "Fleet", "Region")) %>%
  dplyr::select(Year, Variable, Fleet, Region, BAU, Reform)

# Return either ending conditions or all 

if(return == "last") {
  
 out <- diff_df %>%
   dplyr::filter(Year == end_year)
 
 return(out)
 
}else{
  
  return(diff_df)
  
}

} # Close function

# diff_df <- bau_df %>%
#   left_join(reform_df, by = c("Year", "Variable", "Fleet")) %>%
#   mutate(Diff = (Reform - BAU)/abs(BAU)) %>%
#   dplyr::select(Year, Variable, Fleet, BAU, Reform, Diff) %>%
#   gather("Scenario", "Value", 4:6) %>%
#   mutate(Region = region)
