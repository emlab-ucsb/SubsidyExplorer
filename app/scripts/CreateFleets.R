### -------------------------------------------------------------------
### SubsidyExplorer
### An interactive toolkit to explore the tradeoffs across proposed fisheries subsidies disciplines
### 
### Creator(s): Kat Millage and Vienna Saccomanno
### 
### This script creates the fleets to be used in the bioeconomic model based on the selected subsidy reform proposal
### --------------------------------------------------------------------

CreateFleets <- function(vessel_list,
                         iuu,
                         oa,
                         overcap,
                         cap_tier,
                         subsidy_types_all,
                         managed_threshold = 0.66,
                         cap_tier_lookup,
                         country_lookup){
  
  ### ---------------------------------
  ### SETUP ---------------------------
  ### ---------------------------------
  
  ### Since we don't have this as a selectable option - domestic vessels are defined as those that spend less than 1% of their time fishing outside of their own EEZ
  domestic_vessel_cutoff <- 0.01
  
  # All subsidy types
  good_sub_types <- subsidy_types_all[1:3]
  bad_sub_types <- subsidy_types_all[4:10]
  ugly_sub_types <- subsidy_types_all[11:13]
  
  removed_sub_types <- c(bad_sub_types, ugly_sub_types)
  
  # Subsidy types removed if an IUU discipline is triggered
  iuu_subtypes_removed <- c(bad_sub_types, ugly_sub_types)
  
  # Subsidy types removed if an Overfished discipline is triggered
  oa_subtypes_removed <- c(bad_sub_types, ugly_sub_types)
  
  # WTO members and observers
  wto_members_and_observers <- country_lookup$iso3[country_lookup$is_WTO & !country_lookup$is_overseas_territory]
  names(wto_members_and_observers) <- country_lookup$display_name[country_lookup$iso3 %in% wto_members_and_observers] 
  
  # WTO territories 
  wto_territories <- country_lookup$iso3[(country_lookup$sovereign_iso3 != country_lookup$iso3) & (country_lookup$sovereign_iso3 %in% wto_members_and_observers)]
  
  # EU states
  eu_countries <- country_lookup$iso3[country_lookup$is_EU & country_lookup$iso3 != "EU"]
  names(eu_countries) <- country_lookup$display_name[country_lookup$iso3 %in% eu_countries]
  
  # List of all EU overseas territories - vessels flagged here should be considered WTO Members
  eu_territories <- country_lookup$iso3[country_lookup$is_overseas_territory & country_lookup$sovereign_iso3 %in% eu_countries]
  names(eu_territories) <- country_lookup$display_name[country_lookup$iso3 %in% eu_territories]
  
  # All WTO territories (with their sovereign states) -  vessels flagged here should be considered WTO Members
  territories <- country_lookup %>%
    dplyr::filter(is_overseas_territory & is_WTO) %>%
    distinct(iso3, sovereign_iso3)
  
  all_territories <- unique(c(wto_territories, eu_territories))
  
  # SVE countries
  sves <- c("ATG", "BRB", "BLZ", "BOL", "CUB", "DMA", "DOM", "SLV", "ECU", "FJI", "GRD", "GTM", "HND", "JAM", "MRT", "NIC", "PAN", "PNG", "KNA", "LCA", "VCT", "WSM", "SYC", "LKA", "TON", "TTO", "BHS")
  
  # EU countries
  eu_states_t <- paste0(eu_countries, "-T")
  
  # Exclusion of certain states for various rankings (because state is included in total for another entity)
  subs_ranking_exclude <- c(eu_countries) # Exclude individual EU states in "top subsidizer ranking" 
  capture_ranking_exclude <- c(eu_countries, eu_states_t, "AUS", "MAR", "NOR", "NZL", "USA", all_territories)
  development_ranking_exclude <- c("AUS-T", eu_countries, eu_states_t, "NZL-T", "NOR-T", "MAR-T", "USA-T", all_territories)
  cap_exclude <- c(eu_countries)
  
  ### ---------------------------------
  ### ---------------------------------
  ### AFFECTED ------------------------
  ### ---------------------------------
  ### ---------------------------------

  ### Remove vessels with no bad subsidies or that do not below to a WTO Member or Observer state (they can't be affected) 
  vessel_subset <- vessel_list %>%
    dplyr::filter((bad_subs > 0 | ugly_subs > 0) & is_WTO) 
  
  ### Create empty container to track existing/removed subsidies by subtype by affected vessel and region
  vessel_tracking_df <- tibble(ssvid = numeric(0),
                               region = character(0),
                               fao_region = numeric(0),
                               eez_id = numeric(0),
                               is_territorial = logical(0),
                               A1_subs_removed = numeric(0),
                               A2_subs_removed = numeric(0),
                               A3_subs_removed = numeric(0),
                               B1_subs_removed = numeric(0),
                               B2_subs_removed = numeric(0),
                               B3_subs_removed = numeric(0),
                               B4_subs_removed = numeric(0),
                               B5_subs_removed = numeric(0),
                               B6_subs_removed = numeric(0),
                               B7_subs_removed = numeric(0),
                               C1_subs_removed = numeric(0),
                               C2_subs_removed = numeric(0),
                               C3_subs_removed = numeric(0),
                               subs_removed = numeric(0))
  
  ### Create empty container to track activity that should be excluded b/c of S&DT
  vessel_sdt_df <- tibble(ssvid = numeric(0),
                          region = character(0),
                          fao_region = numeric(0),
                          eez_id = numeric(0),
                          is_territorial = logical(0))
  
  ### Section #1 ---------------------------------------------------------------
  ### Illegal, unreported, and unregulated fishing -----------------------------
  ### --------------------------------------------------------------------------
  
  # Empnty df for vessels triggering the IUU prohibitions
  iuu_vessel_subset <- vessel_tracking_df
  
  # Apply definitions
  if("IUU1" %in% iuu$definitions){
    
    # "Currently listed as having engaged in IUU fishing activities by an RFMO or international agreement" = "iuu1"
    iuu1_vessels <- vessel_subset %>%
        dplyr::filter(iuu1) %>%
        group_by(ssvid, region, fao_region, eez_id, is_territorial) %>%
        summarize_at(paste0(iuu_subtypes_removed, "_subs"), list(removed = sum), na.rm = T) %>%
        ungroup() %>%
        mutate(subs_removed = rowSums(select(., one_of(paste0(iuu_subtypes_removed, "_subs_removed")))))
    
    # Join to IUU subset
     iuu_vessel_subset <- iuu_vessel_subset %>%
      bind_rows(iuu1_vessels)
     iuu_vessel_subset[is.na(iuu_vessel_subset)] <- 0
  
  }

  if("IUU2" %in% iuu$definitions | 
     "IUU3" %in% iuu$definitions | 
     "IUU4" %in% iuu$definitions |
     "IUU5" %in% iuu$definitions | 
     "IUU6" %in% iuu$definitions){

    # "Currently listed as having engaged in IUU fishing activities by the coastal state" = "iuu2"
    # "Currently listed as having engaged in IUU fishing activities by the flag state" = "iuu3"
    # "Currently listed as having engaged in IUU fishing activities by the subsidizing Member state" = "iuu4"

    if(iuu$assumption == "NO"){
      
      # No matching vessels
      iuu_vessel_subset <- iuu_vessel_subset %>%
        bind_rows(vessel_tracking_df)
      
    }else if(iuu$assumption == "YES" & !is.na(iuu$percent)){
      
      # Assumed percentage of fishing that is IUU
      iuu_percent <- iuu$percent/100
      f <- function(value, mult) value*mult
      
      # Identify vessels matching definition
      iuu2_vessels <- vessel_subset %>%
        group_by(ssvid, region, fao_region, eez_id, is_territorial) %>%
        summarize_at(paste0(iuu_subtypes_removed, "_subs"), list(removed = f), mult = iuu_percent) %>%
        ungroup() %>%
        mutate(subs_removed = rowSums(select(., one_of(paste0(iuu_subtypes_removed, "_subs_removed"))))) 
      
      # Join to IUU subset
      iuu_vessel_subset <- iuu_vessel_subset %>%
        bind_rows(iuu2_vessels)
      iuu_vessel_subset[is.na(iuu_vessel_subset)] <- 0
      
    }
  }

  # Combine IUU subsets
  iuu_vessels <- iuu_vessel_subset %>%
    group_by(ssvid, region, fao_region, eez_id, is_territorial) %>%
    summarize_all(max, na.rm = T) %>%
    ungroup()
  
  ### IUU scope ----------

  # There must be at least one affected vessel in order to define scope
  if(nrow(iuu_vessels) >= 1){
    
    ### Vessel list for scope (need additional characteristics) - WHY DOES THIS GIVE ME MORE ENTRIES THAN I STARTED WITH - DUPLICATING SOMEWHERE
    iuu_vessels_scope <- iuu_vessels %>%
      left_join(vessel_subset, by = c("ssvid", "region", "fao_region", "eez_id", "is_territorial"))
    
    # All affected vessels are within scope
    if(iuu$scope == "ALL"){
      
      iuu_vessels_scope <- iuu_vessels_scope
      

    # Only those with selected characteristics
    }else if(iuu$scope == "SELECT"){
      
      if("MANUAL" %in% iuu$scope_select){
        
        # Select only certain member countries 
        countries <- iuu$scope_manual
        
        # Deal with EU
        if("EU" %in% countries){
          
          # Filter for selected Members
          iuu_vessels_scope <- iuu_vessels_scope %>%
            dplyr::filter(flag_iso3 %in% countries | flag_iso3 %in% eu_countries | flag_iso3 %in% eu_territories)
          
        }else{
          
          # Filter for selected Members
          iuu_vessels_scope <- iuu_vessels_scope %>%
            dplyr::filter(flag_iso3 %in% countries)
          
        }
        
      }else if("EX_TER" %in% iuu$scope_select){
        
        # Filter for only vessels fishing outside of territorial waters
        iuu_vessels_scope <- iuu_vessels_scope %>%
          dplyr::filter(!is_territorial)
        
      }
    } # /SELECT
    
    # OA vessels in scope
    iuu_vessels_scope <- iuu_vessels_scope %>%
      dplyr::select(ssvid, region, fao_region, eez_id, is_territorial, contains("subs_removed"))
    
  }else{
    
    iuu_vessels_scope <- iuu_vessels
    
  }

  ### IUU S&DT ----------
  
  if(nrow(iuu_vessels_scope) >= 1 & iuu$allow_sdt == "YES"){
  
  ### Vessel list for S&DT
  iuu_vessels_sdt <- vessel_subset %>%
      right_join(iuu_vessels_scope, by = c("ssvid", "region", "fao_region", "eez_id", "is_territorial"))
  
  ### 1) LDC S&DT ---
  
  if(iuu$sdt_ldc == "YES"){
    
    iuu_vessels_sdt_ldc <- iuu_vessels_sdt %>%
      dplyr::filter(development_status == "LDC")
    
    if("ALL" %in% iuu$sdt_what_ldc){
      
      # If all Member-flagged vessels are exempt... This trumps all other options
      iuu_vessels_sdt_ldc <- iuu_vessels_sdt_ldc
      
    }else if("DOMESTIC" %in% iuu$sdt_what_ldc){
      
      # Exempt domestic fishing vessels (less than 1% of fishing effort)
      iuu_vessels_sdt_ldc <- iuu_vessels_sdt_ldc %>%
        dplyr::filter(prop_fishing_KWh_high_seas < domestic_vessel_cutoff) 
      
    }else if("TER" %in% iuu$sdt_what_ldc){
      
      # Exempt fishing within territorial waters
      iuu_vessels_sdt_ldc <- iuu_vessels_sdt_ldc %>%
        dplyr::filter(is_territorial)

    }else if("TIME" %in% iuu$sdt_what_ldc){
      
      #################
      # XXX: NEED TO DO 
      #################
      
    }
    
  }else{
    
    iuu_vessels_sdt_ldc <- vessel_sdt_df
    
  }
  
  ### 2) Developing country S&DT ---
  
  if(iuu$sdt_developing == "YES"){
    
    iuu_vessels_sdt_developing <- iuu_vessels_sdt %>%
      dplyr::filter(development_status == "Developing")
    
    if("ALL" %in% iuu$sdt_what_developing){
      
      # If all Member-flagged vessels are exempt... This trumps all other options
      iuu_vessels_sdt_developing <- iuu_vessels_sdt_developing
      
    }else if("DOMESTIC" %in% iuu$sdt_what_developing){
      
      # Exempt domestic fishing vessels (less than 1% of fishing effort)
      iuu_vessels_sdt_developing <- iuu_vessels_sdt_developing %>%
        dplyr::filter(prop_fishing_KWh_high_seas < domestic_vessel_cutoff) 
      
    }else if("TER" %in% iuu$sdt_what_developing){
      
      # Exempt fishing within territorial waters
      iuu_vessels_sdt_developing <- iuu_vessels_sdt_developing %>%
        dplyr::filter(is_territorial)
      
    }else if("TIME" %in% iuu$sdt_what_developing){
      
      #################
      # XXX: NEED TO DO 
      #################
      
    }
    
  }else{
    
    iuu_vessels_sdt_developing <- vessel_sdt_df
    
  }
  
  ### 3) SVE S&DT ---
  
  if(iuu$sdt_sve == "YES"){
    
    iuu_vessels_sdt_sve <- iuu_vessels_sdt %>%
      dplyr::filter(flag_iso3 %in% sves)
    
    if("ALL" %in% iuu$sdt_what_sve){
      
      # If all Member-flagged vessels are exempt... This trumps all other options
      iuu_vessels_sdt_sve <- iuu_vessels_sdt_sve
      
    }else if("DOMESTIC" %in% iuu$sdt_what_sve){
      
      # Exempt domestic fishing vessels (less than 1% of fishing effort)
      iuu_vessels_sdt_sve <- iuu_vessels_sdt_sve %>%
        dplyr::filter(prop_fishing_KWh_high_seas < domestic_vessel_cutoff) 
      
    }else if("TER" %in% iuu$sdt_what_sve){
      
      # Exempt fishing within territorial waters
      iuu_vessels_sdt_sve <- iuu_vessels_sdt_sve %>%
        dplyr::filter(is_territorial)
      
    }else if("TIME" %in% iuu$sdt_what_sve){
      
      #################
      # XXX: NEED TO DO 
      #################
      
    }
    
  }else{
    
    iuu_vessels_sdt_sve <- vessel_sdt_df
    
  }
  
  # Combine S&DT across all categories
  iuu_vessels_sdt <- iuu_vessels_sdt_ldc %>%
    bind_rows(iuu_vessels_sdt_developing) %>%
    bind_rows(iuu_vessels_sdt_sve)
  
  # List of activity to be excluded from affected because of s&dt
    iuu_vessels_sdt_exclude <- iuu_vessels_sdt %>%
      distinct(ssvid, region, fao_region, eez_id, is_territorial)
  
  }else{
  
    # List of activity ids to be excluded from affected because of s&dt
    iuu_vessels_sdt_exclude <- vessel_sdt_df
    
  }

  ### Output subsidy summary for all vessels triggering iuu prohibitions, within scope and excluding S&DT
  iuu_vessels_out <- iuu_vessels_scope %>%
    anti_join(iuu_vessels_sdt_exclude, by = c("ssvid", "region", "fao_region", "eez_id", "is_territorial"))
    
  
  ### Section #2 ---------------------------------------------------------------
  ### Fishing on overfished and unassessed stocks ------------------------------
  ### --------------------------------------------------------------------------
  
  # Empnty df for vessels triggering the OA prohibitions
  oa_vessel_subset <- vessel_tracking_df
  
  # Apply definitions
  if("OA1" %in% oa$definitions){
    
    # Is considered to be overfished (B/Bmsy < 1) as determined by RAM" = "OA1"
    
    # Identify vessels matching definition
    oa1_vessels <- vessel_subset %>%
      dplyr::filter(oa1_median) %>%
      group_by(ssvid, region, fao_region, eez_id, is_territorial) %>%
      summarize_at(paste0(oa_subtypes_removed, "_subs"), list(removed = sum), na.rm = T) %>%
      ungroup() %>%
      mutate(subs_removed = rowSums(select(., one_of(paste0(oa_subtypes_removed, "_subs_removed")))))
    
    # Join to OA subset
    oa_vessel_subset <- oa_vessel_subset %>%
      bind_rows(oa1_vessels)
    oa_vessel_subset[is.na(oa_vessel_subset)] <- 0
    
  }

  if("OA2" %in% oa$definitions){

    # Is considered to be overfished (B/Bmsy < 0.8) as determined by the most recent formal stock assessment" = "OA2"
    
    # Identify vessels matching definition
    oa2_vessels <- vessel_subset %>%
      dplyr::filter(oa2_median) %>%
      group_by(ssvid, region, fao_region, eez_id, is_territorial) %>%
      summarize_at(paste0(oa_subtypes_removed, "_subs"), list(removed = sum), na.rm = T) %>%
      ungroup() %>%
      mutate(subs_removed = rowSums(select(., one_of(paste0(oa_subtypes_removed, "_subs_removed")))))
    
    # Join to OA subset
    oa_vessel_subset <- oa_vessel_subset %>%
      bind_rows(oa2_vessels)
    oa_vessel_subset[is.na(oa_vessel_subset)] <- 0

  }
  
  # Apply definitions
  if("OA3" %in% oa$definitions){
    
    #Is considered to be overfished (B/Bmsy < 1 or similar metric) as determined by the most recent data-limited stock assessment" = "OA3"
    
    # Identify vessels matching definition
    oa3_vessels <- vessel_subset %>%
      dplyr::filter(oa3_median) %>%
      group_by(ssvid, region, fao_region, eez_id, is_territorial) %>%
      summarize_at(paste0(oa_subtypes_removed, "_subs"), list(removed = sum), na.rm = T) %>%
      ungroup() %>%
      mutate(subs_removed = rowSums(select(., one_of(paste0(oa_subtypes_removed, "_subs_removed")))))
    
    # Join to OA subset
    oa_vessel_subset <- oa_vessel_subset %>%
      bind_rows(oa3_vessels)
    oa_vessel_subset[is.na(oa_vessel_subset)] <- 0
    
  }
  
  # Apply definitions
  if("OA4" %in% oa$definitions){
    
    #Is considered to be overfished (B/Bmsy < 0.8 or similar metric) as determined by the most recent data-limited stock assessment" = "OA4"
    
    # Identify vessels matching definition
    oa4_vessels <- vessel_subset %>%
      dplyr::filter(oa4_median) %>%
      group_by(ssvid, region, fao_region, eez_id, is_territorial) %>%
      summarize_at(paste0(oa_subtypes_removed, "_subs"), list(removed = sum), na.rm = T) %>%
      ungroup() %>%
      mutate(subs_removed = rowSums(select(., one_of(paste0(oa_subtypes_removed, "_subs_removed")))))
    
    # Join to OA subset
    oa_vessel_subset <- oa_vessel_subset %>%
      bind_rows(oa4_vessels)
    oa_vessel_subset[is.na(oa_vessel_subset)] <- 0
    
  }
  
  # Combine OA subsets
  oa_vessels <- oa_vessel_subset %>%
    group_by(ssvid, region, fao_region, eez_id, is_territorial) %>%
    summarize_all(max, na.rm = T) %>%
    ungroup()
  
  ### O/A scope ----------

  # There must be at least one affected vessel in order to define scope
  if(nrow(oa_vessels) >= 1){
    
    ### Vessel list for scope (need additional characteristics)
    oa_vessels_scope <- oa_vessels %>%
      left_join(vessel_subset, by = c("ssvid", "region", "fao_region", "eez_id", "is_territorial"))
    
    # All affected vessels are within scope
    if(oa$scope == "ALL"){
      
      oa_vessels_scope <- oa_vessels_scope
      
    }else if(oa$scope == "SELECT"){
      
      # 1) Only select certain countries 
      if("MANUAL" %in% oa$scope_select){
        
        # Select only certain member countries 
        countries <- oa$scope_manual
        
        # Deal with EU
        if("EU" %in% countries){
          
          # Filter for selected Members
          oa_vessels_scope <- oa_vessels_scope %>%
            dplyr::filter(flag_iso3 %in% countries | flag_iso3 %in% eu_countries | flag_iso3 %in% eu_territories)
          
        }else{
          
          # Filter for selected Members
          oa_vessels_scope <- oa_vessels_scope %>%
            dplyr::filter(flag_iso3 %in% countries)
          
        }
        
      # 2) Only vessels fishing outside of their territorial waters  
      }else if("EX_TER" %in% oa$scope_select){
        
        # Filter for only vessels fishing outside of territorial waters
        oa_vessels_scope <- oa_vessels_scope %>%
          dplyr::filter(!is_territorial)
        
      # 3) Only those coming from the top XX worst subsidizers are within scope  
      }else if("SUB" %in% oa$scope_select){
        
        # Country ranking subsidies 
        country_ranking_subsidies <- cap_tier_lookup %>%
          dplyr::filter(variable == "bad_ugly_subs") %>%
          dplyr::filter(!(iso3 %in% subs_ranking_exclude)) %>%
          group_by(iso3) %>%
          summarize(subs = unique(`2018`)) %>%
          mutate(subs_rank = dense_rank(desc(subs))) %>%
          arrange(subs_rank)
        
        # Find top 10 subsidizing Member states
        top_countries_subs <- country_ranking_subsidies$iso3[country_ranking_subsidies$subs_rank <= 10]
        
        # Deal with EU
        if("EU" %in% top_countries_subs){
          
          # Filter for top 10 subsidizing Member states
          oa_vessels_scope <- oa_vessels_scope %>%
            dplyr::filter(flag_iso3 %in% eu_countries | flag_iso3 %in% eu_territories | flag_iso3 %in% top_countries_subs)
          
        }else{
          # Filter for top 10 subsidizing Member states
          oa_vessels_scope <- oa_vessels_scope %>%
            dplyr::filter(flag_iso3 %in% top_countries_subs)
        }
      
      # 4) Only those responsible for more than 1% of global capture production    
      }else if("CAPTURE" %in% oa$scope_select){
        
        capture_years <- paste0("", seq(2016, 2018, by = 1), "")
        
        # Country ranking capture 
        country_ranking_capture <- cap_tier_lookup %>%
          dplyr::filter(variable == "capture_production") %>%
          dplyr::filter(!(iso3 %in% capture_ranking_exclude)) %>%
          mutate(prod = rowSums(select(., one_of(c(capture_years))))) %>%
          mutate(percent_prod = prod/(sum(prod, na.rm = T))) %>%
          arrange(percent_prod)
        
        # Find states responsible for greater than 1% of global capture production
        top_countries_capture <- str_replace(country_ranking_capture$iso3[country_ranking_capture$percent_prod >= 0.01], "-T", "")
        
        # Deal with EU
        if("EU" %in% top_countries_capture){
          
          # Filter for top 10 subsidizing Member states
          oa_vessels_scope <- oa_vessels_scope %>%
            dplyr::filter(flag_iso3 %in% eu_countries | flag_iso3 %in% eu_territories | flag_iso3 %in% top_countries_capture)
          
        }else{
          # Filter for top 10 subsidizing Member states
          oa_vessels_scope <- oa_vessels_scope %>%
            dplyr::filter(flag_iso3 %in% top_countries_capture)
        }
        
      # 5) Only those vessels fishing on the high seas    
      }else if("HS" %in% oa$scope_select & !is.na(oa$hs_cutoff)){
        
        # Filter for high seas vessels only
        oa_vessels_scope <- oa_vessels_scope %>%
          dplyr::filter(prop_fishing_KWh_high_seas >= (oa$hs_cutoff/100))
        
      # 6) Distant water fishing  
      }else if("DW" %in% oa$scope_select){
        
        # Filter for distant water fishing only
        oa_vessels_scope <- oa_vessels_scope %>%
          dplyr::filter(distant_water)
       
      # 6) Distant water fishing OR HIGH SEAS    
      }else if("OUT" %in% oa$scope_select & !is.na(oa$hs_cutoff)){
        
        # Filter for high seas OR distant water fishing only
        oa_vessels_scope <- oa_vessels_scope %>%
          dplyr::filter(distant_water | prop_fishing_KWh_high_seas >= (oa$hs_cutoff/100))
      
      # 7) Disputed areas  
      }else if("DISPUTE" %in% oa$scope_select){
        
        # Filter for disputed waters
        oa_vessels_scope <- oa_vessels_scope %>%
          dplyr::filter(is_disputed)
        
      # 8) Length   
      }else if("LENGTH" %in% oa$scope_select & !is.na(oa$length_cutoff)){
        
        # Filter by vessel length
        oa_vessels_scope <- oa_vessels_scope %>%
          dplyr::filter(length_m >= oa$length_cutoff)
      
      # 9) Tonnage    
      }else if("TONNAGE" %in% oa$scope_select & !is.na(oa$tonnage_cutoff)){
        
        # Filter by vessel tonnage
        oa_vessels_scope <- oa_vessels_scope %>%
          dplyr::filter(tonnage_gt >= oa$tonnage_cutoff)
      
      # 10) Engine power    
      }else if("ENGINE" %in% oa$scope_select & !is.na(oa$engine_cutoff)){
        
        # Filter by vessel engine power
        oa_vessels_scope <- oa_vessels_scope %>%
          dplyr::filter(engine_power_kw >= oa$engine_cutoff)
        
      }
      
    } # /SELECT
    
    # OA vessels in scope
    oa_vessels_scope <- oa_vessels_scope %>%
      dplyr::select(ssvid, region, fao_region, eez_id, is_territorial, contains("subs_removed"))
    
  }else{
    
    oa_vessels_scope <- oa_vessels
    
  }
  
  ### O/A - SDT -----
  
  if(nrow(oa_vessels_scope) >= 1 & oa$allow_sdt == "YES"){
    
    ### Vessel list for S&DT
    oa_vessels_sdt <- vessel_subset %>%
      right_join(oa_vessels_scope, by = c("ssvid", "region", "fao_region", "eez_id", "is_territorial"))
    
    ### 1) LDC S&DT ---
    
    if(oa$sdt_ldc == "YES"){
      
      oa_vessels_sdt_ldc <- oa_vessels_sdt %>%
        dplyr::filter(development_status == "LDC")
      
      if("ALL" %in% oa$sdt_what_ldc){
        
        # If all Member-flagged vessels are exempt... This trumps all other options
        oa_vessels_sdt_ldc <- oa_vessels_sdt_ldc
        
      }else if("DOMESTIC" %in% oa$sdt_what_ldc){
        
        # Exempt domestic fishing vessels (less than 1% of fishing effort)
        oa_vessels_sdt_ldc <- oa_vessels_sdt_ldc %>%
          dplyr::filter(prop_fishing_KWh_high_seas < domestic_vessel_cutoff) 
        
      }else if("TER" %in% oa$sdt_what_ldc){
        
        # Exempt domestic fishing vessels (less than 1% of fishing effort)
        oa_vessels_sdt_ldc <- oa_vessels_sdt_ldc %>%
          dplyr::filter(is_territorial) 
        
      }else if("HS" %in% oa$sdt_what_ldc){
        
        # Exempt vessels fishing on the high seas
        oa_vessels_sdt_ldc <- oa_vessels_sdt_ldc %>%
          dplyr::filter(prop_fishing_KWh_high_seas >= (oa$sdt_hs_cutoff_ldc/100))
        
      }else if("TIME" %in% oa$sdt_what_ldc){
        
        ####################
        ### XXX: NEED TO DO 
        ####################
        
      }
      
    }else{
      
      oa_vessels_sdt_ldc <- vessel_sdt_df
      
    }
    
    ### 2) Developing country S&DT ---
    
    if(oa$sdt_developing == "YES"){
      
      oa_vessels_sdt_developing <- oa_vessels_sdt %>%
        dplyr::filter(development_status == "Developing")
      
      if("ALL" %in% oa$sdt_what_developing){
        
        # If all Member-flagged vessels are exempt... This trumps all other options
        oa_vessels_sdt_developing <- oa_vessels_sdt_developing
        
      }else if("DOMESTIC" %in% oa$sdt_what_developing){
        
        # Exempt domestic fishing vessels (less than 1% of fishing effort on HS)
        oa_vessels_sdt_developing <- oa_vessels_sdt_developing %>%
          dplyr::filter(prop_fishing_KWh_high_seas < domestic_vessel_cutoff) 
        
      }else if("TER" %in% oa$sdt_what_developing){
        
        # Exempt vessels fishing in territorial waters
        oa_vessels_sdt_developing <- oa_vessels_sdt_developing %>%
          dplyr::filter(is_territorial) 
        
      }else if("HS" %in% oa$sdt_what_developing){
        
        # Exempt vessels fishing on the high seas
        oa_vessels_sdt_developing <- oa_vessels_sdt_developing %>%
          dplyr::filter(prop_fishing_KWh_high_seas >= (oa$sdt_hs_cutoff_developing/100))
        
      }else if("TIME" %in% oa$sdt_what_developing){
        
        ####################
        ### XXX: NEED TO DO 
        ####################
        
      }
      
    }else{
      
      oa_vessels_sdt_developing <- vessel_sdt_df
      
    }
    
    ### 3) SVE S&DT ---
    
    if(oa$sdt_sve == "YES"){
      
      oa_vessels_sdt_sve <- oa_vessels_sdt %>%
        dplyr::filter(flag_iso3 %in% sves)
      
      if("ALL" %in% oa$sdt_what_sve){
        
        # If all Member-flagged vessels are exempt... This trumps all other options
        oa_vessels_sdt_sve <- oa_vessels_sdt_sve
        
      }else if("DOMESTIC" %in% oa$sdt_what_sve){
        
        # Exempt domestic fishing vessels (less than 1% of fishing effort on HS)
        oa_vessels_sdt_sve <- oa_vessels_sdt_sve %>%
          dplyr::filter(prop_fishing_KWh_high_seas < domestic_vessel_cutoff) 
        
      }else if("TER" %in% oa$sdt_what_sve){
        
        # Exempt vessels fishing in territorial waters
        oa_vessels_sdt_sve <- oa_vessels_sdt_sve %>%
          dplyr::filter(is_territorial) 
        
      }else if("HS" %in% oa$sdt_what_sve){
        
        # Exempt vessels fishing on the high seas
        oa_vessels_sdt_sve <- oa_vessels_sdt_sve %>%
          dplyr::filter(prop_fishing_KWh_high_seas >= (oa$sdt_hs_cutoff_sve/100))
        
      }else if("TIME" %in% oa$sdt_what_sve){
        
        ####################
        ### XXX: NEED TO DO 
        ####################
        
      }
      
    }else{
      
      oa_vessels_sdt_sve <- vessel_sdt_df
      
    }
    
    # Combine S&DT across all categories
    oa_vessels_sdt <- oa_vessels_sdt_ldc %>%
      bind_rows(oa_vessels_sdt_developing) %>%
      bind_rows(oa_vessels_sdt_sve)
    
    # List of vessel ids to be excluded from affected because of s&dt
    oa_vessels_sdt_exclude <- oa_vessels_sdt %>%
      distinct(ssvid, region, fao_region, eez_id, is_territorial)

  }else{
    
    # List of vessel ids to be excluded from affected because of s&dt
    oa_vessels_sdt_exclude <- vessel_sdt_df
    
  }
  
  ### Output subsidy summary for all vessels triggering iuu prohibitions, within scope and excluding S&DT
  oa_vessels_out <- oa_vessels_scope %>%
    anti_join(oa_vessels_sdt_exclude, by = c("ssvid", "region", "fao_region", "eez_id", "is_territorial"))
  
  
  ### Section #3 ---------------------------------------------------------------
  ### Subsidies contributing to overcapacity and overfishing -------------------
  ### --------------------------------------------------------------------------
  
  if(length(overcap$definitions) == 0 | any(overcap$definitions == "") | any(is.null(overcap$definitions))){
    
    # Shortcut output if no disciplines are selected from this category
    overcap_vessel_subset <- vessel_tracking_df
    
  }else if(length(overcap$definitions) == 1){
    
    # It does some weird column naming if only one subtype is selected so we have to do the calculation differently here
    overcap_subtypes_removed <- overcap$definitions
  
    oc_vessels <- vessel_subset %>%
      group_by(ssvid, region, fao_region, eez_id, is_territorial) %>%
      summarize_at(paste0(overcap_subtypes_removed, "_subs"), list(removed = sum), na.rm = T) %>%
      ungroup() %>%
      mutate(subs_removed = removed) %>%
      dplyr::filter(subs_removed > 0)
    colnames(oc_vessels)[6] <- paste0(overcap_subtypes_removed, "_subs_removed")
    
    # Join to overcap subset
    overcap_vessel_subset <- vessel_tracking_df %>%
      bind_rows(oc_vessels)
    overcap_vessel_subset[is.na(overcap_vessel_subset)] <- 0
    
  }else{
    
    # B1: Boat construction and modernization
    # B2: Fisheries development projects
    # B3: Port and harbor development
    # B4: Marketing and storage infrastructure
    # B5: Non-fuel tax exemptions
    # B6: Fishing access agreements
    # B7: Fuel subsidies
    overcap_subtypes_removed <- overcap$definitions
    
    oc_vessels <- vessel_subset %>%
      group_by(ssvid, region, fao_region, eez_id, is_territorial) %>%
      summarize_at(paste0(overcap_subtypes_removed, "_subs"), list(removed = sum), na.rm = T) %>%
      ungroup() %>%
      mutate(subs_removed = rowSums(select(., one_of(paste0(overcap_subtypes_removed, "_subs_removed"))))) %>%
      dplyr::filter(subs_removed > 0)
    
    # Join to overcap subset
    overcap_vessel_subset <- vessel_tracking_df %>%
      bind_rows(oc_vessels)
    overcap_vessel_subset[is.na(overcap_vessel_subset)] <- 0
    
  }
  
  # Combine overcap subsets
  overcap_vessels <- overcap_vessel_subset %>%
    group_by(ssvid, region, fao_region, eez_id, is_territorial) %>%
    summarize_all(max, na.rm = T) %>%
    ungroup()
  
  ### Overcap scope ----------
  
  # There must be at least one affected vessel in order to define scope
  if(nrow(overcap_vessels) >= 1){
    
    ### Vessel list for scope (need additional characteristics)
    overcap_vessels_scope <- overcap_vessels %>%
      left_join(vessel_subset, by = c("ssvid", "region", "fao_region", "eez_id", "is_territorial"))
    
    # All affected vessels are within scope
    if(overcap$scope == "ALL"){
      
      overcap_vessels_scope <- overcap_vessels_scope
      
    }else if(overcap$scope == "SELECT"){
      
      # 1) Only select certain countries 
      if("MANUAL" %in% overcap$scope_select){
        
        # Select only certain member countries 
        countries <- overcap$scope_manual
        
        # Deal with EU
        if("EU" %in% countries){
          
          # Filter for selected Members
          overcap_vessels_scope <- overcap_vessels_scope %>%
            dplyr::filter(flag_iso3 %in% countries | flag_iso3 %in% eu_countries | flag_iso3 %in% eu_territories)
          
        }else{
          
          # Filter for selected Members
          overcap_vessels_scope <- overcap_vessels_scope %>%
            dplyr::filter(flag_iso3 %in% countries)
          
        }
        
        # 2) Only vessels fishing outside of their territorial waters  
      }else if("EX_TER" %in% overcap$scope_select){
        
        # Filter for only vessels fishing outside of territorial waters
        overcap_vessels_scope <- overcap_vessels_scope %>%
          dplyr::filter(!is_territorial)
        
        # 3) Only those coming from the top 10 worst subsidizers are within scope  
      }else if("SUB" %in% overcap$scope_select){
        
        # Country ranking subsidies 
        country_ranking_subsidies <- cap_tier_lookup %>%
          dplyr::filter(variable == "bad_ugly_subs") %>%
          dplyr::filter(!(iso3 %in% subs_ranking_exclude)) %>%
          group_by(iso3) %>%
          summarize(subs = unique(`2018`)) %>%
          mutate(subs_rank = dense_rank(desc(subs))) %>%
          arrange(subs_rank)
        
        # Find top 10 subsidizing Member states
        top_countries_subs <- country_ranking_subsidies$iso3[country_ranking_subsidies$subs_rank <= 10]
        
        # Deal with EU
        if("EU" %in% top_countries_subs){
          
          # Filter for top 10 subsidizing Member states
          overcap_vessels_scope <- overcap_vessels_scope %>%
            dplyr::filter(flag_iso3 %in% eu_countries | flag_iso3 %in% eu_territories | flag_iso3 %in% top_countries_subs)
          
        }else{
          # Filter for top 10 subsidizing Member states
          overcap_vessels_scope <- overcap_vessels_scope %>%
            dplyr::filter(flag_iso3 %in% top_countries_subs)
        }
        
        # 4a) Only those responsible for more than 1% of global capture production
      }else if("CAPTURE" %in% overcap$scope_select){
        
        capture_years <- paste0("", seq(2016, 2018, by = 1), "")
        
        # Country ranking capture 
        country_ranking_capture <- cap_tier_lookup %>%
          dplyr::filter(variable == "capture_production") %>%
          dplyr::filter(!(iso3 %in% capture_ranking_exclude)) %>%
          mutate(prod = rowSums(select(., one_of(c(capture_years))))) %>%
          mutate(percent_prod = prod/(sum(prod, na.rm = T))) %>%
          arrange(percent_prod)
        
        # Find states responsible for greater than 1% of global capture production
        top_countries_capture <- str_replace(country_ranking_capture$iso3[country_ranking_capture$percent_prod >= 0.01], "-T", "")
        
        # Deal with EU
        if("EU" %in% top_countries_capture){
          
          # Filter for top 10 subsidizing Member states
          overcap_vessels_scope <- overcap_vessels_scope %>%
            dplyr::filter(flag_iso3 %in% eu_countries | flag_iso3 %in% eu_territories | flag_iso3 %in% top_countries_capture)
          
        }else{
          # Filter for top 10 subsidizing Member states
          overcap_vessels_scope <- overcap_vessels_scope %>%
            dplyr::filter(flag_iso3 %in% top_countries_capture)
        }
        
      # 4b) Only those responsible for more than 2% of global capture production
      }else if("CAPTURE2" %in% overcap$scope_select){
        
        capture_years <- paste0("", seq(2016, 2018, by = 1), "")
        
        # Country ranking capture 
        country_ranking_capture <- cap_tier_lookup %>%
          dplyr::filter(variable == "capture_production") %>%
          dplyr::filter(!(iso3 %in% capture_ranking_exclude)) %>%
          mutate(prod = rowSums(select(., one_of(c(capture_years))))) %>%
          mutate(percent_prod = prod/(sum(prod, na.rm = T))) %>%
          arrange(percent_prod)
        
        # Find states responsible for greater than 1% of global capture production
        top_countries_capture <- str_replace(country_ranking_capture$iso3[country_ranking_capture$percent_prod >= 0.02], "-T", "")
        
        # Deal with EU
        if("EU" %in% top_countries_capture){
          
          # Filter for top 10 subsidizing Member states
          overcap_vessels_scope <- overcap_vessels_scope %>%
            dplyr::filter(flag_iso3 %in% eu_countries | flag_iso3 %in% eu_territories | flag_iso3 %in% top_countries_capture)
          
        }else{
          # Filter for top 10 subsidizing Member states
          overcap_vessels_scope <- overcap_vessels_scope %>%
            dplyr::filter(flag_iso3 %in% top_countries_capture)
        }

        # 4c) All vessels from Members accounting for more than 2% of global capture production, and only non-EEZ activity from Members accounting for less than 2% of global capture production  
      }else if("CAPTURE_EEZ" %in% overcap$scope_select){
        
        capture_years <- paste0("", seq(2016, 2018, by = 1), "")
        
        # Country ranking capture 
        country_ranking_capture <- cap_tier_lookup %>%
          dplyr::filter(variable == "capture_production") %>%
          dplyr::filter(!(iso3 %in% capture_ranking_exclude)) %>%
          mutate(prod = rowSums(select(., one_of(c(capture_years))))) %>%
          mutate(percent_prod = prod/(sum(prod, na.rm = T))) %>%
          arrange(percent_prod)
        
        # Find states responsible for greater than 2% of global capture production
        top_countries_capture <- str_replace(country_ranking_capture$iso3[country_ranking_capture$percent_prod >= 0.02], "-T", "")
        
        # Deal with EU and filter for states responsible for more than 2% of global capture production
        if("EU" %in% top_countries_capture){

          overcap_vessels_scope1 <- overcap_vessels_scope %>%
            dplyr::filter(flag_iso3 %in% eu_countries | flag_iso3 %in% eu_territories | flag_iso3 %in% top_countries_capture)
          
        }else{

          overcap_vessels_scope1 <- overcap_vessels_scope %>%
            dplyr::filter(flag_iso3 %in% top_countries_capture)
        }
        
        # Now Find states responsible for less than 2% of global capture production
        bottom_countries_capture <- str_replace(country_ranking_capture$iso3[country_ranking_capture$percent_prod < 0.02], "-T", "")
        
        # Deal with EU and filter for high seas and DW activity from states responsible for less than 2% of global capture production
        if("EU" %in% bottom_countries_capture){

          overcap_vessels_scope2 <- overcap_vessels_scope %>%
            dplyr::filter(flag_iso3 %in% eu_countries | flag_iso3 %in% eu_territories | flag_iso3 %in% bottom_countries_capture) %>%
            dplyr::filter(distant_water | prop_fishing_KWh_high_seas >= (overcap$hs_cutoff/100))
          
        }else{

          overcap_vessels_scope2 <- overcap_vessels_scope %>%
            dplyr::filter(flag_iso3 %in% bottom_countries_capture) %>%
            dplyr::filter(distant_water | prop_fishing_KWh_high_seas >= (overcap$hs_cutoff/100))
        }
        
        # Combine
        overcap_vessels_scope <- overcap_vessels_scope1 %>%
          bind_rows(overcap_vessels_scope2)

        # 5) Only those vessels fishing on the high seas    
      }else if("HS" %in% overcap$scope_select & !is.na(overcap$hs_cutoff)){
        
        # Filter for high seas vessels only
        overcap_vessels_scope <- overcap_vessels_scope %>%
          dplyr::filter(prop_fishing_KWh_high_seas >= (overcap$hs_cutoff/100))
        
        # 6) Distant water fishing  
      }else if("DW" %in% overcap$scope_select){
        
        # Filter for distant water fishing only
        overcap_vessels_scope <- overcap_vessels_scope %>%
          dplyr::filter(distant_water)
        
        # 6) Distant water fishing OR HIGH SEAS    
      }else if("OUT" %in% overcap$scope_select & !is.na(overcap$hs_cutoff)){
        
        # Filter for high seas OR distant water fishing only
        overcap_vessels_scope <- overcap_vessels_scope %>%
          dplyr::filter(distant_water | prop_fishing_KWh_high_seas >= (overcap$hs_cutoff/100))
        
        # 7) Disputed areas  
      }else if("DISPUTE" %in% overcap$scope_select){
        
        # Filter for disputed waters
        overcap_vessels_scope <- overcap_vessels_scope %>%
          dplyr::filter(is_disputed)
        
        # 8) Length   
      }else if("LENGTH" %in% overcap$scope_select & !is.na(overcap$length_cutoff)){
        
        # Filter by vessel length
        overcap_vessels_scope <- overcap_vessels_scope %>%
          dplyr::filter(length_m >= overcap$length_cutoff)
        
        # 9) Tonnage    
      }else if("TONNAGE" %in% overcap$scope_select & !is.na(overcap$tonnage_cutoff)){
        
        # Filter by vessel tonnage
        overcap_vessels_scope <- overcap_vessels_scope %>%
          dplyr::filter(tonnage_gt >= overcap$tonnage_cutoff)
        
        # 10) Engine power    
      }else if("ENGINE" %in% overcap$scope_select & !is.na(overcap$engine_cutoff)){
        
        # Filter by vessel engine power
        overcap_vessels_scope <- overcap_vessels_scope %>%
          dplyr::filter(engine_power_kw >= overcap$engine_cutoff)
        
      # 11) Distant water fishing OR HIGH SEAS (for all vessels) plus all fishing by OA vessels    
      }else if("OUT/OA" %in% overcap$scope_select & !is.na(overcap$hs_cutoff)){
      
        # Filter for high seas OR distant water fishing only
        overcap_vessels_scope <- overcap_vessels_scope %>%
          dplyr::filter(fmi_best < managed_threshold | distant_water | prop_fishing_KWh_high_seas >= (overcap$hs_cutoff/100))
        
    }
      
    } # /SELECT
    
    # Overcap vessels in scope
    overcap_vessels_scope <- overcap_vessels_scope %>%
      dplyr::select(ssvid, region, fao_region, eez_id, is_territorial, contains("subs_removed"))
    
  }else{
    
    overcap_vessels_scope <- overcap_vessels
    
  }
  
  ### Overcapacity - SDT ----------
  
  if(nrow(overcap_vessels_scope) >= 1 & overcap$allow_sdt == "YES"){
    
    ### Vessel list for S&DT
    overcap_vessels_sdt <- vessel_subset %>%
      right_join(overcap_vessels_scope, by = c("ssvid", "region", "fao_region", "eez_id", "is_territorial"))
    
  ### 1) LDC S&DT ---
  
  if(overcap$sdt_ldc == "YES"){
    
    overcap_vessels_sdt_ldc <- overcap_vessels_sdt %>%
      dplyr::filter(development_status == "LDC")
    
    if("ALL" %in% overcap$sdt_what_ldc){
      
      # If all Member-flagged vessels should be exempt
      overcap_vessels_sdt_ldc <- overcap_vessels_sdt_ldc
      
    }else if("DOMESTIC" %in% overcap$sdt_what_ldc){
      
      # Exempt domestic fishing vessels (less than 1% of fishing effort)
      overcap_vessels_sdt_ldc <- overcap_vessels_sdt_ldc %>%
        dplyr::filter(prop_fishing_KWh_high_seas < domestic_vessel_cutoff) 
      
    }else if("TER" %in% overcap$sdt_what_ldc){
      
      # Exempt vessels fishing in territorial waters
      overcap_vessels_sdt_ldc <- overcap_vessels_sdt_ldc %>%
        dplyr::filter(is_territorial)
      
    }else if("HS" %in% overcap$sdt_what_ldc){
      
      # Exempt vessels fishing on the high seas
      overcap_vessels_sdt_ldc <- overcap_vessels_sdt_ldc %>%
        dplyr::filter(prop_fishing_KWh_high_seas >= (overcap$sdt_hs_cutoff_ldc/100))
      
    }
    
  }else{
    
    overcap_vessels_sdt_ldc <- vessel_sdt_df
    
  }
  
  ### 2) Developing country S&DT ---
  
  if(overcap$sdt_developing == "YES"){
    
    overcap_vessels_sdt_developing <- overcap_vessels_sdt %>%
      dplyr::filter(development_status == "Developing")
    
    if("ALL" %in% overcap$sdt_what_developing){
      
      # If all Member-flagged vessels are exempt
      overcap_vessels_sdt_developing <- overcap_vessels_sdt_developing
      
    }else if("DOMESTIC" %in% overcap$sdt_what_developing){
      
      # Exempt domestic fishing vessels (less than 1% of fishing effort)
      overcap_vessels_sdt_developing <- overcap_vessels_sdt_developing %>%
        dplyr::filter(prop_fishing_KWh_high_seas < domestic_vessel_cutoff) 
      
    }else if("TER" %in% overcap$sdt_what_developing){
      
      # Exempt vessels fishing in territorial waters
      overcap_vessels_sdt_developing <- overcap_vessels_sdt_developing %>%
        dplyr::filter(is_territorial)
      
    }else if("HS" %in% overcap$sdt_what_developing){
      
      # Exempt vessels fishing on the high seas
      overcap_vessels_sdt_developing <- overcap_vessels_sdt_developing %>%
        dplyr::filter(prop_fishing_KWh_high_seas >= (overcap$sdt_hs_cutoff_developing/100))
      
    }
    
  }else{
    
    overcap_vessels_sdt_developing <- vessel_sdt_df
    
  }
  
  ### 3) SVE S&DT ---
  
  if(overcap$sdt_sve == "YES"){
    
    overcap_vessels_sdt_sve <- overcap_vessels_sdt %>%
      dplyr::filter(flag_iso3 %in% sves)
    
    if("ALL" %in% overcap$sdt_what_sve){
      
      # If all Member-flagged vessels are exempt... This trumps all other options
      overcap_vessels_sdt_sve <- overcap_vessels_sdt_sve
      
    }else if("DOMESTIC" %in% overcap$sdt_what_sve){
      
      # Exempt domestic fishing vessels (less than 1% of fishing effort)
      overcap_vessels_sdt_sve <- overcap_vessels_sdt_sve %>%
        dplyr::filter(prop_fishing_KWh_high_seas < domestic_vessel_cutoff) 
      
    }else if("TER" %in% overcap$sdt_what_sve){
      
      # Exempt vessels fishing in territorial waters
      overcap_vessels_sdt_sve <- overcap_vessels_sdt_sve %>%
        dplyr::filter(is_territorial)
      
    }else if("HS" %in% overcap$sdt_what_sve){
      
      # Exempt vessels fishing on the high seas
      overcap_vessels_sdt_sve <- overcap_vessels_sdt_sve %>%
        dplyr::filter(prop_fishing_KWh_high_seas >= (overcap$sdt_hs_cutoff_sve/100))
      
    }
    
  }else{
    
    overcap_vessels_sdt_sve <- vessel_sdt_df
    
  }
  
  # Combine S&DT across all categories
  overcap_vessels_sdt <- overcap_vessels_sdt_ldc %>%
    bind_rows(overcap_vessels_sdt_developing) %>%
    bind_rows(overcap_vessels_sdt_sve)
  
  # List of vessel ids to be excluded from affected because of s&dt
  overcap_vessels_sdt_exclude <- overcap_vessels_sdt %>%
    distinct(ssvid, region, fao_region, eez_id, is_territorial)
  
}else{
  
  # List of vessel ids to be excluded from affected because of s&dt
  overcap_vessels_sdt_exclude <- vessel_sdt_df
  
}

### Output subsidy summary for all vessels triggering overcapacity prohibitions, within scope and excluding S&DT
overcap_vessels_out <- overcap_vessels_scope %>%
  anti_join(overcap_vessels_sdt_exclude, by = c("ssvid", "region", "fao_region", "eez_id", "is_territorial"))

  ### ---------------------------------------------------------------
  ### Wrangle pre-cap output -------------------
  ### ---------------------------------------------------------------
  
  # Combine info on subsidies removed by vessel and area
  affected_subset <- vessel_tracking_df %>%
    bind_rows(iuu_vessels_out) %>%
    bind_rows(oa_vessels_out) %>%
    bind_rows(overcap_vessels_out) %>%
    group_by(ssvid, region, fao_region, eez_id, is_territorial) %>%
    summarize_all(max, na.rm = T) %>%
    ungroup() %>%
    dplyr::select(-subs_removed)

  # Combine with full affected vessel info
  affected_vessels <- vessel_subset %>%
    right_join(affected_subset, by = c("ssvid", "region", "fao_region", "eez_id", "is_territorial"))
  
  
  # Summarize affected subsidy info by flag (needed for cap)
  affected_flag <- left_join(
    
    # Affected general
    (affected_vessels %>%
    group_by(flag_iso3) %>%
    summarize(n_vessels = n_distinct(ssvid),
              n_vessel_class = n_distinct(vessel_class),
              avg_length_m = mean(length_m, na.rm = T),
              avg_tonnage_gt = mean(tonnage_gt, na.rm = T),
              avg_engine_power_kw = mean(engine_power_kw, na.rm = T),
              regions_fished = n_distinct(eez_hs_code),
              development_status = unique(development_status))),
  
    # Affected summed values
    (affected_vessels %>%
    group_by(flag_iso3) %>%
    summarize_at(c("fishing_hours_eez_fao_ter", "fishing_KWh_eez_fao_ter", "catch", "revenue", "good_subs", "bad_subs", "ugly_subs", paste0(subsidy_types_all, "_subs"), paste0(subsidy_types_all, "_subs_removed")), sum, na.rm = T)),
    
    by = "flag_iso3"
  ) %>%
    ungroup()
  
  ### Section # 4 ---------------------------------------
  ### Cap and tier --------------------------------------
  ### ---------------------------------------------------
  
  if(cap_tier$on_off == "YES"){
  
  # Subsidy types to include in cap. 
  # Other bad subsidies could have either been completely prohibited in the overcapacity section, or are allowed.
  # We need to figure that out in this section
  subsidies_to_cap <- paste0(cap_tier$subsidy_types, "_subs")

  # If no subsidy subtypes are selected for the cap, it doesn't do anything
  if(length(subsidies_to_cap) == 0){
    
    return_vessels <- affected_vessels %>%
      dplyr::select(ssvid, region, fao_region, eez_id, is_territorial, flag_iso3, catch, revenue, fishing_hours_eez_fao_ter, fishing_KWh, contains("subs"))
    
  }else{
    
    ### Do some wrangling to make sure that we will have caps for all members
    member_caps_df <- tibble(iso3 = wto_members_and_observers,
                             cap = numeric(length(wto_members_and_observers))) %>%
      dplyr::filter(!(iso3 %in% cap_exclude)) %>%
      arrange(iso3)

    ### Create tidy data frame to track caps, already removed subsidies, and allowed subsidies
    # Get total subsidy amount by flag and subtype
    subs_total <- cap_tier_lookup %>%
      dplyr::filter(variable %in% paste0(subsidy_types_all, "_subs")) %>%
      dplyr::select(iso3, `2018`, variable) %>%
      spread(variable, `2018`) %>%
      gather(type, subs, -"iso3") %>%
      mutate(type = str_replace(type, "_subs", "")) %>%
      rename(flag_iso3 = iso3) %>%
      group_by(flag_iso3)
    
    # Get base subsidy amounts by flag and subtype that will be included in the cap
    subs_for_capping <- cap_tier_lookup %>%
      dplyr::filter(variable %in% subsidies_to_cap) %>%
      dplyr::select(iso3, `2018`, variable) %>%
      spread(variable, `2018`) %>%
      gather(type, subs_for_cap, -"iso3") %>%
      mutate(type = str_replace(type, "_subs", "")) %>%
      rename(flag_iso3 = iso3) %>%
      group_by(flag_iso3) %>%
      mutate(tot_subs_for_cap = sum(subs_for_cap, na.rm = T)) %>%
      ungroup() %>%
      mutate(subs_for_cap_percent_tot = ifelse(tot_subs_for_cap == 0, 0, subs_for_cap/tot_subs_for_cap)) %>%
      dplyr::select(-tot_subs_for_cap)
    
    # Get already removed subsidies by flag and subtype
    subs_removed <- affected_flag %>%
      dplyr::select(flag_iso3, contains("_subs_removed")) %>%
      gather(type, subs_removed, -1) %>%
      mutate(type = str_replace(type, "_subs_removed", "")) %>%
      group_by(flag_iso3)
    
    # Combine back together
    cap_df <- subs_total %>%
      left_join(subs_for_capping, by = c("flag_iso3", "type")) %>%
      left_join(subs_removed, by = c("flag_iso3", "type"))
    cap_df[is.na(cap_df)] <- 0
    
    # Remove individual EU countries - will allocate their individual caps later
    cap_df <- cap_df %>%
      dplyr::filter(flag_iso3 %in% member_caps_df$iso3)
    
    # Make blank entries for countries for which we don't have subsidy information
    cap_missing_countries <- member_caps_df$iso3[!(member_caps_df$iso3 %in% cap_df$flag_iso3)]
    
    cap_df_blank <- tibble(flag_iso3 = rep(cap_missing_countries, each = length(subsidy_types_all)),
                           type = rep(subsidy_types_all, times = length(cap_missing_countries))) %>%
      mutate(subs = 0,
             subs_for_cap = 0,
             subs_for_cap_percent_tot = 0,
             subs_removed = 0)
    
    # Add back together
    cap_df <- cap_df %>%
      bind_rows(cap_df_blank) %>%
      arrange(flag_iso3)
    
   ### DETERMINE TIERING ----------
    
   ### One Tier
   if(cap_tier$tier_number == "ONE"){
     
     tier_1_flags_out <- member_caps_df$iso3

   ### Two Tiers     
   }else if(cap_tier$tier_number == "TWO"){
     
     # Establish who is in which tier
     if(cap_tier$tier_system == "CAPTURE"){
       
       percent_cutoff <- cap_tier$two_tier_cutoff/100
       
       capture_years <- paste0("", seq(2016, 2018, by = 1), "")
       
       # Country ranking capture 
       cap_tier_dat_sorted <- cap_tier_lookup %>%
         dplyr::filter(variable == "capture_production") %>%
         dplyr::filter(!(iso3 %in% capture_ranking_exclude)) %>%
         mutate(prod = rowSums(select(., one_of(c(capture_years))))) %>%
         mutate(percent_prod = prod/(sum(prod, na.rm = T))) %>%
         arrange(percent_prod) %>%
         mutate(new_tier = case_when(percent_prod >= percent_cutoff ~ 1,
                                     percent_prod < percent_cutoff ~ 2,
                                     TRUE ~ 2)) %>%
         mutate(iso3 = str_replace(iso3, "-T", "")) %>%
         arrange(iso3)
       
       # See if we're missing any countries using this method
       missing_countries <- member_caps_df$iso3[!(member_caps_df$iso3 %in% cap_tier_dat_sorted$iso3)]
       missing_countries_df <- tibble(iso3 = missing_countries, new_tier = rep(2, length(missing_countries)))
       
       # Assign tiers to those
       cap_tier_dat_sorted <- cap_tier_dat_sorted %>%
         bind_rows(missing_countries_df)
       
     #}else if(cap_tier$tier_system == "EXPORTS"){
       
       # percent_cutoff <- cap_tier$two_tier_cutoff/100
       # 
       # cap_tier_dat_sorted <- cap_tier_dat %>%
       #   mutate(new_tier = case_when(percent_exports >= percent_cutoff ~ 1, 
       #                               percent_exports < percent_cutoff ~ 2,
       #                               is.na(percent_exports) ~ 2))
       
     }else if(cap_tier$tier_system == "SUBS"){
       
       percent_cutoff <- cap_tier$two_tier_cutoff/100
       
       # Country ranking subsidies 
       cap_tier_dat_sorted <- cap_tier_lookup %>%
         dplyr::filter(variable == "bad_ugly_subs") %>%
         dplyr::filter(!(iso3 %in% subs_ranking_exclude)) %>%
         group_by(iso3) %>%
         summarize(subs = unique(`2018`)) %>%
         mutate(percent_subs = subs/(sum(subs, na.rm = T))) %>%
         arrange(percent_subs) %>%
         mutate(new_tier = case_when(percent_subs >= percent_cutoff ~ 1,
                                     percent_subs < percent_cutoff ~ 2,
                                     TRUE ~ 2)) %>%
         arrange(iso3)
         
         # See if we're missing any countries using this method
         missing_countries <- member_caps_df$iso3[!(member_caps_df$iso3 %in% cap_tier_dat_sorted$iso3)]
         missing_countries_df <- tibble(iso3 = missing_countries, new_tier = rep(2, length(missing_countries)))
       
        # Assign tiers to those
        cap_tier_dat_sorted <- cap_tier_dat_sorted %>%
           bind_rows(missing_countries_df)
       
     }else if(cap_tier$tier_system == "DEVELOPMENT"){
       
       cap_tier_dat_sorted <- cap_tier_lookup %>%
         distinct(iso3) %>%
         dplyr::filter(!(iso3 %in% development_ranking_exclude)) %>%
         left_join(country_lookup %>% dplyr::select(iso3, sovereign_iso3, development_status), by = c("iso3")) %>%
         mutate(development_status = case_when(!is.na(development_status) ~ development_status,
                                               iso3 == "EU" ~ "Developed",
                                               TRUE ~ "Developing")) %>%
         mutate(new_tier = case_when(development_status == "Developed" ~ 1, 
                                     development_status == "Developing" ~ 1,
                                     development_status == "LDC" ~ 2,
                                     TRUE ~ 2)) %>%
         arrange(iso3)
       
       # See if we're missing any countries using this method
       missing_countries <- member_caps_df$iso3[!(member_caps_df$iso3 %in% cap_tier_dat_sorted$iso3)]
       missing_countries_df <- tibble(iso3 = missing_countries, new_tier = rep(2, length(missing_countries)))
       
       # Assign tiers to those
       cap_tier_dat_sorted <- cap_tier_dat_sorted %>%
         bind_rows(missing_countries_df)
       
     }else if(cap_tier$tier_system == "TOP10"){
       
       ### Need to get top 10 global producers and development status
       capture_years <- paste0("", seq(2016, 2018, by = 1), "")
       
       # Country ranking capture 
       cap_tier_dat_sorted <- cap_tier_lookup %>%
         dplyr::filter(variable == "capture_production") %>%
         dplyr::filter(!(iso3 %in% capture_ranking_exclude)) %>%
         mutate(prod = rowSums(select(., one_of(c(capture_years))))) %>%
         mutate(percent_prod = prod/(sum(prod, na.rm = T))) %>%
         arrange(percent_prod) %>%
         left_join(country_lookup %>% dplyr::select(iso3, development_status), by = c("iso3")) %>%
         mutate(development_status = case_when(!is.na(development_status) ~ development_status,
                                               iso3 == "EU" ~ "Developed",
                                               iso3 %in% c("USA-T", "NOR-T", "NZL-T", "AUS-T") ~ "Developed",
                                               iso3 %in% c("MAR-T") ~ "Developing",
                                               TRUE ~ "Developing")) %>%
         mutate(production_rank = dense_rank(desc(percent_prod))) %>%
         mutate(new_tier = case_when(production_rank <= 10 ~ 1,
                                     development_status == "Developed" ~ 1,
                                     development_status == "Developing" ~ 2,
                                     development_status == "LDC" ~ 2,
                                     TRUE ~ 2)) %>%
         mutate(iso3 = str_replace(iso3, "-T", "")) %>%
         arrange(iso3)
       
       # See if we're missing any countries using this method
       missing_countries <- member_caps_df$iso3[!(member_caps_df$iso3 %in% cap_tier_dat_sorted$iso3)]
       missing_countries_df <- tibble(iso3 = missing_countries, new_tier = rep(2, length(missing_countries)))
       
       # Assign tiers to those
       cap_tier_dat_sorted <- cap_tier_dat_sorted %>%
         bind_rows(missing_countries_df)
     }
     
     ### Get flag states in Tier 1
     tier_1_flags <- unique(cap_tier_dat_sorted$iso3[cap_tier_dat_sorted$new_tier == 1])
     
     if("EU" %in% tier_1_flags){
       eu_flags_out1 <- c(eu_countries, eu_territories)
     }else{
       eu_flags_out1 <- NULL
     }
     
     if(any(tier_1_flags %in% territories$sovereign_iso3)){
       ter_flags_out1 <- (territories %>% dplyr::filter(sovereign_iso3 %in% tier_1_flags))$iso3
     }else{
       ter_flags_out1 <- NULL
     }  
     
     tier_1_flags_out <- c(tier_1_flags, eu_flags_out1, ter_flags_out1)
     
     ### Get flag states in Tier 2
     tier_2_flags <- unique(cap_tier_dat_sorted$iso3[cap_tier_dat_sorted$new_tier == 2])
     
     if("EU" %in% tier_2_flags){
       eu_flags_out2 <- c(eu_countries, eu_territories)
     }else{
       eu_flags_out2 <- NULL
     }
     
     if(any(tier_2_flags %in% territories$sovereign_iso3)){
       ter_flags_out2 <- (territories %>% dplyr::filter(sovereign_iso3 %in% tier_2_flags))$iso3
     }else{
       ter_flags_out2 <- NULL
     }  
       
     tier_2_flags_out <- c(tier_2_flags, eu_flags_out2, ter_flags_out2)

   ### Three tiers    
   }else if(cap_tier$tier_number == "THREE"){
     
     # Establish who is in which tier
     if(cap_tier$tier_system == "CAPTURE"){
       
       percent_cutoff_top <- cap_tier$three_tier_cutoff[2]/100
       percent_cutoff_bottom <- cap_tier$three_tier_cutoff[1]/100
       
       capture_years <- paste0("", seq(2016, 2018, by = 1), "")
       
       # Country ranking capture 
       cap_tier_dat_sorted <- cap_tier_lookup %>%
         dplyr::filter(variable == "capture_production") %>%
         dplyr::filter(!(iso3 %in% capture_ranking_exclude)) %>%
         mutate(prod = rowSums(select(., one_of(c(capture_years))))) %>%
         mutate(percent_prod = prod/(sum(prod, na.rm = T))) %>%
         arrange(percent_prod) %>%
         mutate(new_tier = case_when(percent_prod >= percent_cutoff_top ~ 1,
                                     (percent_prod < percent_cutoff_top & percent_prod >= percent_cutoff_bottom) ~ 2,
                                     percent_prod < percent_cutoff_bottom ~ 3,
                                     TRUE ~ 3))%>%
         mutate(iso3 = str_replace(iso3, "-T", "")) %>%
         arrange(iso3)
       
       # See if we're missing any countries using this method
       missing_countries <- member_caps_df$iso3[!(member_caps_df$iso3 %in% cap_tier_dat_sorted$iso3)]
       missing_countries_df <- tibble(iso3 = missing_countries, new_tier = rep(3, length(missing_countries)))
       
       # Assign tiers to those
       cap_tier_dat_sorted <- cap_tier_dat_sorted %>%
         bind_rows(missing_countries_df)
       
     }else if(cap_tier$tier_system == "SUBS"){
       
       percent_cutoff_top <- cap_tier$three_tier_cutoff[2]/100
       percent_cutoff_bottom <- cap_tier$three_tier_cutoff[1]/100
       
       # Country ranking subsidies 
       cap_tier_dat_sorted <- cap_tier_lookup %>%
         dplyr::filter(variable == "bad_ugly_subs") %>%
         dplyr::filter(!(iso3 %in% subs_ranking_exclude)) %>%
         group_by(iso3) %>%
         summarize(subs = unique(`2018`)) %>%
         mutate(percent_subs = subs/(sum(subs, na.rm = T))) %>%
         arrange(percent_subs) %>%
         mutate(new_tier = case_when(percent_subs >= percent_cutoff_top ~ 1,
                                     (percent_subs < percent_cutoff_top & percent_subs >= percent_cutoff_bottom) ~ 2,
                                     percent_subs < percent_cutoff_bottom ~ 3,
                                     TRUE ~ 3)) %>%
         arrange(iso3)
       
       # See if we're missing any countries using this method
       missing_countries <- member_caps_df$iso3[!(member_caps_df$iso3 %in% cap_tier_dat_sorted$iso3)]
       missing_countries_df <- tibble(iso3 = missing_countries, new_tier = rep(3, length(missing_countries)))
       
       # Assign tiers to those
       cap_tier_dat_sorted <- cap_tier_dat_sorted %>%
         bind_rows(missing_countries_df)
       
       
     }else if(cap_tier$tier_system == "DEVELOPMENT"){
       
       cap_tier_dat_sorted <- cap_tier_lookup %>%
         distinct(iso3) %>%
         dplyr::filter(!(iso3 %in% development_ranking_exclude)) %>%
         left_join(country_lookup %>% dplyr::select(iso3, sovereign_iso3, development_status), by = c("iso3")) %>%
         mutate(development_status = case_when(!is.na(development_status) ~ development_status,
                                               iso3 == "EU" ~ "Developed",
                                               TRUE ~ "Developing")) %>%
         mutate(new_tier = case_when(development_status == "Developed" ~ 1, 
                                     development_status == "Developing" ~ 2,
                                     development_status == "LDC" ~ 3,
                                     TRUE ~ 3)) %>%
         arrange(iso3)
       
       # See if we're missing any countries using this method
       missing_countries <- member_caps_df$iso3[!(member_caps_df$iso3 %in% cap_tier_dat_sorted$iso3)]
       missing_countries_df <- tibble(iso3 = missing_countries, new_tier = rep(2, length(missing_countries)))
       
       # Assign tiers to those
       cap_tier_dat_sorted <- cap_tier_dat_sorted %>%
         bind_rows(missing_countries_df)
       
     }else if(cap_tier$tier_system == "TOP10"){
       
       ### Need to get top 10 global producers and development status
       capture_years <- paste0("", seq(2016, 2018, by = 1), "")
       
       # Country ranking capture 
       cap_tier_dat_sorted <- cap_tier_lookup %>%
         dplyr::filter(variable == "capture_production") %>%
         dplyr::filter(!(iso3 %in% capture_ranking_exclude)) %>%
         mutate(prod = rowSums(select(., one_of(c(capture_years))))) %>%
         mutate(percent_prod = prod/(sum(prod, na.rm = T))) %>%
         arrange(percent_prod) %>%
         left_join(country_lookup %>% dplyr::select(iso3, development_status), by = c("iso3")) %>%
         mutate(development_status = case_when(!is.na(development_status) ~ development_status,
                                               iso3 == "EU" ~ "Developed",
                                               iso3 %in% c("USA-T", "NOR-T", "NZL-T", "AUS-T") ~ "Developed",
                                               iso3 %in% c("MAR-T") ~ "Developing",
                                               TRUE ~ "Developing")) %>%
         mutate(production_rank = dense_rank(desc(percent_prod))) %>%
         mutate(new_tier = case_when(production_rank <= 10 ~ 1,
                                     development_status == "Developed" ~ 1,
                                     development_status == "Developing" ~ 2,
                                     development_status == "LDC" ~ 3,
                                     TRUE ~ 3)) %>%
         mutate(iso3 = str_replace(iso3, "-T", "")) %>%
         arrange(iso3)
       
       # See if we're missing any countries using this method
       missing_countries <- member_caps_df$iso3[!(member_caps_df$iso3 %in% cap_tier_dat_sorted$iso3)]
       missing_countries_df <- tibble(iso3 = missing_countries, new_tier = rep(2, length(missing_countries)))
       
       # Assign tiers to those
       cap_tier_dat_sorted <- cap_tier_dat_sorted %>%
         bind_rows(missing_countries_df)
       
     }
     
     ### Get flag states in Tier 1
     tier_1_flags <- unique(cap_tier_dat_sorted$iso3[cap_tier_dat_sorted$new_tier == 1])
     
     if("EU" %in% tier_1_flags){
       eu_flags_out1 <- c(eu_countries, eu_territories)
     }else{
       eu_flags_out1 <- NULL
     }
     
     if(any(tier_1_flags %in% territories$sovereign_iso3)){
       ter_flags_out1 <- (territories %>% dplyr::filter(sovereign_iso3 %in% tier_1_flags))$iso3
     }else{
       ter_flags_out1 <- NULL
     }  
     
     tier_1_flags_out <- c(tier_1_flags, eu_flags_out1, ter_flags_out1)
     
     ### Get flag states in Tier 2
     tier_2_flags <- unique(cap_tier_dat_sorted$iso3[cap_tier_dat_sorted$new_tier == 2])
     
     if("EU" %in% tier_2_flags){
       eu_flags_out2 <- c(eu_countries, eu_territories)
     }else{
       eu_flags_out2 <- NULL
     }
     
     if(any(tier_2_flags %in% territories$sovereign_iso3)){
       ter_flags_out2 <- (territories %>% dplyr::filter(sovereign_iso3 %in% tier_2_flags))$iso3
     }else{
       ter_flags_out2 <- NULL
     }  
     
     tier_2_flags_out <- c(tier_2_flags, eu_flags_out2, ter_flags_out2)
     
     ### Get flag states in Tier 3
     tier_3_flags <- unique(cap_tier_dat_sorted$iso3[cap_tier_dat_sorted$new_tier == 3])
     
     if("EU" %in% tier_3_flags){
       eu_flags_out3 <- c(eu_countries, eu_territories)
     }else{
       eu_flags_out3 <- NULL
     }
     
     if(any(tier_3_flags %in% territories$sovereign_iso3)){
       ter_flags_out3 <- (territories %>% dplyr::filter(sovereign_iso3 %in% tier_3_flags))$iso3
     }else{
       ter_flags_out3 <- NULL
     }  
     
     tier_3_flags_out <- c(tier_3_flags, eu_flags_out3, ter_flags_out3)
     
   } # close tiering system
    
   ### DETERMINE CAPS ----------

   ### Tier 1 - ALWAYS  
    if(cap_tier$tier1_cap_rule == "BRAZIL"){
      
      tot_subs <- cap_df %>%
        group_by(flag_iso3) %>%
        summarize(tot_subs = sum(subs_for_cap, na.rm = T)) %>%
        mutate(percent_reduction = case_when(tot_subs < 15000000 ~ 0,
                                             tot_subs >= 15000000 & tot_subs < 30000000 ~ 0.15,
                                             tot_subs >= 30000000 & tot_subs < 75000000 ~ 0.2,
                                             tot_subs >= 75000000 & tot_subs < 150000000 ~ 0.25,
                                             tot_subs >= 150000000 & tot_subs < 300000000 ~ 0.3,
                                             tot_subs >= 300000000 & tot_subs < 600000000 ~ 0.35,
                                             tot_subs >= 600000000 & tot_subs < 1200000000 ~ 0.4,
                                             tot_subs >= 1200000000 ~ 0.45,
                                             TRUE ~ 0)) %>%
        dplyr::select(flag_iso3, percent_reduction)
      
      flag_caps_tier1 <- cap_df %>%
        left_join(tot_subs, by = "flag_iso3") %>%
        dplyr::filter(flag_iso3 %in% tier_1_flags_out) %>%
        mutate(cap = subs_for_cap*percent_reduction) %>%
        dplyr::select(-percent_reduction)
      
    }else if(cap_tier$tier1_cap_rule == "PERCENT_SUBS"){
       
       percent_cap <- cap_tier$tier1_cap_percent/100
       
       flag_caps_tier1 <- cap_df %>%
         dplyr::filter(flag_iso3 %in% tier_1_flags_out) %>%
         mutate(cap = subs_for_cap*percent_cap)
       
    }else if(cap_tier$tier1_cap_rule == "VALUE"){
       
       absolute_cap <- cap_tier$tier1_cap_value*1e6
       
       flag_caps_tier1 <- cap_df %>%
         dplyr::filter(flag_iso3 %in% tier_1_flags_out) %>%
         mutate(cap = absolute_cap*subs_for_cap_percent_tot)
       
    }else if(cap_tier$tier1_cap_rule == "PERCENT_REV"){
       
       percent_cap <- cap_tier$tier1_cap_percent/100
       
       # Country revenue 
       revenue_years <- paste0("", seq(2016, 2018, by = 1), "")
       
       flag_revenue <- cap_tier_lookup %>%
         dplyr::filter(variable == "landed_value") %>%
         mutate(revenue = rowSums(select(., one_of(c(revenue_years))))) %>%
         dplyr::select(iso3, revenue) %>%
         mutate(cap_value = revenue*percent_cap) %>%
         dplyr::select(iso3, cap_value)
       
       flag_caps_tier1 <- cap_df %>%
         dplyr::filter(flag_iso3 %in% tier_1_flags_out) %>%
         left_join(flag_revenue, by = c("flag_iso3" = "iso3")) %>%
         mutate(new_cap_value = ifelse(subs_for_cap == 0, 0, cap_value)) %>%
         mutate(cap = new_cap_value*subs_for_cap_percent_tot) %>%
         dplyr::select(-cap_value, -new_cap_value)
       
     }else if(cap_tier$tier1_cap_rule == "FISHERS"){
       
       global_fishers <- cap_tier_lookup %>%
         dplyr::filter(variable == "fishers") %>%
         gather(year, value, -c("iso3", "variable", "is_WTO")) %>%
         group_by(iso3, variable, is_WTO) %>%
         dplyr::filter(year == max(year[!is.na(value)])) %>%
         ungroup() %>%
         dplyr::select(iso3, fishers = value)
       
       global_average_fishers <- mean(global_fishers$fishers, na.rm = T)
       
       global_subs_per_fisher <- cap_df %>%
         dplyr::filter(type %in% str_replace(subsidies_to_cap, "_subs", "")) %>%
         group_by(flag_iso3) %>%
         summarize(tot_subs = sum(subs, na.rm = T)) %>%
         left_join(global_fishers, by = c("flag_iso3" = "iso3")) %>%
         mutate(subs_per_fisher = tot_subs/fishers)
         
       global_average_subs_per_fisher <- mean(global_subs_per_fisher$subs_per_fisher, na.rm = T)
         
       percent_cap <- cap_tier$tier1_cap_percent/100
       
       # Get number of fishers from most recent year
       flag_fishers <- global_fishers %>%
         mutate(cap_value = global_average_subs_per_fisher*fishers*percent_cap) %>%
         dplyr::select(iso3, cap_value)
         
       flag_caps_tier1 <- cap_df %>%
         dplyr::filter(flag_iso3 %in% tier_1_flags_out) %>%
         left_join(flag_fishers, by = c("flag_iso3" = "iso3")) %>%
         mutate(new_cap_value = ifelse(subs_for_cap == 0, 0, cap_value)) %>%
         mutate(cap = new_cap_value*subs_for_cap_percent_tot) %>%
         dplyr::select(-cap_value, -new_cap_value)
       
     }else if(cap_tier$tier1_cap_rule == "BEST"){
       
       ### Do percent subs calculations
       percent_subs_cap <- cap_tier$tier1_cap_best_percent_subs/100
       
       flag_caps_subs_tier1 <- cap_df %>%
         dplyr::filter(flag_iso3 %in% tier_1_flags_out) %>%
         mutate(subs_cap = subs_for_cap*percent_subs_cap) %>%
         dplyr::select(flag_iso3, type, subs_cap)
       
       ### Do percent landed value calculation
       percent_landed_value_cap <- cap_tier$tier1_cap_best_percent_landed_value/100
       
       # Country revenue 
       revenue_years <- paste0("", seq(2016, 2018, by = 1), "")
       
       flag_revenue <- cap_tier_lookup %>%
         dplyr::filter(variable == "landed_value") %>%
         mutate(revenue = rowSums(select(., one_of(c(revenue_years))))) %>%
         dplyr::select(iso3, revenue) %>%
         mutate(cap_value = revenue*percent_landed_value_cap) %>%
         dplyr::select(iso3, cap_value)
       
       flag_caps_landed_value_tier1 <- cap_df %>%
         dplyr::filter(flag_iso3 %in% tier_1_flags_out) %>%
         left_join(flag_revenue, by = c("flag_iso3" = "iso3")) %>%
         mutate(new_cap_value = ifelse(subs_for_cap == 0, 0, cap_value)) %>%
         mutate(landed_value_cap = new_cap_value*subs_for_cap_percent_tot) %>%
         dplyr::select(flag_iso3, type, landed_value_cap)
       
       ### Do fishers calculation
       global_fishers <- cap_tier_lookup %>%
         dplyr::filter(variable == "fishers") %>%
         gather(year, value, -c("iso3", "variable", "is_WTO")) %>%
         group_by(iso3, variable, is_WTO) %>%
         dplyr::filter(year == max(year[!is.na(value)])) %>%
         ungroup() %>%
         dplyr::select(iso3, fishers = value)
       
       global_average_fishers <- mean(global_fishers$fishers, na.rm = T)
       
       global_subs_per_fisher <- cap_df %>%
         dplyr::filter(type %in% str_replace(subsidies_to_cap, "_subs", "")) %>%
         group_by(flag_iso3) %>%
         summarize(tot_subs = sum(subs, na.rm = T)) %>%
         left_join(global_fishers, by = c("flag_iso3" = "iso3")) %>%
         mutate(subs_per_fisher = tot_subs/fishers)
       
       global_average_subs_per_fisher <- mean(global_subs_per_fisher$subs_per_fisher, na.rm = T)
       
       percent_fishers_cap <- cap_tier$tier1_cap_best_percent_fishers/100
       
       # Get number of fishers from most recent year
       flag_fishers <- global_fishers %>%
         mutate(cap_value = global_average_subs_per_fisher*fishers*percent_fishers_cap) %>%
         dplyr::select(iso3, cap_value)
       
       flag_caps_fishers_tier1 <- cap_df %>%
         dplyr::filter(flag_iso3 %in% tier_1_flags_out) %>%
         left_join(flag_fishers, by = c("flag_iso3" = "iso3")) %>%
         mutate(new_cap_value = ifelse(subs_for_cap == 0, 0, cap_value)) %>%
         mutate(fishers_cap = new_cap_value*subs_for_cap_percent_tot) %>%
         dplyr::select(flag_iso3, type, fishers_cap)
       
       ### Combine
       flag_caps_best_tier1 <- flag_caps_subs_tier1 %>%
         left_join(flag_caps_landed_value_tier1, by = c("flag_iso3", "type")) %>%
         left_join(flag_caps_fishers_tier1, by = c("flag_iso3", "type")) %>%
         group_by(flag_iso3, type) %>%
         mutate(cap = max(subs_cap, landed_value_cap, fishers_cap, na.rm = T)) %>%
         ungroup() %>%
         dplyr::select(flag_iso3, type, cap)
       
       # test <- flag_caps_best_tier1 %>%
       #   mutate(which_is_best = case_when(cap == 0 ~ "N/A",
       #                                    cap == subs_cap ~ "subsidies",
       #                                    cap == landed_value_cap ~ "landed value",
       #                                    cap == fishers_cap ~ "fishers")) %>%
       #   mutate(which_is_best = ifelse(which_is_best != "N/A", which_is_best, NA)) %>%
       #   dplyr::filter(!is.na(which_is_best)) %>%
       #   group_by(flag_iso3) %>%
       #   summarize(best_approach = unique(which_is_best))
       # 
       # write_csv(test, here::here("results", "china_cap_proposal_best_approaches.csv"))
       
       flag_caps_tier1 <- cap_df %>%
         dplyr::filter(flag_iso3 %in% tier_1_flags_out) %>%
         left_join(flag_caps_best_tier1, by = c("flag_iso3", "type"))
     }
    
    # Out
    flag_caps_out <- flag_caps_tier1
    
    ### Tier 2 - needed regardless of whether there are two total, or three total tiers
    if(cap_tier$tier_number == "TWO" | cap_tier$tier_number == "THREE"){
      
      if(cap_tier$tier2_cap_rule == "PERCENT_SUBS"){
        
        percent_cap <- cap_tier$tier2_cap_percent/100
        
        flag_caps_tier2 <- cap_df %>%
          dplyr::filter(flag_iso3 %in% tier_2_flags_out) %>%
          mutate(cap = subs_for_cap*percent_cap)
        
      }else if(cap_tier$tier2_cap_rule == "VALUE"){
        
        absolute_cap <- cap_tier$tier2_cap_value*1e6
        
        flag_caps_tier2 <- cap_df %>%
          dplyr::filter(flag_iso3 %in% tier_2_flags_out) %>%
          mutate(cap = absolute_cap*subs_for_cap_percent_tot)
        
      }else if(cap_tier$tier2_cap_rule == "PERCENT_REV"){
        
        percent_cap <- cap_tier$tier2_cap_percent/100
        
        # Country revenue 
        revenue_years <- paste0("", seq(2016, 2018, by = 1), "")
        
        flag_revenue <- cap_tier_lookup %>%
          dplyr::filter(variable == "landed_value") %>%
          mutate(revenue = rowSums(select(., one_of(c(revenue_years))))) %>%
          dplyr::select(iso3, revenue) %>%
          mutate(cap_value = revenue*percent_cap) %>%
          dplyr::select(iso3, cap_value)
        
        flag_caps_tier2 <- cap_df %>%
          dplyr::filter(flag_iso3 %in% tier_2_flags_out) %>%
          left_join(flag_revenue, by = c("flag_iso3" = "iso3")) %>%
          mutate(new_cap_value = ifelse(subs_for_cap == 0, 0, cap_value)) %>%
          mutate(cap = new_cap_value*subs_for_cap_percent_tot) %>%
          dplyr::select(-cap_value, -new_cap_value)
        
      }else if(cap_tier$tier2_cap_rule == "FISHERS"){
        
        global_fishers <- cap_tier_lookup %>%
          dplyr::filter(variable == "fishers") %>%
          gather(year, value, -c("iso3", "variable", "is_WTO")) %>%
          group_by(iso3, variable, is_WTO) %>%
          dplyr::filter(year == max(year[!is.na(value)])) %>%
          ungroup() %>%
          dplyr::select(iso3, fishers = value)
        
        global_average_fishers <- mean(global_fishers$fishers, na.rm = T)
        
        global_subs_per_fisher <- cap_df %>%
          dplyr::filter(type %in% str_replace(subsidies_to_cap, "_subs", "")) %>%
          group_by(flag_iso3) %>%
          summarize(tot_subs = sum(subs, na.rm = T)) %>%
          left_join(global_fishers, by = c("flag_iso3" = "iso3")) %>%
          mutate(subs_per_fisher = tot_subs/fishers)
        
        global_average_subs_per_fisher <- mean(global_subs_per_fisher$subs_per_fisher, na.rm = T)
        
        percent_cap <- cap_tier$tier1_cap_percent/100
        
        # Get number of fishers from most recent year
        flag_fishers <- global_fishers %>%
          mutate(cap_value = global_average_subs_per_fisher*fishers*percent_cap) %>%
          dplyr::select(iso3, cap_value)
        
        flag_caps_tier2 <- cap_df %>%
          dplyr::filter(flag_iso3 %in% tier_2_flags_out) %>%
          left_join(flag_fishers, by = c("flag_iso3" = "iso3")) %>%
          mutate(new_cap_value = ifelse(subs_for_cap == 0, 0, cap_value)) %>%
          mutate(cap = new_cap_value*subs_for_cap_percent_tot) %>%
          dplyr::select(-cap_value, -new_cap_value)
        
      }else if(cap_tier$tier2_cap_rule == "BEST"){
        
        ### Do percent subs calculations
        percent_subs_cap <- cap_tier$tier2_cap_best_percent_subs/100
        
        flag_caps_subs_tier2 <- cap_df %>%
          dplyr::filter(flag_iso3 %in% tier_2_flags_out) %>%
          mutate(subs_cap = subs_for_cap*percent_subs_cap) %>%
          dplyr::select(flag_iso3, type, subs_cap)
        
        ### Do percent landed value calculation
        percent_landed_value_cap <- cap_tier$tier2_cap_best_percent_landed_value/100
        
        # Country revenue 
        revenue_years <- paste0("", seq(2016, 2018, by = 1), "")
        
        flag_revenue <- cap_tier_lookup %>%
          dplyr::filter(variable == "landed_value") %>%
          mutate(revenue = rowSums(select(., one_of(c(revenue_years))))) %>%
          dplyr::select(iso3, revenue) %>%
          mutate(cap_value = revenue*percent_landed_value_cap) %>%
          dplyr::select(iso3, cap_value)
        
        flag_caps_landed_value_tier2 <- cap_df %>%
          dplyr::filter(flag_iso3 %in% tier_2_flags_out) %>%
          left_join(flag_revenue, by = c("flag_iso3" = "iso3")) %>%
          mutate(new_cap_value = ifelse(subs_for_cap == 0, 0, cap_value)) %>%
          mutate(landed_value_cap = new_cap_value*subs_for_cap_percent_tot) %>%
          dplyr::select(flag_iso3, type, landed_value_cap)
        
        ### Do fishers calculation
        global_fishers <- cap_tier_lookup %>%
          dplyr::filter(variable == "fishers") %>%
          gather(year, value, -c("iso3", "variable", "is_WTO")) %>%
          group_by(iso3, variable, is_WTO) %>%
          dplyr::filter(year == max(year[!is.na(value)])) %>%
          ungroup() %>%
          dplyr::select(iso3, fishers = value)
        
        global_average_fishers <- mean(global_fishers$fishers, na.rm = T)
        
        global_subs_per_fisher <- cap_df %>%
          dplyr::filter(type %in% str_replace(subsidies_to_cap, "_subs", "")) %>%
          group_by(flag_iso3) %>%
          summarize(tot_subs = sum(subs, na.rm = T)) %>%
          left_join(global_fishers, by = c("flag_iso3" = "iso3")) %>%
          mutate(subs_per_fisher = tot_subs/fishers)
        
        global_average_subs_per_fisher <- mean(global_subs_per_fisher$subs_per_fisher, na.rm = T)
        
        percent_fishers_cap <- cap_tier$tier2_cap_best_percent_fishers/100
        
        # Get number of fishers from most recent year
        flag_fishers <- global_fishers %>%
          mutate(cap_value = global_average_subs_per_fisher*fishers*percent_fishers_cap) %>%
          dplyr::select(iso3, cap_value)
        
        flag_caps_fishers_tier2 <- cap_df %>%
          dplyr::filter(flag_iso3 %in% tier_2_flags_out) %>%
          left_join(flag_fishers, by = c("flag_iso3" = "iso3")) %>%
          mutate(new_cap_value = ifelse(subs_for_cap == 0, 0, cap_value)) %>%
          mutate(fishers_cap = new_cap_value*subs_for_cap_percent_tot) %>%
          dplyr::select(flag_iso3, type, fishers_cap)
        
        ### Combine
        flag_caps_best_tier2 <- flag_caps_subs_tier2 %>%
          left_join(flag_caps_landed_value_tier2, by = c("flag_iso3", "type")) %>%
          left_join(flag_caps_fishers_tier2, by = c("flag_iso3", "type")) %>%
          group_by(flag_iso3, type) %>%
          mutate(cap = max(subs_cap, landed_value_cap, fishers_cap, na.rm = T)) %>%
          ungroup() %>%
          dplyr::select(flag_iso3, type, cap)
        
        flag_caps_tier2 <- cap_df %>%
          dplyr::filter(flag_iso3 %in% tier_2_flags_out) %>%
          left_join(flag_caps_best_tier2, by = c("flag_iso3", "type"))
        
      }else if(cap_tier$tier2_cap_rule == "NONE"){
        
        flag_caps_tier2 <- cap_df %>%
          dplyr::filter(flag_iso3 %in% tier_2_flags_out) %>%
          mutate(cap = NA)
        
      }
      
      # Out
      flag_caps_out <- flag_caps_out %>%
        bind_rows(flag_caps_tier2)
      
    } # close tier 2
    
    ### Tier 3 - only needed if there are three total tiers
    if(cap_tier$tier_number == "THREE"){
      
      if(cap_tier$tier3_cap_rule == "PERCENT_SUBS"){
        
        percent_cap <- cap_tier$tier3_cap_percent/100
        
        flag_caps_tier3 <- cap_df %>%
          dplyr::filter(flag_iso3 %in% tier_3_flags_out) %>%
          mutate(cap = subs_for_cap*percent_cap)
        
      }else if(cap_tier$tier3_cap_rule == "VALUE"){
        
        absolute_cap <- cap_tier$tier3_cap_value*1e6
        
        flag_caps_tier3 <- cap_df %>%
          dplyr::filter(flag_iso3 %in% tier_3_flags_out) %>%
          mutate(cap = absolute_cap*subs_for_cap_percent_tot)
        
      }else if(cap_tier$tier3_cap_rule == "PERCENT_REV"){
        
        percent_cap <- cap_tier$tier3_cap_percent/100
        
        # Country revenue 
        revenue_years <- paste0("", seq(2016, 2018, by = 1), "")
        
        flag_revenue <- cap_tier_lookup %>%
          dplyr::filter(variable == "landed_value") %>%
          mutate(revenue = rowSums(select(., one_of(c(revenue_years))))) %>%
          dplyr::select(iso3, revenue) %>%
          mutate(cap_value = revenue*percent_cap) %>%
          dplyr::select(iso3, cap_value)
        
        flag_caps_tier3 <- cap_df %>%
          dplyr::filter(flag_iso3 %in% tier_3_flags_out) %>%
          left_join(flag_revenue, by = c("flag_iso3" = "iso3")) %>%
          mutate(new_cap_value = ifelse(subs_for_cap == 0, 0, cap_value)) %>%
          mutate(cap = new_cap_value*subs_for_cap_percent_tot) %>%
          dplyr::select(-cap_value, -new_cap_value)
        
      }else if(cap_tier$tier3_cap_rule == "FISHERS"){
        
        global_fishers <- cap_tier_lookup %>%
          dplyr::filter(variable == "fishers") %>%
          gather(year, value, -c("iso3", "variable", "is_WTO")) %>%
          group_by(iso3, variable, is_WTO) %>%
          dplyr::filter(year == max(year[!is.na(value)])) %>%
          ungroup() %>%
          dplyr::select(iso3, fishers = value)
        
        global_average_fishers <- mean(global_fishers$fishers, na.rm = T)
        
        global_subs_per_fisher <- cap_df %>%
          dplyr::filter(type %in% str_replace(subsidies_to_cap, "_subs", "")) %>%
          group_by(flag_iso3) %>%
          summarize(tot_subs = sum(subs, na.rm = T)) %>%
          left_join(global_fishers, by = c("flag_iso3" = "iso3")) %>%
          mutate(subs_per_fisher = tot_subs/fishers)
        
        global_average_subs_per_fisher <- mean(global_subs_per_fisher$subs_per_fisher, na.rm = T)
        
        percent_cap <- cap_tier$tier1_cap_percent/100
        
        # Get number of fishers from most recent year
        flag_fishers <- global_fishers %>%
          mutate(cap_value = global_average_subs_per_fisher*fishers*percent_cap) %>%
          dplyr::select(iso3, cap_value)
        
        flag_caps_tier3 <- cap_df %>%
          dplyr::filter(flag_iso3 %in% tier_3_flags_out) %>%
          left_join(flag_fishers, by = c("flag_iso3" = "iso3")) %>%
          mutate(new_cap_value = ifelse(subs_for_cap == 0, 0, cap_value)) %>%
          mutate(cap = new_cap_value*subs_for_cap_percent_tot) %>%
          dplyr::select(-cap_value, -new_cap_value)
        
      }else if(cap_tier$tier3_cap_rule == "BEST"){
        
        ### Do percent subs calculations
        percent_subs_cap <- cap_tier$tier3_cap_best_percent_subs/100
        
        flag_caps_subs_tier3 <- cap_df %>%
          dplyr::filter(flag_iso3 %in% tier_3_flags_out) %>%
          mutate(subs_cap = subs_for_cap*percent_subs_cap) %>%
          dplyr::select(flag_iso3, type, subs_cap)
        
        ### Do percent landed value calculation
        percent_landed_value_cap <- cap_tier$tier3_cap_best_percent_landed_value/100
        
        # Country revenue 
        revenue_years <- paste0("", seq(2016, 2018, by = 1), "")
        
        flag_revenue <- cap_tier_lookup %>%
          dplyr::filter(variable == "landed_value") %>%
          mutate(revenue = rowSums(select(., one_of(c(revenue_years))))) %>%
          dplyr::select(iso3, revenue) %>%
          mutate(cap_value = revenue*percent_landed_value_cap) %>%
          dplyr::select(iso3, cap_value)
        
        flag_caps_landed_value_tier3 <- cap_df %>%
          dplyr::filter(flag_iso3 %in% tier_3_flags_out) %>%
          left_join(flag_revenue, by = c("flag_iso3" = "iso3")) %>%
          mutate(new_cap_value = ifelse(subs_for_cap == 0, 0, cap_value)) %>%
          mutate(landed_value_cap = new_cap_value*subs_for_cap_percent_tot) %>%
          dplyr::select(flag_iso3, type, landed_value_cap)
        
        ### Do fishers calculation
        global_fishers <- cap_tier_lookup %>%
          dplyr::filter(variable == "fishers") %>%
          gather(year, value, -c("iso3", "variable", "is_WTO")) %>%
          group_by(iso3, variable, is_WTO) %>%
          dplyr::filter(year == max(year[!is.na(value)])) %>%
          ungroup() %>%
          dplyr::select(iso3, fishers = value)
        
        global_average_fishers <- mean(global_fishers$fishers, na.rm = T)
        
        global_subs_per_fisher <- cap_df %>%
          dplyr::filter(type %in% str_replace(subsidies_to_cap, "_subs", "")) %>%
          group_by(flag_iso3) %>%
          summarize(tot_subs = sum(subs, na.rm = T)) %>%
          left_join(global_fishers, by = c("flag_iso3" = "iso3")) %>%
          mutate(subs_per_fisher = tot_subs/fishers)
        
        global_average_subs_per_fisher <- mean(global_subs_per_fisher$subs_per_fisher, na.rm = T)
        
        percent_fishers_cap <- cap_tier$tier1_cap_best_percent_fishers/100
        
        # Get number of fishers from most recent year
        flag_fishers <- global_fishers %>%
          mutate(cap_value = global_average_subs_per_fisher*fishers*percent_fishers_cap) %>%
          dplyr::select(iso3, cap_value)
        
        flag_caps_fishers_tier3 <- cap_df %>%
          dplyr::filter(flag_iso3 %in% tier_3_flags_out) %>%
          left_join(flag_fishers, by = c("flag_iso3" = "iso3")) %>%
          mutate(new_cap_value = ifelse(subs_for_cap == 0, 0, cap_value)) %>%
          mutate(fishers_cap = new_cap_value*subs_for_cap_percent_tot) %>%
          dplyr::select(flag_iso3, type, fishers_cap)
        
        ### Combine
        flag_caps_best_tier3 <- flag_caps_subs_tier3 %>%
          left_join(flag_caps_landed_value_tier3, by = c("flag_iso3", "type")) %>%
          left_join(flag_caps_fishers_tier3, by = c("flag_iso3", "type")) %>%
          group_by(flag_iso3, type) %>%
          mutate(cap = max(subs_cap, landed_value_cap, fishers_cap, na.rm = T)) %>%
          ungroup() %>%
          dplyr::select(flag_iso3, type, cap)
        
        flag_caps_tier3 <- cap_df %>%
          dplyr::filter(flag_iso3 %in% tier_3_flags_out) %>%
          left_join(flag_caps_best_tier3, by = c("flag_iso3", "type"))
        
      }else if(cap_tier$tier3_cap_rule == "NONE"){
        
        flag_caps_tier3 <- cap_df %>%
          dplyr::filter(flag_iso3 %in% tier_3_flags_out) %>%
          mutate(cap = NA)
        
      }
      
      flag_caps_out <- flag_caps_out %>%
        bind_rows(flag_caps_tier3)
      
    } # close tier 3
    
    ### Calculate remainders, apply to vessel list and be done --------
      
    # After caps are set, calculate remaining subs, accounting for already removed subs
     flag_remainders <- flag_caps_out %>%
       arrange(flag_iso3, type) %>%
       mutate(subs_remainder = subs - subs_removed,
              overage = case_when(is.na(cap) ~ 0,
                                  (cap - subs_remainder < 0) ~ -(cap - subs_remainder),
                                  TRUE ~ 0),
              percent_allowed = case_when(subs_for_cap == 0 ~ 1,
                                          subs_remainder <= 0 ~ 0, 
                                          subs_remainder > 0 ~ (1 - (overage/subs_remainder)),
                                          TRUE ~ 0))
     
     # Get table of percent of existing subsidies allowed by type and flag
     flag_reduction_percents <- flag_remainders %>%
       dplyr::select(flag_iso3, type, percent_allowed) %>%
       rename(allowed = type) %>%
       spread(allowed, percent_allowed, sep = "_")
     
     # Create rows for EU states (and other relevant territories)
     flag_reduction_percents_eu <- vessel_subset %>%
       distinct(flag_iso3) %>%
       dplyr::filter(flag_iso3 %in% wto_members_and_observers & !(flag_iso3 %in% flag_reduction_percents$flag_iso3)) %>%
       mutate(iso3_for_matching = case_when(flag_iso3 %in% eu_countries ~ "EU")) %>%
       left_join(flag_reduction_percents, by = c("iso3_for_matching" = "flag_iso3")) %>%
       dplyr::select(-iso3_for_matching)
     
     # Combine
     flag_reduction_percents <- flag_reduction_percents %>%
       bind_rows(flag_reduction_percents_eu)
     
     # Join to vessels and calculate removed subs and allowed subs  
     cap_return_vessels <-  vessel_subset %>%
       dplyr::select(ssvid, region, fao_region, eez_id, is_territorial, flag_iso3, catch, revenue, fishing_hours_eez_fao_ter, fishing_KWh_eez_fao_ter, contains("_subs")) %>%
       left_join(flag_reduction_percents, by = c("flag_iso3")) %>%
       mutate(A1_subs_removed = A1_subs*(1-allowed_A1),
              A2_subs_removed = A2_subs*(1-allowed_A2),
              A3_subs_removed = A3_subs*(1-allowed_A3),
              B1_subs_removed = B1_subs*(1-allowed_B1),
              B2_subs_removed = B2_subs*(1-allowed_B2),
              B3_subs_removed = B3_subs*(1-allowed_B3),
              B4_subs_removed = B4_subs*(1-allowed_B4),
              B5_subs_removed = B5_subs*(1-allowed_B5),
              B6_subs_removed = B6_subs*(1-allowed_B6),
              B7_subs_removed = B7_subs*(1-allowed_B7),
              C1_subs_removed = C1_subs*(1-allowed_C1),
              C2_subs_removed = C2_subs*(1-allowed_C2),
              C3_subs_removed = C3_subs*(1-allowed_C3)) %>%
       dplyr::select(-contains("allowed_"))
     
     cap_return_vessels <- cap_return_vessels %>% 
       mutate(A1_subs_removed = ifelse(is.na(A1_subs_removed), 0, A1_subs_removed),
              A2_subs_removed = ifelse(is.na(A2_subs_removed), 0, A2_subs_removed),
              A3_subs_removed = ifelse(is.na(A3_subs_removed), 0, A3_subs_removed),
              B1_subs_removed = ifelse(is.na(B1_subs_removed), 0, B1_subs_removed),
              B2_subs_removed = ifelse(is.na(B2_subs_removed), 0, B2_subs_removed),
              B3_subs_removed = ifelse(is.na(B3_subs_removed), 0, B3_subs_removed),
              B4_subs_removed = ifelse(is.na(B4_subs_removed), 0, B4_subs_removed),
              B5_subs_removed = ifelse(is.na(B5_subs_removed), 0, B5_subs_removed),
              B6_subs_removed = ifelse(is.na(B6_subs_removed), 0, B6_subs_removed),
              B7_subs_removed = ifelse(is.na(B7_subs_removed), 0, B7_subs_removed),
              C1_subs_removed = ifelse(is.na(C1_subs_removed), 0, C1_subs_removed),
              C2_subs_removed = ifelse(is.na(C2_subs_removed), 0, C2_subs_removed),
              C3_subs_removed = ifelse(is.na(C3_subs_removed), 0, C3_subs_removed))
     
     # Join to other affected vessels and summarize
     return_vessels <- affected_vessels %>%
       dplyr::select(ssvid, region, fao_region, eez_id, is_territorial, flag_iso3, catch, revenue, fishing_hours_eez_fao_ter, fishing_KWh_eez_fao_ter, contains("subs")) %>%
       bind_rows(cap_return_vessels) %>%
       group_by(ssvid, region, fao_region, eez_id, is_territorial, flag_iso3) %>%
       summarize_all(max, na.rm = T) %>%
       ungroup()
     
  } # close requirement that there must be at least one type of subsidy available for capping

  ### NO CAP/TIER SYSTEM ---
  }else{
    
    return_vessels <- affected_vessels %>%
      dplyr::select(ssvid, region, fao_region, eez_id, is_territorial, flag_iso3, catch, revenue, fishing_hours_eez_fao_ter, fishing_KWh_eez_fao_ter, contains("subs"))
    
  }
 
  ### Section # 5 ---------------------------------------
  ### Combine affected -------------------------------------------
  ### ---------------------------------------------------
  
  ### Combine affected vessels
  affected_vessels <- vessel_subset %>%
    right_join(return_vessels %>% dplyr::select(ssvid, region, fao_region, eez_id, is_territorial, contains("_subs_removed")),
               by = c("ssvid", "region", "fao_region", "eez_id", "is_territorial")) %>%
    mutate(removed_subs = rowSums(select(., one_of(paste0(removed_sub_types, "_subs_removed"))))) %>%
    mutate(status = "Affected") %>%
    mutate(fleet = ifelse(fmi_best >= managed_threshold, "affected_managed", "affected_oa"))
  
  if(nrow(affected_vessels) == 0){
    affected_vessels <- affected_vessels %>%
      mutate(fleet = as.character(fleet))
  }
  
  ### ---------------------------------
  ### ---------------------------------
  ### UNAFFECTED ----------------------
  ### ---------------------------------
  ### ---------------------------------
  # Get unaffected vessels
  unaffected_vessels <- vessel_list %>%
    anti_join(affected_vessels, by = c("ssvid", "region", "fao_region", "eez_id", "is_territorial")) 
  
  # Create a blank df with columns we need
  blank_columns <- vessel_tracking_df %>%
    select(-ssvid, -region, -fao_region, -eez_id, -is_territorial) %>%
    rename(removed_subs = subs_removed)
  
  blank_df <- as.data.frame(matrix(ncol = length(colnames(blank_columns)), nrow = nrow(unaffected_vessels), 0))
  colnames(blank_df) <- colnames(blank_columns)
  
  # Add back in
  unaffected_vessels <- unaffected_vessels %>%
    bind_cols(blank_df) %>%
    mutate(status = "Unaffected", 
           fleet = ifelse(fmi_best >= managed_threshold, "unaffected_managed", "unaffected_oa"))
  
  ### ---------------------------------
  ### ---------------------------------
  ### SUMMARY -------------------------
  ### ---------------------------------
  ### ---------------------------------
  
  out_vessels <- affected_vessels %>%
    bind_rows(unaffected_vessels)
  
  out_summary <- out_vessels %>%
    group_by(region, fleet) %>%
    summarize(catch = sum(catch, na.rm = T),
              revenue = sum(revenue, na.rm = T),
              fishing_h = sum(fishing_hours_eez_fao_ter, na.rm = T),
              fishing_KWh = sum(fishing_KWh_eez_fao_ter, na.rm = T),
              bad_subs = sum(bad_subs, na.rm = T),
              good_subs = sum(good_subs, na.rm = T),
              ugly_subs = sum(ugly_subs, na.rm = T),
              vessels = n_distinct(ssvid),
              removed_subs = sum(removed_subs, na.rm = T))
  
  # return
    out <- list(vessels = out_vessels,
                summary = out_summary) 
    
  return(out)
  
}
