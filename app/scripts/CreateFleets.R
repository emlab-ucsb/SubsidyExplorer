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
                         cap_tier_dat,
                         profile_dat,
                         subsidy_types_all,
                         managed_threshold = 0.66,
                         wto_members_and_observers,
                         subsidy_types,
                         flag_summary_dat){
  
  ### ---------------------------------
  ### SETUP ---------------------------
  ### ---------------------------------
  
  ### Since we don't have this as a selectable option - we define domestic vessels are those that spend less than 1% of their time fishing outside of their own EEZ
  domestic_vessel_cutoff <- 0.01
  
  # Subsidy types removed if an IUU discipline is triggered
  iuu_subtypes_removed <- c("B1", "B2", "B3", "B4", "B5", "B6", "B7")
  
  # Subsidy types removed if an Overfished discipline is triggered
  oa_subtypes_removed <- c("B1", "B2", "B3", "B4", "B5", "B6", "B7")
  
  # SVE countries
  sves <- c("ATG", "BRB", "BLZ", "BOL", "CUB", "DMA", "DOM", "SLV", "ECU", "FJI", "GRD", "GTM", "HND", "JAM", "MRT", "NIC", "PAN", "PNG", "KNA", "LCA", "VCT", "WSM", "SYC", "LKA", "TON", "TTO", "BHS")
  
  # All subsidy types
  good_sub_types <- subsidy_types_all[1:3]
  bad_sub_types <- subsidy_types_all[4:10]
  ugly_sub_types <- subsidy_types_all[11:13]
  
  ### ---------------------------------
  ### ---------------------------------
  ### AFFECTED ------------------------
  ### ---------------------------------
  ### ---------------------------------

  ### Remove vessels with no bad subsidies or that do not below to a WTO Member or Observer state (they can't be affected) 
  vessel_subset <- vessel_list %>%
    dplyr::filter(bad_subs > 0 & is_WTO)
  
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
            dplyr::filter(flag_iso3 %in% countries | is_EU)
          
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
      #}else if("HS" %in% iuu$scope_select & !is.na(iuu$hs_cutoff)){
      #   
      #   # Filter for high seas vessels only
      #   iuu_vessels_scope <- iuu_vessels_scope %>%
      #     dplyr::filter(prop_fishing_KWh_high_seas >= (iuu$hs_cutoff/100))
      #   
      # }else if("DW" %in% iuu$scope_select){
      #   
      #   # Filter for distant water fishing only
      #   iuu_vessels_scope <- iuu_vessels_scope %>%
      #     dplyr::filter(distant_water)
      #   
      # }else if("OUT" %in% iuu$scope_select & !is.na(iuu$hs_cutoff)){
      #   
      #   # Filter for high seas OR distant water fishing only
      #   iuu_vessels_scope <- iuu_vessels_scope %>%
      #     dplyr::filter(distant_water | prop_fishing_KWh_high_seas >= (iuu$hs_cutoff/100))
      #   
      # }else if("DISPUTE" %in% iuu$scope_select){
      #   
      #   # Filter for disputed waters
      #   iuu_vessels_scope <- iuu_vessels_scope %>%
      #     dplyr::filter(is_disputed)
      #   
      # }else if("LENGTH" %in% iuu$scope_select & !is.na(iuu$length_cutoff)){
      #   
      #   # Filter by vessel length
      #   iuu_vessels_scope <- iuu_vessels_scope %>%
      #     dplyr::filter(length_m >= iuu$length_cutoff)
      #   
      # }else if("TONNAGE" %in% iuu$scope_select & !is.na(iuu$tonnage_cutoff)){
      #   
      #   # Filter by vessel tonnage
      #   iuu_vessels_scope <- iuu_vessels_scope %>%
      #     dplyr::filter(tonnage_gt >= iuu$tonnage_cutoff)
      #   
      # }else if("ENGINE" %in% iuu$scope_select & !is.na(iuu$engine_cutoff)){
      #   
      #   # Filter by vessel engine power
      #   iuu_vessels_scope <- iuu_vessels_scope %>%
      #     dplyr::filter(engine_power_kw >= iuu$engine_cutoff)
      #   
      # }
      
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
    
    ### Vessel list for scope (need additional characteristics) - WHY DOES THIS GIVE ME MORE ENTRIES THAN I STARTED WITH - DUPLICATING SOMEWHERE
    oa_vessels_scope <- oa_vessels %>%
      left_join(vessel_subset, by = c("ssvid", "region", "fao_region", "eez_id", "is_territorial"))
    
    # All affected vessels are within scope
    if(oa$scope == "ALL"){
      
      oa_vessels_scope <- oa_vessels_scope
      
    # Only those coming from the top 10 worst subsidizers are within scope
    }else if(oa$scope == "SUB"){
      
      # Get rankings by country in terms of total "bad subsidy" provision
      country_ranking_subsidies <- flag_summary_dat %>%
        group_by(flag_iso3) %>%
        summarize(bad_subs = unique(bad_subs)) %>%
        mutate(bad_subs_rank = dense_rank(desc(bad_subs))) %>%
        arrange(bad_subs_rank)
      
      # Find top 10 subsidizing Member states
      top_countries_subs <- country_ranking_subsidies$flag_iso3[country_ranking_subsidies$bad_subs_rank <= 10]
      
      # Deal with EU
      if("EU" %in% top_countries_subs){
        
        # Filter for top 10 subsidizing Member states
        oa_vessels_scope <- oa_vessels_scope %>%
          dplyr::filter(is_EU | flag_iso3 %in% top_countries_subs)
        
      }else{
        # Filter for top 10 subsidizing Member states
        oa_vessels_scope <- oa_vessels_scope %>%
          dplyr::filter(flag_iso3 %in% top_countries_subs)
      }
      
    # Only those with selected characteristics
    }else if(oa$scope == "SELECT"){
      
      
      if("MANUAL" %in% oa$scope_select){
        
        # Select only certain member countries 
        countries <- oa$scope_manual
        
        # Deal with EU
        if("EU" %in% countries){
          
          # Filter for selected Members
          oa_vessels_scope <- oa_vessels_scope %>%
            dplyr::filter(flag_iso3 %in% countries | is_EU)
          
        }else{
          
          # Filter for selected Members
          oa_vessels_scope <- oa_vessels_scope %>%
            dplyr::filter(flag_iso3 %in% countries)
          
        }
        
      }else if("EX_TER" %in% oa$scope_select){
        
        # Filter for only vessels fishing outside of territorial waters
        oa_vessels_scope <- oa_vessels_scope %>%
          dplyr::filter(!is_territorial)
        
      }else if("HS" %in% oa$scope_select & !is.na(oa$hs_cutoff)){
        
        # Filter for high seas vessels only
        oa_vessels_scope <- oa_vessels_scope %>%
          dplyr::filter(prop_fishing_KWh_high_seas >= (oa$hs_cutoff/100))
        
      }else if("DW" %in% oa$scope_select){
        
        # Filter for distant water fishing only
        oa_vessels_scope <- oa_vessels_scope %>%
          dplyr::filter(distant_water)
        
      }else if("OUT" %in% oa$scope_select & !is.na(oa$hs_cutoff)){
        
        # Filter for high seas OR distant water fishing only
        oa_vessels_scope <- oa_vessels_scope %>%
          dplyr::filter(distant_water | prop_fishing_KWh_high_seas >= (oa$hs_cutoff/100))
        
      }else if("DISPUTE" %in% oa$scope_select){
        
        # Filter for disputed waters
        oa_vessels_scope <- oa_vessels_scope %>%
          dplyr::filter(is_disputed)
        
      }else if("LENGTH" %in% oa$scope_select & !is.na(oa$length_cutoff)){
        
        # Filter by vessel length
        oa_vessels_scope <- oa_vessels_scope %>%
          dplyr::filter(length_m >= oa$length_cutoff)
        
      }else if("TONNAGE" %in% oa$scope_select & !is.na(oa$tonnage_cutoff)){
        
        # Filter by vessel tonnage
        oa_vessels_scope <- oa_vessels_scope %>%
          dplyr::filter(tonnage_gt >= oa$tonnage_cutoff)
        
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
    
    ### Vessel list for S&DT - ALSO DUPLICATING SOMETHING
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

  if(length(overcap$definitions) == 0){
    
    # Shortcut output if no disciplines are selected from this category
    overcap_vessel_subset <- vessel_tracking_df
    
  }else if(overcap$definitions == ""){
    
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
    colnames(oc_vessels)[5] <- paste0(overcap_subtypes_removed, "_subs_removed")
    
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
  
  ### Overcpacity - scope ---------
  
  # There must be at least one affected vessel in order to define scope
  if(nrow(overcap_vessels) >= 1){
    
    ### Vessel list for scope (need additional characteristics) - WHY DOES THIS GIVE ME MORE ENTRIES THAN I STARTED WITH - DUPLICATING SOMEWHERE
    overcap_vessels_scope <- overcap_vessels %>%
      left_join(vessel_subset, by = c("ssvid", "region", "fao_region", "eez_id", "is_territorial"))
    
    # All affected vessels are within scope
    if(overcap$scope == "ALL"){
      
      overcap_vessels_scope <- overcap_vessels_scope
      
      # Only those coming from the top 10 worst subsidizers are within scope
    }else if(overcap$scope == "SUB"){
      
      # Get rankings by country in terms of total "bad subsidy" provision
      country_ranking_subsidies <- flag_summary_dat %>%
        group_by(flag_iso3) %>%
        summarize(bad_subs = unique(bad_subs)) %>%
        mutate(bad_subs_rank = dense_rank(desc(bad_subs))) %>%
        arrange(bad_subs_rank)
      
      # Find top 10 subsidizing Member states
      top_countries_subs <- country_ranking_subsidies$flag_iso3[country_ranking_subsidies$bad_subs_rank <= 10]
      
      # Deal with EU
      if("EU" %in% top_countries_subs){
        
        # Filter for top 10 subsidizing Member states
        overcap_vessels_scope <- overcap_vessels_scope %>%
          dplyr::filter(is_EU | flag_iso3 %in% top_countries_subs)
        
      }else{
        # Filter for top 10 subsidizing Member states
        overcap_vessels_scope <- overcap_vessels_scope %>%
          dplyr::filter(flag_iso3 %in% top_countries_subs)
      }
      
      # Only those with selected characteristics
    }else if(overcap$scope == "SELECT"){
      
      
      if("MANUAL" %in% overcap$scope_select){
        
        # Select only certain member countries 
        countries <- overcap$scope_manual
        
        # Deal with EU
        if("EU" %in% countries){
          
          # Filter for selected Members
          overcap_vessels_scope <- overcap_vessels_scope %>%
            dplyr::filter(flag_iso3 %in% countries | is_EU)
          
        }else{
          
          # Filter for selected Members
          overcap_vessels_scope <- overcap_vessels_scope %>%
            dplyr::filter(flag_iso3 %in% countries)
          
        }
        
      }else if("EX_TER" %in% overcap$scope_select){
        
        # Filter for only vessels fishing outside of territorial waters
        overcap_vessels_scope <- overcap_vessels_scope %>%
          dplyr::filter(!is_territorial)
        
      }else if("HS" %in% overcap$scope_select & !is.na(overcap$hs_cutoff)){
        
        # Filter for high seas vessels only
        overcap_vessels_scope <- overcap_vessels_scope %>%
          dplyr::filter(prop_fishing_KWh_high_seas >= (overcap$hs_cutoff/100))
        
      }else if("DW" %in% overcap$scope_select){
        
        # Filter for distant water fishing only
        overcap_vessels_scope <- overcap_vessels_scope %>%
          dplyr::filter(distant_water)
        
      }else if("OUT" %in% overcap$scope_select & !is.na(overcap$hs_cutoff)){
        
        # Filter for high seas OR distant water fishing only
        overcap_vessels_scope <- overcap_vessels_scope %>%
          dplyr::filter(distant_water | prop_fishing_KWh_high_seas >= (overcap$hs_cutoff/100))
        
      }else if("DISPUTE" %in% overcap$scope_select){
        
        # Filter for disputed waters
        overcap_vessels_scope <- overcap_vessels_scope %>%
          dplyr::filter(is_disputed)
        
      }else if("LENGTH" %in% overcap$scope_select & !is.na(overcap$length_cutoff)){
        
        # Filter by vessel length
        overcap_vessels_scope <- overcap_vessels_scope %>%
          dplyr::filter(length_m >= overcap$length_cutoff)
        
      }else if("TONNAGE" %in% overcap$scope_select & !is.na(overcap$tonnage_cutoff)){
        
        # Filter by vessel tonnage
        overcap_vessels_scope <- overcap_vessels_scope %>%
          dplyr::filter(tonnage_gt >= overcap$tonnage_cutoff)
        
      }else if("ENGINE" %in% overcap$scope_select & !is.na(overcap$engine_cutoff)){
        
        # Filter by vessel engine power
        overcap_vessels_scope <- overcap_vessels_scope %>%
          dplyr::filter(engine_power_kw >= overcap$engine_cutoff)
        
      }
      
    } # /SELECT
    
    # OA vessels in scope
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
      
    }else if("TIME" %in% overcap$sdt_what_ldc){
      
      #################
      # XXX: NEED TO DO 
      #################
      
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
      overcap_vessels_sdt_ldc <- overcap_vessels_sdt_ldc %>%
        dplyr::filter(is_territorial)
      
    }else if("HS" %in% overcap$sdt_what_developing){
      
      # Exempt vessels fishing on the high seas
      overcap_vessels_sdt_developing <- overcap_vessels_sdt_developing %>%
        dplyr::filter(prop_fishing_KWh_high_seas >= (overcap$sdt_hs_cutoff_developing/100))
      
    }else if("TIME" %in% overcap$sdt_what_developing){
      
      #################
      # XXX: NEED TO DO 
      #################
      
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
      
    }else if("TIME" %in% overcap$sdt_what_sve){
      
      #################
      # XXX: NEED TO DO 
      #################
      
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
      dplyr::select(region, ssvid, flag_iso3, is_territorial, eez_id, fao_region, catch, revenue, fishing_hours_eez_fao_ter, fishing_KWh, contains("subs"))
    
  }else{
    
    ### Create tidy data frame to track caps, already removed subsidies, and allowed subsidies
    # Get total subsidy amount by flag and subtype
    subs_total <- flag_summary_dat %>%
      dplyr::select(flag_iso3, paste0(subsidy_types_all, "_subs")) %>%
      gather(type, subs, -1) %>%
      mutate(type = str_replace(type, "_subs", "")) %>%
      group_by(flag_iso3)
    
    # Get base subsidy amounts by flag and subtype that will be included in the cap
    subs_for_capping <- flag_summary_dat %>%
      dplyr::select(flag_iso3, one_of(subsidies_to_cap)) %>%
      gather(type, subs_for_cap, -1) %>%
      mutate(type = str_replace(type, "_subs", "")) %>%
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
    
    
   ### DETERMINE TIERING -----------
    
   ### One Tier
   if(cap_tier$tier_number == "ONE"){
     
     tier_1_flags_out <- wto_members_and_observers 

   ### Two Tiers     
   }else if(cap_tier$tier_number == "TWO"){
     
     # Establish who is in which tier
     if(cap_tier$tier_system == "CAPTURE"){
       
       percent_cutoff <- cap_tier$two_tier_cutoff/100
       
       cap_tier_dat_sorted <- cap_tier_dat %>%
         mutate(new_tier = case_when(percent_global_capture >= percent_cutoff ~ 1, 
                                     percent_global_capture < percent_cutoff ~ 2,
                                     is.na(percent_global_capture) ~ 2))
       
     }else if(cap_tier$tier_system == "EXPORTS"){
       
       percent_cutoff <- cap_tier$two_tier_cutoff/100
       
       cap_tier_dat_sorted <- cap_tier_dat %>%
         mutate(new_tier = case_when(percent_exports >= percent_cutoff ~ 1, 
                                     percent_exports < percent_cutoff ~ 2,
                                     is.na(percent_exports) ~ 2))
       
     }else if(cap_tier$tier_system == "SUBS"){
       
       percent_cutoff <- cap_tier$two_tier_cutoff/100
       
       cap_tier_dat_sorted <- cap_tier_dat %>%
         left_join(flag_summary %>% dplyr::select(flag_iso3, percent_bad_subs), by = c("iso3" = "flag_iso3")) %>%
         mutate(new_tier = case_when(percent_bad_subs >= percent_cutoff ~ 1, 
                                     percent_bad_subs < percent_cutoff ~ 2,
                                     is.na(percent_bad_subs) ~ 2))
       
     }else if(cap_tier$tier_system == "DEVELOPMENT"){
       
       cap_tier_dat_sorted <- cap_tier_dat %>%
         left_join(flag_summary %>% dplyr::select(flag, development_status), by = c("iso3" = "flag")) %>%
         mutate(new_tier = case_when(development_status == "Developed" ~ 1, 
                                     development_status == "Developing" ~ 2,
                                     development_status == "LDC" ~ 2,
                                     is.na(development_status) ~ 2))
     }
     
     tier_1_flags <- unique(cap_tier_dat_sorted$iso3[cap_tier_dat_sorted$new_tier == 1])
     # if("EU" %in% tier_1_flags){
     #   tier_1_flags_out <- c(tier_1_flags, eu_countries, eu_territories)
     # }else if("USA" %in% tier_1_flags){
     #   tier_1_flags_out <- c(tier_1_flags, us_territories)
     # }else if("EU" %in% tier_1_flags &"USA" %in% tier_1_flags){
     #   tier_1_flags_out <- c(tier_1_flags, eu_countries, eu_territories, us_territories)
     # }else{
       tier_1_flags_out <- tier_1_flags
     #}
     
     tier_2_flags <- unique(cap_tier_dat_sorted$iso3[cap_tier_dat_sorted$new_tier == 2])
     # if("EU" %in% tier_2_flags){
     #   tier_2_flags_out <- c(tier_2_flags, eu_countries, eu_territories)
     # }else if("USA" %in% tier_2_flags){
     #   tier_2_flags_out <- c(tier_2_flags, us_territories)
     # }else if("EU" %in% tier_2_flags &"USA" %in% tier_2_flags){
     #   tier_2_flags_out <- c(tier_2_flags, eu_countries, eu_territories, us_territories)
     # }else{
       tier_2_flags_out <- tier_2_flags
     #}
     
   ### Three tiers    
   }else if(cap_tier$tier_number == "THREE"){
     
     # Establish who is in which tier
     if(cap_tier$tier_system == "CAPTURE"){
       
       percent_cutoff_top <- cap_tier$three_tier_cutoff[2]/100
       percent_cutoff_bottom <- cap_tier$three_tier_cutoff[1]/100
       
       cap_tier_dat_sorted <- cap_tier_dat %>%
         mutate(new_tier = case_when(percent_global_capture >= percent_cutoff_top ~ 1, 
                                     (percent_global_capture < percent_cutoff_top & percent_global_capture >= percent_cutoff_bottom) ~ 2,
                                     percent_global_capture < percent_cutoff_bottom ~ 3,
                                     is.na(percent_global_capture) ~ 3))
       
     }else if(cap_tier$tier_system == "EXPORTS"){
       
       percent_cutoff_top <- cap_tier$three_tier_cutoff[2]/100
       percent_cutoff_bottom <- cap_tier$three_tier_cutoff[1]/100
       
       cap_tier_dat_sorted <- cap_tier_dat %>%
         mutate(new_tier = case_when(percent_exports >= percent_cutoff_top ~ 1, 
                                     (percent_exports < percent_cutoff_top & percent_exports >= percent_cutoff_bottom) ~ 2,
                                     percent_exports < percent_cutoff_bottom ~ 3,
                                     is.na(percent_exports) ~ 3))
       
     }else if(cap_tier$tier_system == "SUBS"){
       
       percent_cutoff_top <- cap_tier$three_tier_cutoff[2]/100
       percent_cutoff_bottom <- cap_tier$three_tier_cutoff[1]/100
       
       cap_tier_dat_sorted <- cap_tier_dat %>%
         left_join(flag_summary_dat %>% dplyr::select(flag_iso3, percent_bad_subs), by = c("iso3" = "flag_iso3")) %>%
         mutate(new_tier = case_when(percent_bad_subs >= percent_cutoff_top ~ 1, 
                                     (percent_bad_subs < percent_cutoff_top & percent_exports >= percent_cutoff_bottom) ~ 2,
                                     percent_bad_subs < percent_cutoff_bottom ~ 3,
                                     is.na(percent_bad_subs) ~ 3))
       
     }else if(cap_tier$tier_system == "DEVELOPMENT"){
       
       cap_tier_dat_sorted <- cap_tier_dat %>%
         left_join(flag_summary_dat %>% dplyr::select(flag_iso3, development_status), by = c("iso3" = "flag_iso3")) %>%
         mutate(new_tier = case_when(development_status == "Developed" ~ 1, 
                                     development_status == "Developing" ~ 2,
                                     development_status == "LDC" ~ 3,
                                     is.na(development_status) ~ 3))
     }
     
     tier_1_flags <- unique(cap_tier_dat_sorted$iso3[cap_tier_dat_sorted$new_tier == 1])
     # if("EU" %in% tier_1_flags){
     #   tier_1_flags_out <- c(tier_1_flags, eu_countries, eu_territories)
     # }else if("USA" %in% tier_1_flags){
     #   tier_1_flags_out <- c(tier_1_flags, us_territories)
     # }else if("EU" %in% tier_1_flags &"USA" %in% tier_1_flags){
     #   tier_1_flags_out <- c(tier_1_flags, eu_countries, eu_territories, us_territories)
     # }else{
       tier_1_flags_out <- tier_1_flags
     #}
     
     tier_2_flags <- unique(cap_tier_dat_sorted$iso3[cap_tier_dat_sorted$new_tier == 2])
     # if("EU" %in% tier_2_flags){
     #   tier_2_flags_out <- c(tier_2_flags, eu_countries, eu_territories)
     # }else if("USA" %in% tier_2_flags){
     #   tier_2_flags_out <- c(tier_2_flags, us_territories)
     # }else if("EU" %in% tier_2_flags &"USA" %in% tier_2_flags){
     #   tier_2_flags_out <- c(tier_2_flags, eu_countries, eu_territories, us_territories)
     # }else{
       tier_2_flags_out <- tier_2_flags
     #}
     
     tier_3_flags <- unique(cap_tier_dat_sorted$iso3[cap_tier_dat_sorted$new_tier == 3])
     # if("EU" %in% tier_3_flags){
     #   tier_3_flags_out <- c(tier_3_flags, eu_countries, eu_territories)
     # }else if("USA" %in% tier_3_flags){
     #   tier_3_flags_out <- c(tier_3_flags, us_territories)
     # }else if("EU" %in% tier_3_flags &"USA" %in% tier_3_flags){
     #   tier_3_flags_out <- c(tier_3_flags, eu_countries, eu_territories, us_territories)
     # }else{
       tier_3_flags_out <- tier_3_flags
     #}
     
   } # close tiering system
    

   ### DETERMINE CAPS ----------
    
   ### Tier 1 - ALWAYS  
   if(cap_tier$tier1_cap_rule == "PERCENT_SUBS"){
       
       percent_cap <- cap_tier$tier1_cap_percent/100
       
       flag_caps_tier1 <- cap_df %>%
         dplyr::filter(flag %in% tier_1_flags_out) %>%
         mutate(cap = subs_for_cap*percent_cap)
       
    }else if(cap_tier$tier1_cap_rule == "VALUE"){
       
       absolute_cap <- cap_tier$tier1_cap_value*1e6
       
       flag_caps_tier1 <- cap_df %>%
         dplyr::filter(flag %in% tier_1_flags_out) %>%
         mutate(cap = absolute_cap*subs_for_cap_percent_tot)
       
    }else if(cap_tier$tier1_cap_rule == "PERCENT_REVENUE"){
       
       percent_cap <- cap_tier$tier1_cap_percent/100
       
       flag_revenue <- flag_summary_dat %>%
         dplyr::select(flag_iso3, revenue) %>%
         mutate(cap_value = revenue*percent_cap) 
       
       flag_caps_tier1 <- cap_df %>%
         dplyr::filter(flag %in% tier_1_flags_out) %>%
         left_join(flag_revenue, by = c("flag" = "flag_iso3")) %>%
         mutate(new_cap_value = ifelse(subs_for_cap == 0, 0, cap_value)) %>%
         mutate(cap = new_cap_value*subs_for_cap_percent_tot) %>%
         dplyr::select(-revenue, -cap_value, -new_cap_value)
       
     }else if(cap_tier$tier1_cap_rule == "FISHERS"){
       
       money_per_fisher <- cap_tier$tier1_cap_fishers
       
       flag_fishers <- flag_summary_dat %>%
         dplyr::select(flag_iso3, fishers) %>%
         mutate(cap_value = fishers*money_per_fisher)
       
       flag_caps_tier1 <- cap_df %>%
         dplyr::filter(flag %in% tier_1_flags_out) %>%
         left_join(flag_fishers, by = c("flag" = "flag_iso3")) %>%
         mutate(new_cap_value = ifelse(subs_for_cap == 0, 0, cap_value)) %>%
         mutate(cap = new_cap_value*subs_for_cap_percent_tot) %>%
         dplyr::select(-fishers, -cap_value, -new_cap_value)
       
     }
    
    # Out
    flag_caps_out <- flag_caps_tier1
    
    ### Tier 2 - needed regardless of whether there are two total, or three total tiers
    if(cap_tier$tier_number == "TWO" | cap_tier$tier_number == "THREE"){
      
      if(cap_tier$tier2_cap_rule == "PERCENT_SUBS"){
        
        percent_cap <- cap_tier$tier2_cap_percent/100
        
        flag_caps_tier2 <- cap_df %>%
          dplyr::filter(flag %in% tier_2_flags_out) %>%
          mutate(cap = subs_for_cap*percent_cap)
        
      }else if(cap_tier$tier2_cap_rule == "VALUE"){
        
        absolute_cap <- cap_tier$tier2_cap_value*1e6
        
        flag_caps_tier2 <- cap_df %>%
          dplyr::filter(flag %in% tier_2_flags_out) %>%
          mutate(cap = absolute_cap*subs_for_cap_percent_tot)
        
      }else if(cap_tier$tier2_cap_rule == "PERCENT_REVENUE"){
        
        percent_cap <- cap_tier$tier2_cap_percent/100
        
        flag_revenue <- flag_summary_dat %>%
          dplyr::select(flag_iso3, revenue) %>%
          mutate(cap_value = revenue*percent_cap) 
        
        flag_caps_tier2 <- cap_df %>%
          dplyr::filter(flag %in% tier_2_flags_out) %>%
          left_join(flag_revenue, by = c("flag" = "flag_iso3")) %>%
          mutate(new_cap_value = ifelse(subs_for_cap == 0, 0, cap_value)) %>%
          mutate(cap = new_cap_value*subs_for_cap_percent_tot) %>%
          dplyr::select(-revenue, -cap_value, -new_cap_value)
        
      }else if(cap_tier$tier2_cap_rule == "FISHERS"){
        
        money_per_fisher <- cap_tier$tier2_cap_fishers
        
        flag_fishers <- flag_summary_dat %>%
          dplyr::select(flag_iso3, fishers) %>%
          mutate(cap_value = fishers*money_per_fisher)
        
        flag_caps_tier2 <- cap_df %>%
          dplyr::filter(flag %in% tier_1_flags_out) %>%
          left_join(flag_fishers, by = c("flag" = "flag_iso3")) %>%
          mutate(new_cap_value = ifelse(subs_for_cap == 0, 0, cap_value)) %>%
          mutate(cap = new_cap_value*subs_for_cap_percent_tot) %>%
          dplyr::select(-fishers, -cap_value, -new_cap_value)
        
      }else if(cap_tier$tier2_cap_rule == "NONE"){
        
        flag_caps_tier2 <- cap_df %>%
          dplyr::filter(flag %in% tier_2_flags_out) %>%
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
          dplyr::filter(flag %in% tier_3_flags_out) %>%
          mutate(cap = subs_for_cap*percent_cap)
        
      }else if(cap_tier$tier3_cap_rule == "VALUE"){
        
        absolute_cap <- cap_tier$tier3_cap_value*1e6
        
        flag_caps_tier3 <- cap_df %>%
          dplyr::filter(flag %in% tier_3_flags_out) %>%
          mutate(cap = absolute_cap*subs_for_cap_percent_tot)
        
      }else if(cap_tier$tier3_cap_rule == "PERCENT_REVENUE"){
        
        percent_cap <- cap_tier$tier3_cap_percent/100
        
        flag_revenue <- flag_summary_dat %>%
          dplyr::select(flag_iso3, revenue) %>%
          mutate(cap_value = revenue*percent_cap) 
        
        flag_caps_tier3 <- cap_df %>%
          dplyr::filter(flag %in% tier_3_flags_out) %>%
          left_join(flag_revenue, by = c("flag" = "flag_iso3")) %>%
          mutate(new_cap_value = ifelse(subs_for_cap == 0, 0, cap_value)) %>%
          mutate(cap = new_cap_value*subs_for_cap_percent_tot) %>%
          dplyr::select(-revenue, -cap_value, -new_cap_value)
        
      }else if(cap_tier$tier2_cap_rule == "FISHERS"){
        
        money_per_fisher <- cap_tier$tier3_cap_fishers
        
        flag_fishers <- flag_summary_dat %>%
          dplyr::select(flag_iso3, fishers) %>%
          mutate(cap_value = fishers*money_per_fisher)
        
        flag_caps_tier3 <- cap_df %>%
          dplyr::filter(flag %in% tier_3_flags_out) %>%
          left_join(flag_fishers, by = c("flag" = "flag_iso3")) %>%
          mutate(new_cap_value = ifelse(subs_for_cap == 0, 0, cap_value)) %>%
          mutate(cap = new_cap_value*subs_for_cap_percent_tot) %>%
          dplyr::select(-fishers, -cap_value, -new_cap_value)
        
      }else if(cap_tier$tier3_cap_rule == "NONE"){
        
        flag_caps_tier3 <- cap_df %>%
          dplyr::filter(flag %in% tier_3_flags_out) %>%
          mutate(cap = NA)
        
      }
      
      flag_caps_out <- flag_caps_out %>%
        bind_rows(flag_caps_tier3)
      
    } # close tier 3
    
    ### Calculate remainders, apply to vessel list and be done --------
      
    # After caps are set, calculate remaining subs, accounting for already removed subs
     flag_remainders <- flag_caps_out %>%
       arrange(flag, type) %>%
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
       dplyr::select(flag, type, percent_allowed) %>%
       rename(allowed = type) %>%
       spread(allowed, percent_allowed, sep = "_")
       
     # Join to vessels and calculate removed subs and allowed subs  
     cap_return_vessels <-  vessel_subset %>%
       dplyr::select(region, ssvid, eez_id, fao_region, flag, catch, revenue, fishing_hours_eez_fao_ter, fishing_KWh_eez_fao_ter, contains("_subs")) %>%
       left_join(flag_reduction_percents, by = c("flag")) %>%
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
       dplyr::select(region, ssvid, flag, eez_id, fao_region, catch, revenue, fishing_hours_eez_fao_ter, fishing_KWh_eez_fao_ter, contains("subs")) %>%
       bind_rows(cap_return_vessels) %>%
       group_by(region, ssvid, flag, eez_id, fao_region) %>%
       summarize_all(max, na.rm = T) %>%
       ungroup()
     
  } # close requirement that there must be at least one type of subsidy available for capping

  ### NO CAP/TIER SYSTEM ---
  }else{
    
    return_vessels <- affected_vessels %>%
      dplyr::select(region, ssvid, is_territorial, flag_iso3, eez_id, fao_region, catch, revenue, fishing_hours_eez_fao_ter, fishing_KWh_eez_fao_ter, contains("subs"))
    
  }
 
  ### Section # 5 ---------------------------------------
  ### Combine affected -------------------------------------------
  ### ---------------------------------------------------
  
  affected_vessels <- vessel_subset %>%
    right_join(return_vessels %>%
               dplyr::select(ssvid, region, fao_region, eez_id, is_territorial, contains("_subs_removed")),
               by = c("ssvid", "region", "fao_region", "eez_id", "is_territorial")) %>%
    mutate(removed_subs = rowSums(select(., one_of(paste0(bad_sub_types, "_subs_removed"))))) %>%
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
