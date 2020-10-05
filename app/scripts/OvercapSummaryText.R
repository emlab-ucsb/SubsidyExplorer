###---------------------------------
### OCOF reactive policy summary
### --------------------------------

OvercapSummaryText <- function(overcap,
                               cap_tier,
                               wid,
                               text,
                               country_choices){
  
  ### OCOF title
  overcap_header <- paste0("<b class = 'big'>", "Overcapacity and Overfishing", "</b>", "</br>")
  
  ### Nothing selected ---
  if(length(overcap$definitions) == 0){
    
    overcap_pro <- "None selected."
    overcap_disciplines <- paste0("<small class = 'gray'>", "DISCIPLINES:  ", "</small>",
                              "<small>", overcap_pro, "</small>", "<br>")
    
    overcap_scope <- ""
    
    overcap_sdt <- ""
    
    # Output for nothing selected
    overcap_out <- paste0(overcap_header,
                      overcap_disciplines,
                      overcap_scope,
                      overcap_sdt)
    
  ### At least one definition selected ---
  }else if(length(overcap$definitions) >= 1){
    
    overcap_pro_names <- subsidy_types_sorted_sumaila[4:13]
    overcap_pro_selected <- names(overcap_pro_names[overcap_pro_names %in% overcap$definitions])
    
    overcap_pro <- paste0("<small>",text$item_label[text$item_id == "w_overcap_definitions"], 
                      "<ul><li>", 
                      paste0(overcap_pro_selected, collapse = "</li><li>"), 
                      "</li></ul></small>")
    
    overcap_disciplines <- paste0("<small class = 'gray'>", "DISCIPLINES:  ", "</small>",
                              overcap_pro)
    
    ### Scope (at least one definition selected) ---
    if(overcap$scope == "ALL"){
      
      overcap_scope <- paste0("<small class = 'gray'>", "SCOPE:  ", "</small>",
                          "<small>", "Disciplines apply to all Members & vessels.", "</br></small>")
      
    }else if(overcap$scope == "SELECT"){
      
      overcap_scope_names <- unlist(wid$choices[wid$item_id == "w_overcap_scope_select"])
      
      overcap_scope <- paste0("<small class = 'gray'>", "SCOPE:  ", "</small>",
                          "<small>", "Disciplines only apply to vessels: ", "<ul>")
      
      if(length(overcap$scope_select) >= 1){
        
        # SUB: Top 10 subsidizers
        if("SUB" %in% overcap$scope_select){
          overcap_scope_sub <- paste0("<li>", names(overcap_scope_names[overcap_scope_names == "SUB"]), "</li>")
        }else{
          overcap_scope_sub <- ""
        }
        
        # MANUAL: Manual country selection
        if("MANUAL" %in% overcap$scope_select & length(overcap$scope_manual) > 0){
          overcap_country_names <- names(country_choices[country_choices %in% overcap$scope_manual])
          overcap_scope_manual <- paste0("<li>", names(overcap_scope_names[overcap_scope_names == "MANUAL"]),
                                    " (", paste0(overcap_country_names, collapse = ", "), ")", "</li>")
        }else{
          overcap_scope_manual <- ""
        }
        
        # EX_TER: Outside of territorial waters
        if("EX_TER" %in% overcap$scope_select){
          overcap_scope_ex_ter <- paste0("<li>", names(overcap_scope_names[overcap_scope_names == "EX_TER"]), "</li>")
        }else{
          overcap_scope_ex_ter <- ""
        }
        
        # HS: Fishing in ABNJ
        if("HS" %in% overcap$scope_select){
          overcap_scope_hs <- paste0("<li>", names(overcap_scope_names[overcap_scope_names == "HS"]), " (min. of ", overcap$hs_cutoff, "% of annual effort)", "</li>")
        }else{
          overcap_scope_hs <- ""
        }
        
        # DW: Fishing in other countries' EEZs
        if("DW" %in% overcap$scope_select){
          overcap_scope_dw <- paste0("<li>", names(overcap_scope_names[overcap_scope_names == "DW"]), "</li>")
        }else{
          overcap_scope_dw <- ""
        }
        
        # OUT: Fishing in ABNJ or in other countries' EEZs
        if("OUT" %in% overcap$scope_select){
          overcap_scope_out <- paste0("<li>", names(overcap_scope_names[overcap_scope_names == "OUT"]), " (min. of ", overcap$hs_cutoff, "% of annual effort)", "</li>")
        }else{
          overcap_scope_out <- ""
        }
        
        # DISPUTE: Fishing in disputed areas
        if("DISPUTE" %in% overcap$scope_select){
          overcap_scope_dispute <- paste0("<li>", names(overcap_scope_names[overcap_scope_names == "DISPUTE"]), "</li>")
        }else{
          overcap_scope_dispute <- ""
        }
        
        # LENGTH: Minimum vessel length
        if("LENGTH" %in% overcap$scope_select){
          overcap_scope_length <- paste0("<li>", names(overcap_scope_names[overcap_scope_names == "LENGTH"]), " (", overcap$length_cutoff, "m)", "</li>")
        }else{
          overcap_scope_length <- ""
        }
        
        # TONNAGE: Fishing in disputed areas
        if("TONNAGE" %in% overcap$scope_select){
          overcap_scope_tonnage <- paste0("<li>", names(overcap_scope_names[overcap_scope_names == "TONNAGE"]), " (", overcap$tonnage_cutoff, "gt)", "</li>")
        }else{
          overcap_scope_tonnage <- ""
        }
        
        # ENGINE: Fishing in disputed areas
        if("ENGINE" %in% overcap$scope_select){
          overcap_scope_engine <- paste0("<li>", names(overcap_scope_names[overcap_scope_names == "ENGINE"]), " (", overcap$engine_cutoff, "kW)", "</li>")
        }else{
          overcap_scope_engine <- ""
        }
      
      # Scope output
      overcap_scope <- paste0(overcap_scope,
                         overcap_scope_sub,
                         overcap_scope_manual,
                         overcap_scope_ex_ter,
                         overcap_scope_hs,
                         overcap_scope_dw,
                         overcap_scope_out,
                         overcap_scope_dispute,
                         overcap_scope_length,
                         overcap_scope_tonnage,
                         overcap_scope_engine,
                         "</ul></small>")
      
      }else if(length(overcap$scope_select) == 0){
        
        overcap_scope <- paste0(overcap_scope,
                           "</ul></small>")
        
      }
      
    } # /scope == select
      
    ### S&DT (at least one definition selected) ---
    if(overcap$allow_sdt == "NO"){
      
      overcap_sdt <- "None."
      overcap_sdt <- paste0("<small class = 'gray'>", "S&DT: ", "</small>",
                        "<small>", overcap_sdt, "</br></small>")
      
    }else if(overcap$allow_sdt == "YES"){
      
      # LDC S&DT
      if(overcap$sdt_ldc == "YES"){
        
        overcap_sdt_ldc_names <- unlist(wid$choices[wid$item_id == "w_overcap_sdt_what_ldc"])
        overcap_sdt_ldc_selected <- paste0(names(overcap_sdt_ldc_names[overcap_sdt_ldc_names %in% overcap$sdt_what_ldc]), collapse = ", ")
        overcap_sdt_ldc <- paste0("<li>", "LDCs: ", overcap_sdt_ldc_selected, "</li>")
        
      }else{
        overcap_sdt_ldc <- paste0("<li>", "LDCs: None.","</li>")
      }
      
      # Developing S&DT
      if(overcap$sdt_developing == "YES"){
        
        overcap_sdt_developing_names <- unlist(wid$choices[wid$item_id == "w_overcap_sdt_what_developing"])
        overcap_sdt_developing_selected <- paste0(names(overcap_sdt_developing_names[overcap_sdt_developing_names %in% overcap$sdt_what_developing]), collapse = ", ")
        overcap_sdt_developing <- paste0("<li>", "Developing: ", overcap_sdt_developing_selected, "</li>")
        
      }else{
        overcap_sdt_developing <- paste0("<li>", "Developing: None.", "</li>")
      }
      
      # Developing S&DT
      if(overcap$sdt_sve == "YES"){
        
        overcap_sdt_sve_names <- unlist(wid$choices[wid$item_id == "w_overcap_sdt_what_sve"])
        overcap_sdt_sve_selected <- paste0(names(overcap_sdt_sve_names[overcap_sdt_sve_names %in% overcap$sdt_what_sve]), collapse = ", ")
        overcap_sdt_sve <- paste0("<li>", "SVEs: ", overcap_sdt_sve_selected, "</li>")
        
      }else{
        overcap_sdt_sve <- paste0("<li>", "SVEs: None.", "</li>")
      }
      
      overcap_sdt <- paste0("<small class = 'gray'>", "S&DT: ", "</br></small>",
                        "<small><ul>", 
                        overcap_sdt_ldc,
                        overcap_sdt_developing,
                        overcap_sdt_sve,
                        "</small></ul>")
      
    }
    
    # Output (at least one definition selected)
    overcap_out <- paste0(overcap_header,
                      overcap_disciplines,
                      overcap_scope,
                      overcap_sdt)
    
  } # / length OCOF definitions > 0
  
  ### CAP/TIER --------------------
  
  if(cap_tier$on_off == "NO"){
    
    cap_tier_out <- paste0("<small class = 'gray'>", "CAP/TIER:  ", "</small>",
                       "<small>", "No.", "</small>", "<br>")
    
  }else if(cap_tier$on_off == "YES"){
    
    cap_tier_intro <- paste0("<small class = 'gray'>", "CAP/TIER:  ", "</small>",
                             "<small>", "Yes - ")
    
    ### 1 Tier
    if(cap_tier$tier_number == "ONE"){
      
      # Cap setting method
      if(cap_tier$tier1_cap_rule == "VALUE"){
        
        cap <- paste0("All Members recieve a cap equal to $", cap_tier$tier1_cap_value, " million USD.")

      }else if(cap_tier$tier1_cap_rule == "PERCENT_SUBS"){
        
        cap <- paste0("All Members recieve a cap equal to ", cap_tier$tier1_cap_percent, "% of total baseline subsidies.")

      }else if(cap_tier$tier1_cap_rule == "PERCENT_REV"){
        
        cap <- paste0("All Members recieve a cap equal to ", cap_tier$tier1_cap_percent, "% of total landed value of marine fisheries.")
        
      }else if(cap_tier$tier1_cap_rule == "FISHERS"){
        
        cap <- paste0("All Members recieve a cap equal to ", cap_tier$tier1_cap_percent, "% of global average subsides per fisher multiplied by the number of fishers.")

      }else if(cap_tier$tier1_cap_rule == "BEST"){
        
        cap <- paste0("All Members recieve a cap equal to whichever of the following three options yields the greatest value: 1) ", cap_tier$tier1_cap_best_percent_subs, "% of total baseline subsidies; 2) ", cap_tier$tier1_cap_best_percent_landed_value, "% of total landed value of marine fisheries; 3) ", cap_tier$tier1_cap_best_percent_fishers, "% of global average subsides per fisher multiplied by the number of fishers.") 
        
      }else if(cap_tier$tier1_cap_rule == "BRAZIL"){
        
        cap <- paste0("All Members recieve a cap based upon the formula presented in the Brazil - Formula proposal.") 
        
      }
      
      cap_tier_out <- paste0(cap_tier_intro, 
                             cap, "</small><br>")
      
    
    ### 2 Tiers
    }else if(cap_tier$tier_number == "TWO"){
      
      ### Tier method
      tier_choices <- unlist(wid$choices[wid$item_id == "w_tier_system"])
      
      tier <- paste0("Members are sorted into two tiers based upon ", names(tier_choices[tier_choices == cap_tier$tier_system]), ". ")
      
      ### Tier 1 caps
      if(cap_tier$tier1_cap_rule == "VALUE"){
        
        tier_1_cap <- paste0("Tier 1 Members recieve a cap equal to $", cap_tier$tier1_cap_value, " million USD. ")
        
      }else if(cap_tier$tier1_cap_rule == "PERCENT_SUBS"){
        
        tier_1_cap <- paste0("Tier 1 Members recieve a cap equal to ", cap_tier$tier1_cap_percent, "% of total baseline subsidies. ")
        
      }else if(cap_tier$tier1_cap_rule == "PERCENT_REV"){
        
        tier_1_cap <- paste0("Tier 1 Members recieve a cap equal to ", cap_tier$tier1_cap_percent, "% of total landed value of marine fisheries. ")
        
      }else if(cap_tier$tier1_cap_rule == "FISHERS"){
        
        tier_1_cap <- paste0("Tier 1 Members recieve a cap equal to ", cap_tier$tier1_cap_percent, "% of global average subsides per fisher multiplied by the number of fishers. ")
        
      }else if(cap_tier$tier1_cap_rule == "BEST"){
        
        tier_1_cap <- paste0("Tier 1 Members recieve a cap equal to whichever of the following three options yields the greatest value: 1) ", cap_tier$tier1_cap_best_percent_subs, "% of total baseline subsidies; 2) ", cap_tier$tier1_cap_best_percent_landed_value, "% of total landed value of marine fisheries; 3) ", cap_tier$tier1_cap_best_percent_fishers, "% of global average subsides per fisher multiplied by the number of fishers. ") 
        
      }
      
      ### Tier 2 caps
      if(cap_tier$tier2_cap_rule == "VALUE"){
        
        tier_2_cap <- paste0("Tier 2 Members recieve a cap equal to $", cap_tier$tier2_cap_value, " million USD.")
        
      }else if(cap_tier$tier2_cap_rule == "PERCENT_SUBS"){
        
        tier_2_cap <- paste0("Tier 2 Members recieve a cap equal to ", cap_tier$tier2_cap_percent, "% of total baseline subsidies.")
        
      }else if(cap_tier$tier2_cap_rule == "PERCENT_REV"){
        
        tier_2_cap <- paste0("Tier 2 Members recieve a cap equal to ", cap_tier$tier2_cap_percent, "% of total landed value of marine fisheries.")
        
      }else if(cap_tier$tier2_cap_rule == "FISHERS"){
        
        tier_2_cap <- paste0("Tier 2 Members recieve a cap equal to ", cap_tier$tier2_cap_percent, "% of global average subsides per fisher multiplied by the number of fishers.")
        
      }else if(cap_tier$tier2_cap_rule == "BEST"){
        
        tier_2_cap <- paste0("Tier 2 Members recieve a cap equal to whichever of the following three options yields the greatest value: 1) ", cap_tier$tier2_cap_best_percent_subs, "% of total baseline subsidies; 2) ", cap_tier$tier2_cap_best_percent_landed_value, "% of total landed value of marine fisheries; 3) ", cap_tier$tier2_cap_best_percent_fishers, "% of global average subsides per fisher multiplied by the number of fishers.") 
        
      }else if(cap_tier$tier2_cap_rule == "NONE"){
        
        tier_2_cap <- paste0("Tier 2 Members do not recieve a cap.")
      }
      
      cap_tier_out <- paste0(cap_tier_intro, 
                             tier,
                             tier_1_cap,
                             tier_2_cap,
                             "</small><br>")
      
    ### 3 Tiers  
    }else if(cap_tier$tier_number == "THREE"){
      
      ### Tier method
      tier_choices <- unlist(wid$choices[wid$item_id == "w_tier_system"])
      
      tier <- paste0("Members are sorted into three tiers based upon ", names(tier_choices[tier_choices == cap_tier$tier_system]), ". ")
      
      ### Tier 1 caps
      if(cap_tier$tier1_cap_rule == "VALUE"){
        
        tier_1_cap <- paste0("Tier 1 Members recieve a cap equal to $", cap_tier$tier1_cap_value, " million USD.")
        
      }else if(cap_tier$tier1_cap_rule == "PERCENT_SUBS"){
        
        tier_1_cap <- paste0("Tier 1 Members recieve a cap equal to ", cap_tier$tier1_cap_percent, "% of total baseline subsidies. ")
        
      }else if(cap_tier$tier1_cap_rule == "PERCENT_REV"){
        
        tier_1_cap <- paste0("Tier 1 Members recieve a cap equal to ", cap_tier$tier1_cap_percent, "% of total landed value of marine fisheries. ")
        
      }else if(cap_tier$tier1_cap_rule == "FISHERS"){
        
        tier_1_cap <- paste0("Tier 1 Members recieve a cap equal to ", cap_tier$tier1_cap_percent, "% of global average subsides per fisher multiplied by the number of fishers. ")
        
      }else if(cap_tier$tier1_cap_rule == "BEST"){
        
        tier_1_cap <- paste0("Tier 1 Members recieve a cap equal to whichever of the following three options yields the greatest value: 1) ", cap_tier$tier1_cap_best_percent_subs, "% of total baseline subsidies; 2) ", cap_tier$tier1_cap_best_percent_landed_value, "% of total landed value of marine fisheries; 3) ", cap_tier$tier1_cap_best_percent_fishers, "% of global average subsides per fisher multiplied by the number of fishers. ") 
        
      }
      
      ### Tier 2 caps
      if(cap_tier$tier2_cap_rule == "VALUE"){
        
        tier_2_cap <- paste0("Tier 2 Members recieve a cap equal to $", cap_tier$tier2_cap_value, " million USD. ")
        
      }else if(cap_tier$tier2_cap_rule == "PERCENT_SUBS"){
        
        tier_2_cap <- paste0("Tier 2 Members recieve a cap equal to ", cap_tier$tier2_cap_percent, "% of total baseline subsidies. ")
        
      }else if(cap_tier$tier2_cap_rule == "PERCENT_REV"){
        
        tier_2_cap <- paste0("Tier 2 Members recieve a cap equal to ", cap_tier$tier2_cap_percent, "% of total landed value of marine fisheries. ")
        
      }else if(cap_tier$tier2_cap_rule == "FISHERS"){
        
        tier_2_cap <- paste0("Tier 2 Members recieve a cap equal to ", cap_tier$tier2_cap_percent, "% of global average subsides per fisher multiplied by the number of fishers. ")
        
      }else if(cap_tier$tier2_cap_rule == "BEST"){
        
        tier_2_cap <- paste0("Tier 2 Members recieve a cap equal to whichever of the following three options yields the greatest value: 1) ", cap_tier$tier2_cap_best_percent_subs, "% of total baseline subsidies; 2) ", cap_tier$tier2_cap_best_percent_landed_value, "% of total landed value of marine fisheries; 3) ", cap_tier$tier2_cap_best_percent_fishers, "% of global average subsides per fisher multiplied by the number of fishers. ") 
        
      }else if(cap_tier$tier2_cap_rule == "NONE"){
        
        tier_2_cap <- paste0("Tier 2 Members do not recieve a cap. ")
      }
      
      ### Tier 3 caps
      if(cap_tier$tier3_cap_rule == "VALUE"){
        
        tier_3_cap <- paste0("Tier 3 Members recieve a cap equal to $", cap_tier$tier3_cap_value, " million USD.")
        
      }else if(cap_tier$tier3_cap_rule == "PERCENT_SUBS"){
        
        tier_3_cap <- paste0("Tier 3 Members recieve a cap equal to ", cap_tier$tier3_cap_percent, "% of total baseline subsidies.")
        
      }else if(cap_tier$tier3_cap_rule == "PERCENT_REV"){
        
        tier_3_cap <- paste0("Tier 3 Members recieve a cap equal to ", cap_tier$tier3_cap_percent, "% of total landed value of marine fisheries.")
        
      }else if(cap_tier$tier3_cap_rule == "FISHERS"){
        
        tier_3_cap <- paste0("Tier 3 Members recieve a cap equal to ", cap_tier$tier3_cap_percent, "% of global average subsides per fisher multiplied by the number of fishers.")
        
      }else if(cap_tier$tier3_cap_rule == "BEST"){
        
        tier_3_cap <- paste0("Tier 2 Members recieve a cap equal to whichever of the following three options yields the greatest value: 1) ", cap_tier$tier3_cap_best_percent_subs, "% of total baseline subsidies; 2) ", cap_tier$tier3_cap_best_percent_landed_value, "% of total landed value of marine fisheries; 3) ", cap_tier$tier3_cap_best_percent_fishers, "% of global average subsides per fisher multiplied by the number of fishers.") 
        
      }else if(cap_tier$tier3_cap_rule == "NONE"){
        
        tier_3_cap <- paste0("Tier 3 Members do not recieve a cap.")
      }
      
      cap_tier_out <- paste0(cap_tier_intro, 
                             tier,
                             tier_1_cap,
                             tier_2_cap,
                             tier_3_cap,
                             "</small><br>")
      
    }
    
  } # /close Cap/Tier ON
  
  final_out <- paste0(overcap_out,
                      cap_tier_out)
  
}