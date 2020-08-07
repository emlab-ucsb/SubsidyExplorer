
IUUSummaryText <- function(iuu,
                           text,
                           iuu_definitions,
                           country_choices){
  
  # IUU
  if(length(iuu$definitions) == 0){
    
    iuu_sum <- "None selected."
    
  }else if(length(iuu$definitions) >= 1){
    
    iuu_prohib_names <- names(iuu_definitions[iuu_definitions %in% iuu$definitions])
    iuu_pro <- paste0(text$item_label[text$item_id == "w_iuu_definitions"], 
                      "<ul><li>", 
                      paste0(iuu_prohib_names, collapse = "</li><li>"), 
                      "</li></ul>")
    
    if("IUU2" %in% iuu$definitions | 
       "IUU3" %in% iuu$definitions | 
       "IUU4" %in% iuu$definitions | 
       "IUU5" %in% iuu$definitions | 
       "IUU6" %in% iuu$definitions){
      
      if(iuu$assumption == "YES"){
        
        # iuu_assump <- paste0(names(iuu_definitions[iuu_definitions %in% iuu$definitions]))
        # iuu_assump_short <- 
        
        iuu_pro <- paste0(iuu_pro, iuu$percent, "% of global fishing effort is assumed to have been identified as IUU by coastal-, flag-, subsidizing-, port-, and/or market-Member states")
        
      }
    }
    
    # IUU - scope
    if(iuu$scope == "ALL"){
      
      iuu_sco <- "Selected disciplines apply to all Member-flagged vessels"
      
    }else if(iuu$scope == "EX_TER"){
      
      iuu_sco <- "Selected disciplines only apply to Member-flagged vessels fishing outside of their own territorial waters"
      
    }else if(iuu$scope == "SELECT"){
      
      iuu_country_names <- names(country_choices[country_choices %in% iuu$scope_manual])
      iuu_sco <- paste0("Selected disciplines only apply to the following Members: ", paste0(iuu_country_names, collapse = ", "))
      
    }
    
    # # IUU - S&DT
    # if(iuu$allow_sdt == "Yes"){
    #   
    #   iuu_sdt_names <- names(sdt_who[sdt_who == iuu$sdt_who])
    #   iuu_sdt_what <- names(sdt_what[sdt_what == iuu$sdt_what])
    #   
    #   iuu_sdt <- paste0("S&DT is allowed. The following S&DT applies to ", iuu_sdt_names, ": ", iuu_sdt_what) 
    #   
    # }else if(iuu$allow_sdt == "No"){
    #   
    #   iuu_sdt <- "S&DT is not allowed"
    #   
    # }
    
    iuu_sum <- paste0(iuu_pro, ". ", iuu_sco, ". ")
    
  } # /IUU
  
}

OASummaryText <- function(oa,
                          oa_definitions,
                          country_choices){
  
  # OA
  if(length(oa$definitions) == 0){
    
    oa_sum <- "None selected."
    
  }else if(length(oa$definitions >= 1)){
    
    oa_prohib_names <- names(oa_definitions[oa_definitions %in% oa$definitions])
    oa_pro <- paste0("Subsidies shall be prohibited to any Member-flagged vessel if the target fish stock... ", paste0(oa_prohib_names, collapse = "; "))
    
    # OA - scope
    if(oa$scope == "SELECT"){
      
      oa_country_names <- names(country_choices[country_choices %in% oa$scope_manual])
      oa_sco <- paste0("Selected disciplines only apply to the following Members: ", paste0(oa_country_names, collapse = ", "))
      
    }else if(oa$scope == "HS"){
      
      oa_sco <- paste0("Selected disciplines only apply to vessels spending at least ",oa$hs_cutoff, "% of their total fishing effort on the high seas")
      
    }else if(oa$scope == "DW"){
      
      oa_sco <- "Selected disciplines only apply to Member-flagged vessels fishing in EEZs belonging to other coastal states ('distant-water' vessels)"
      
    }else {
      
      oa_sco <- "Selected disciplines apply to all Members"
      
    }
    
    # # OA - S&DT
    # if(oa$allow_sdt == "Yes"){
    #   
    #   oa_sdt_names <- names(sdt_who[sdt_who == oa$sdt_who])
    #   oa_sdt_what <- names(sdt_what[sdt_what == oa$sdt_what])
    #   
    #   oa_sdt <- paste0("S&DT is allowed. The following S&DT applies to ", oa_sdt_names, ": ", oa_sdt_what) 
    #   
    # }else if(oa$allow_sdt == "No"){
    #   
    #   oa_sdt <- "S&DT is not allowed"
    #   
    # }
    
    oa_sum <- paste0(oa_pro, ". ", oa_sco, ". ")
  
  }
}
  
OvercapSummaryText <- function(overcap,
                               overcap_definitions,
                               country_choices){
  
  # Overcapacity
  
  if(length(overcap$definitions) == 0){
    
    overcap_sum <- "None selected."
    
  }else if(length(overcap$definitions >= 1)){
    
    overcap_prohib_names <- names(overcap_definitions[overcap_definitions %in% overcap$definitions])
    overcap_pro <- paste0("The following types of subsidies are considered to contribute to overcapacity and overfishing and are therefore prohibited: ", paste0(overcap_prohib_names, collapse = "; "))
    
    # Overcapacity - scope
    if(overcap$scope == "SELECT"){
      
      overcap_country_names <- names(country_choices[country_choices %in% overcap$scope_manual])
      overcap_sco <- paste0("Selected disciplines only apply to the following Members: ", paste0(overcap_country_names, collapse = ", "))
      
    }else if(overcap$scope == "HS"){
      
      overcap_sco <- paste0("Selected disciplines only apply to Member-flagged vessels spending at least ", overcap$hs_cutoff, "% of their total fishing effort on the high seas")
      
    }else if(overcap$scope == "DW"){
      
      overcap_sco <- "Selected disciplines only apply to Member-flagged vessels fishing in EEZs belonging to other coastal states ('distant-water' vessels)"
      
    }else {
      
      overcap_sco <- "Selected disciplines apply to all Members"
      
    }
    
    # # Overcapacity - S&DT
    # if(overcap$allow_sdt == "Yes"){
    #   
    #   overcap_sdt_names <- names(sdt_who[sdt_who == overcap$sdt_who])
    #   overcap_sdt_what <- names(sdt_what[sdt_what == overcap$sdt_what])
    #   
    #   overcap_sdt <- paste0("S&DT is allowed. The following S&DT applies to ", overcap_sdt_names, ": ", overcap_sdt_what) 
    #   
    # }else if(overcap$allow_sdt == "No"){
    #   
    #   overcap_sdt <- "S&DT is not allowed"
    #   
    # }
    
    overcap_sum <- paste0(overcap_pro, ". ", overcap_sco, ". ")
    
  } # /overcapacity
  
}
# 
# PolicySummaryText <- function(oa,
#                               overcap,
#                               cap,
#                               iuu_definitions,
#                               oa_definitions,
#                               overcap_definitions,
#                               cap_set_methods,
#                               tiering_options,
#                               country_choices){
#   
#   
# 
#     
#   } # / Overfished and unassessed
#   
#   ### ---------------------------
#   
#   
#   ### ----------------------------
#   
#   # # Cap/Tier
#   # 
#   # if(cap$on_off == "No"){
#   #   
#   #   cap_sum <- "No."
#   #   
#   # }else if(cap$on_off == "Yes"){
#   #   
#   #   cap_pro <- "Yes."
#   #   
#   #   # Number of tiers. 
#   #   if(cap$tier_number == "One"){
#   #     
#   #     cap_tier_1 <- "Subsidies are capped for all Members (one 'tier')."
#   #     
#   #     cap_1_names <- names(cap_set_methods[cap_set_methods == cap$tier1_cap_rule])
#   #     cap_1 <- paste0("The cap for all Members is set based on ", cap_1_names, ": ")
#   #     
#   #     if(cap$tier1_cap_rule == "value"){
#   #       
#   #       cap_1_rule <- paste0("US$", cap$tier1_cap_value, " million.")
#   #       
#   #     }else if(cap$tier1_cap_rule == "fishers"){
#   #       
#   #       cap_1_rule <- paste0("US$", cap$tier1_cap_fishers, " multiplied by the number of fishers.")
#   #       
#   #     }else{
#   #       
#   #       cap_1_rule <- paste0(cap$tier1_cap_percent, "%.")
#   #     }
#   #     
#   #     cap_tier <- paste0(cap_tier_1, " ", cap_1, cap_1_rule)
#   #     
#   #     
#   #   }else if(cap$tier_number == "Two"){
#   #     
#   #     cap_tier_names <- names(tiering_options[tiering_options == cap$tier_system])
#   #     cap_tier_2 <- paste0("Members are divided into two tiers based on ", cap_tier_names, ".")
#   #     
#   #     if(cap$tier_system == "development"){
#   #       
#   #       cap_threshold <- "Developed countries are in tier 1; developing countries and LDCs are in tier 2."
#   #       
#   #     }else{
#   #       
#   #       cap_threshold <- paste0("Members responsible for at least ", cap$tier2_cutoff, "% are in tier 1; the rest are in tier 2.")
#   #       
#   #     }
#   #           
#   #     
#   #     # Cap tier 1
#   #     cap_1_names <- names(cap_set_methods[cap_set_methods == cap$tier1_cap_rule])
#   #     cap_1 <- paste0("The cap for tier 1 Members is determined based on ", cap_1_names, ": ")
#   #     
#   #     if(cap$tier1_cap_rule == "value"){
#   #       
#   #       cap_1_rule <- paste0("US$", cap$tier1_cap_value, " million.")
#   #       
#   #     }else if(cap$tier1_cap_rule == "fishers"){
#   #       
#   #       cap_1_rule <- paste0("US$", cap$tier1_cap_fishers, " multiplied by the number of fishers.")
#   #       
#   #     }else{
#   #       
#   #       cap_1_rule <- paste0(cap$tier1_cap_percent, "%.")
#   #     }
#   #     
#   #     cap_1_out <- paste0(cap_1, cap_1_rule)
#   #     
#   #     # Cap tier 2
#   #     if(cap$tier2_cap_rule == "none"){
#   #       
#   #       cap_2_out <- "There is no cap for tier 2 Members."
#   #       
#   #     }else{
#   #       
#   #       cap_2_names <- names(cap_set_methods[cap_set_methods == cap$tier2_cap_rule])
#   #       cap_2 <- paste0("The cap for tier 2 Members is determined based on ", cap_2_names, ": ")
#   #       
#   #       if(cap$tier2_cap_rule == "value"){
#   #         
#   #         cap_2_rule <- paste0("US$", cap$tier2_cap_value, " million.")
#   #         
#   #       }else if(cap$tier2_cap_rule == "fishers"){
#   #         
#   #         cap_2_rule <- paste0("US$", cap$tier2_cap_fishers, " multiplied by the number of fishers.")
#   #         
#   #       }else{
#   #         
#   #         cap_2_rule <- paste0(cap$tier2_cap_percent, "%.")
#   #       }
#   #       
#   #       cap_2_out <- paste0(cap_2, cap_2_rule)
#   #       
#   #     }
#   #     
#   #     cap_tier <- paste0(cap_tier_2, " ", cap_threshold, " ", cap_1_out, " ", cap_2_out)
#   #     
#   #     ### Three tiers  
#   #   }else if(cap$tier_number == "Three"){
#   #     
#   #     cap_tier_names <- names(tiering_options[tiering_options == cap$tier_system])
#   #     cap_tier_3 <- paste0("Members are divided into three tiers based on ", cap_tier_names, ".")
#   #     
#   #     if(cap$tier_system == "development"){
#   #       
#   #       cap_threshold <- "Developed countries are in tier 1; developing countries are in tier 2; LDCs are in tier 3."
#   #       
#   #     }else{
#   #       
#   #       cap_threshold <- paste0("Members responsible for at least ", cap$tier3_cutoff[2], "% are in tier 1; Members responsible for less than ", cap$tier3_cutoff[1], "% are in tier 3; the rest are in tier 2.")
#   #       
#   #     }
#   #     
#   #     # Cap tier 1
#   #     cap_1_names <- names(cap_set_methods[cap_set_methods == cap$tier1_cap_rule])
#   #     cap_1 <- paste0("The cap for tier 1 Members is determined based on ", cap_1_names, ": ")
#   #     
#   #     if(cap$tier1_cap_rule == "value"){
#   #       
#   #       cap_1_rule <- paste0("US$", cap$tier1_cap_value, " million.")
#   #       
#   #     }else if(cap$tier1_cap_rule == "fishers"){
#   #       
#   #       cap_1_rule <- paste0("US$", cap$tier1_cap_fishers, " multiplied by the number of fishers.")
#   #       
#   #     }else{
#   #       
#   #       cap_1_rule <- paste0(cap$tier1_cap_percent, "%.")
#   #     }
#   #     
#   #     cap_1_out <- paste0(cap_1, cap_1_rule)
#   #     
#   #     # Cap tier 2
#   #     if(cap$tier2_cap_rule == "none"){
#   #       
#   #       cap_2_out <- "There is no cap for tier 2 Members."
#   #       
#   #     }else{
#   #       
#   #       cap_2_names <- names(cap_set_methods[cap_set_methods == cap$tier2_cap_rule])
#   #       cap_2 <- paste0("The cap for tier 2 Members is determined based on ", cap_2_names, ": ")
#   #       
#   #       if(cap$tier2_cap_rule == "value"){
#   #         
#   #         cap_2_rule <- paste0("US$", cap$tier2_cap_value, " million.")
#   #         
#   #       }else if(cap$tier2_cap_rule == "fishers"){
#   #         
#   #         cap_2_rule <- paste0("US$", cap$tier2_cap_fishers, " multiplied by the number of fishers.")
#   #         
#   #       }else{
#   #         
#   #         cap_2_rule <- paste0(cap$tier2_cap_percent, "%.")
#   #       }
#   #       
#   #       cap_2_out <- paste0(cap_2, cap_2_rule)
#   #       
#   #     }
#   #     
#   #     # Cap tier 3
#   #     if(cap$tier3_cap_rule == "none"){
#   #       
#   #       cap_3_out <- "There is no cap for tier 3 Members."
#   #       
#   #     }else{
#   #       
#   #       cap_3_names <- names(cap_set_methods[cap_set_methods == cap$tier3_cap_rule])
#   #       cap_3 <- paste0("The cap for tier 3 Members is determined based on ", cap_3_names, ": ")
#   #       
#   #       if(cap$tier3_cap_rule == "value"){
#   #         
#   #         cap_3_rule <- paste0("US$", cap$tier3_cap_value, " million.")
#   #         
#   #       }else if(cap$tier3_cap_rule == "fishers"){
#   #         
#   #         cap_3_rule <- paste0("US$", cap$tier3_cap_fishers, " multiplied by the number of fishers.")
#   #         
#   #       }else{
#   #         
#   #         cap_3_rule <- paste0(cap$tier3_cap_percent, "%.")
#   #       }
#   #       
#   #       cap_3_out <- paste0(cap_3, cap_3_rule)
#   #       
#   #     }
#   #     
#   #     cap_tier <- paste0(cap_tier_3, " ", cap_threshold, " ", cap_1_out, " ", cap_2_out, " ", cap_3_out)
#   #     
#   #   } # /three tiers
#   #   
#   #   cap_sum <- paste0(cap_pro, " ", cap_tier)
#   #   
#   # } # /cap tier on
#   
#   
#   # Output text
#   out <- list(iuu_sum = iuu_sum,
#               oa_sum = oa_sum,
#               overcap_sum = overcap_sum)
#   
#   return(out)
# }
#                               
#                               
#                               