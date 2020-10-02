###---------------------------------
### OCOF reactive policy summary
### --------------------------------

CapSummaryText <- function(overcap,
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
    
    overcap_pro_names <- unlist(wid$choices[wid$item_id == "w_overcap_definitions"])
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
  
}