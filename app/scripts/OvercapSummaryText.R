###---------------------------------
### Overcap reactive policy summary
### --------------------------------

OvercapSummaryText <- function(overcap,
                               cap_tier,
                               wid,
                               text,
                               country_choices){
  
  ### Overcapacity title
  overcap_header <- paste0("<b class = 'big'>", "Overfished", "</b>", "</br>")
  
  ### Nothing selected ---
  if(length(overcap$definitions) == 0){
    
    overcap_pro <- "None selected."
    overcap_disciplines <- paste0("<small class = 'gray'>", "DISCIPLINES:  ", "</small>",
                              "<small>", overcap_pro, "</br></small>")
    
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
      
      # MANUAL: Manual country selection
      if("MANUAL" %in% overcap$scope_select & length(overcap$scope_manual) >= 1){
        
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
      
      # Scope output
      overcap_scope <- paste0(overcap_scope,
                          overcap_scope_manual,
                          overcap_scope_ex_ter,
                          "</ul></small>")
      
    }
    
    ### S&DT (at least one definition selected) ---
    if(overcap$allow_sdt == "NO"){
      
      overcap_sdt <- "None."
      overcap_sdt <- paste0("<small class = 'gray'>", "S&DT: ", "</small>",
                        "<small>", overcap_sdt, "</br></small>")
      
    }else if(overcap$allow_sdt == "YES"){
      
      # LDC S&DT
      if(overcap$sdt_ldc == "YES"){
        
        overcap_sdt_ldc_names <- unlist(wid$choices[wid$item_id == "w_overcap_sdt_what_ldc"])
        overcap_sdt_ldc_selected <- paste0(names(overcap_sdt_ldc_names[overcap$sdt_what_ldc %in% overcap_sdt_ldc_names]), collapse = ", ")
        overcap_sdt_ldc <- paste0("<li>", "LDCs: ", overcap_sdt_ldc_selected, "</li>")
        
      }else{
        overcap_sdt_ldc <- paste0("<li>", "LDCs: None.","</li>")
      }
      
      # Developing S&DT
      if(overcap$sdt_developing == "YES"){
        
        overcap_sdt_developing <- paste0("Developing: ", "None.", "</br>")
        
      }else{
        overcap_sdt_developing <- paste0("<li>", "Developing: None.", "</li>")
      }
      
      # Developing S&DT
      if(overcap$sdt_sve == "YES"){
        
        overcap_sdt_sve <- paste0("SVEs: ", "None.", "</br>")
        
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
    
  } # /OA
  
}