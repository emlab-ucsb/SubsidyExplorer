###---------------------------------
### OA reactive policy summary
### --------------------------------

OASummaryText <- function(oa,
                          wid,
                          text,
                          country_choices){
  
  ### OA title
  oa_header <- paste0("<b class = 'big'>", "Overfished", "</b>", "</br>")
  
  ### Nothing selected ---
  if(length(oa$definitions) == 0){
    
    oa_pro <- "None selected."
    oa_disciplines <- paste0("<small class = 'gray'>", "DISCIPLINES:  ", "</small>",
                              "<small>", oa_pro, "</br></small>")
    
    oa_scope <- ""
    
    oa_sdt <- ""
    
    # Output for nothing selected
    oa_out <- paste0(oa_header,
                      oa_disciplines,
                      oa_scope,
                      oa_sdt)
    
  ### At least one definition selected ---
  }else if(length(oa$definitions) >= 1){
    
    oa_pro_names <- unlist(wid$choices[wid$item_id == "w_oa_definitions"])
    oa_pro_selected <- names(oa_pro_names[oa_pro_names %in% oa$definitions])
    
    oa_pro <- paste0("<small>",text$item_label[text$item_id == "w_oa_definitions"], 
                      "<ul><li>", 
                      paste0(oa_pro_selected, collapse = "</li><li>"), 
                      "</li></ul></small>")
    
    oa_disciplines <- paste0("<small class = 'gray'>", "DISCIPLINES:  ", "</small>",
                              oa_pro)
    
    ### Scope (at least one definition selected) ---
    if(oa$scope == "ALL"){
      
      oa_scope <- paste0("<small class = 'gray'>", "SCOPE:  ", "</small>",
                          "<small>", "Disciplines apply to all Members & vessels.", "</br></small>")
      
    }else if(oa$scope == "SELECT"){
      
      oa_scope_names <- unlist(wid$choices[wid$item_id == "w_oa_scope_select"])
      
      oa_scope <- paste0("<small class = 'gray'>", "SCOPE:  ", "</small>",
                          "<small>", "Disciplines only apply to vessels: ", "<ul>")
      
      # MANUAL: Manual country selection
      if("MANUAL" %in% oa$scope_select & length(oa$scope_manual) >= 1){
        
        oa_country_names <- names(country_choices[country_choices %in% oa$scope_manual])
        oa_scope_manual <- paste0("<li>", names(oa_scope_names[oa_scope_names == "MANUAL"]),
                                   " (", paste0(oa_country_names, collapse = ", "), ")", "</li>")
      }else{
        
        oa_scope_manual <- ""
      }
      
      # EX_TER: Outside of territorial waters
      if("EX_TER" %in% oa$scope_select){
        
        oa_scope_ex_ter <- paste0("<li>", names(oa_scope_names[oa_scope_names == "EX_TER"]), "</li>")
        
      }else{
        
        oa_scope_ex_ter <- ""
        
      }
      
      # Scope output
      oa_scope <- paste0(oa_scope,
                          oa_scope_manual,
                          oa_scope_ex_ter,
                          "</ul></small>")
      
    }
    
    ### S&DT (at least one definition selected) ---
    if(oa$allow_sdt == "NO"){
      
      oa_sdt <- "None."
      oa_sdt <- paste0("<small class = 'gray'>", "S&DT: ", "</small>",
                        "<small>", oa_sdt, "</br></small>")
      
    }else if(oa$allow_sdt == "YES"){
      
      # LDC S&DT
      if(oa$sdt_ldc == "YES"){
        
        oa_sdt_ldc_names <- unlist(wid$choices[wid$item_id == "w_oa_sdt_what_ldc"])
        oa_sdt_ldc_selected <- paste0(names(oa_sdt_ldc_names[oa$sdt_what_ldc %in% oa_sdt_ldc_names]), collapse = ", ")
        oa_sdt_ldc <- paste0("<li>", "LDCs: ", oa_sdt_ldc_selected, "</li>")
        
      }else{
        oa_sdt_ldc <- paste0("<li>", "LDCs: None.","</li>")
      }
      
      # Developing S&DT
      if(oa$sdt_developing == "YES"){
        
        oa_sdt_developing <- paste0("Developing: ", "None.", "</br>")
        
      }else{
        oa_sdt_developing <- paste0("<li>", "Developing: None.", "</li>")
      }
      
      # Developing S&DT
      if(oa$sdt_sve == "YES"){
        
        oa_sdt_sve <- paste0("SVEs: ", "None.", "</br>")
        
      }else{
        oa_sdt_sve <- paste0("<li>", "SVEs: None.", "</li>")
      }
      
      oa_sdt <- paste0("<small class = 'gray'>", "S&DT: ", "</br></small>",
                        "<small><ul>", 
                        oa_sdt_ldc,
                        oa_sdt_developing,
                        oa_sdt_sve,
                        "</small></ul>")
      
    }
    
    # Output (at least one definition selected)
    oa_out <- paste0(oa_header,
                      oa_disciplines,
                      oa_scope,
                      oa_sdt)
    
  } # /OA
  
}