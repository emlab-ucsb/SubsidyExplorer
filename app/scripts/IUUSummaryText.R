###---------------------------------
### IUU reactive policy summary
### --------------------------------

IUUSummaryText <- function(iuu,
                           wid,
                           text,
                           country_choices){
  
  ### IUU title
  iuu_header <- paste0("<b class = 'big'>", "Illegal, Unreported, and Unregulated", "</b>", "</br>")
  
  ### Nothing selected ---
  if(length(iuu$definitions) == 0){
    
    iuu_pro <- "None selected."
    iuu_disciplines <- paste0("<small class = 'gray'>", "DISCIPLINES:  ", "</small>",
                              "<small>", iuu_pro, "</small>", "</br>")
    
    iuu_scope <- ""
    
    iuu_sdt <- ""
    
    # Output for nothing selected
    iuu_out <- paste0(iuu_header,
                      iuu_disciplines,
                      iuu_scope,
                      iuu_sdt)
    
    ### At least one definition selected ---
  }else if(length(iuu$definitions) >= 1){
    
    iuu_pro_names <- unlist(wid$choices[wid$item_id == "w_iuu_definitions"])
    iuu_pro_selected <- names(iuu_pro_names[iuu_pro_names %in% iuu$definitions])
    
    iuu_pro <- paste0("<small>",text$item_label[text$item_id == "w_iuu_definitions"], 
                      "<ul><li>", 
                      paste0(iuu_pro_selected, collapse = "</li><li>"), 
                      "</li></ul></small>")
    
    if("IUU2" %in% iuu$definitions | 
       "IUU3" %in% iuu$definitions | 
       "IUU4" %in% iuu$definitions | 
       "IUU5" %in% iuu$definitions | 
       "IUU6" %in% iuu$definitions){
      
      if(iuu$assumption == "YES"){
        
        iuu_pro <- paste0(iuu_pro, 
                          "<i><small>", "Where data does not exist, ", iuu$percent, "% of global fishing effort is assumed to be IUU.", "</i></small></br>")
      }
    }
    
    iuu_disciplines <- paste0("<small class = 'gray'>", "DISCIPLINES:  ", "</small>",
                              iuu_pro)
    
    ### Scope (at least one definition selected) ---
    if(iuu$scope == "ALL"){
      
      iuu_scope <- paste0("<small class = 'gray'>", "SCOPE:  ", "</small>",
                          "<small>", "Disciplines apply to all Members & vessels.", "</br></small>")
      
    }else if(iuu$scope == "SELECT"){
      
      iuu_scope_names <- unlist(wid$choices[wid$item_id == "w_iuu_scope_select"])
      
      iuu_scope <- paste0("<small class = 'gray'>", "SCOPE:  ", "</small>",
                          "<small>", "Disciplines only apply to vessels: ", "<ul>")
      
      # MANUAL: Manual country selection
      if("MANUAL" %in% iuu$scope_select & length(iuu$scope_manual) >= 1){
        
        iuu_country_names <- names(country_choices[country_choices %in% iuu$scope_manual])
        iuu_scope_manual <- paste0("<li>", names(iuu_scope_names[iuu_scope_names == "MANUAL"]),
                                   " (", paste0(iuu_country_names, collapse = ", "), ")", "</li>")
      }else{
        
        iuu_scope_manual <- ""
      }
      
      # EX_TER: Outside of territorial waters
      if("EX_TER" %in% iuu$scope_select){
        
        iuu_scope_ex_ter <- paste0("<li>", names(iuu_scope_names[iuu_scope_names == "EX_TER"]), "</li>")
        
      }else{
        
        iuu_scope_ex_ter <- ""
        
      }
      
      # Scope output
      iuu_scope <- paste0(iuu_scope,
                          iuu_scope_manual,
                          iuu_scope_ex_ter,
                          "</ul></small>")
      
    }
    
    ### S&DT (at least one definition selected) ---
    if(iuu$allow_sdt == "NO"){
      
      iuu_sdt <- "None."
      iuu_sdt <- paste0("<small class = 'gray'>", "S&DT: ", "</small>",
                        "<small>", iuu_sdt, "</small>", "</br>")
      
    }else if(iuu$allow_sdt == "YES"){
      
      # LDC S&DT
      if(iuu$sdt_ldc == "YES"){
        
        iuu_sdt_ldc_names <- unlist(wid$choices[wid$item_id == "w_iuu_sdt_what_ldc"])
        iuu_sdt_ldc_selected <- paste0(names(iuu_sdt_ldc_names[iuu_sdt_ldc_names %in% iuu$sdt_what_ldc]), collapse = ", ")
        iuu_sdt_ldc <- paste0("<li>", "LDCs: ", iuu_sdt_ldc_selected, "</li>")
        
      }else{
        iuu_sdt_ldc <- paste0("<li>", "LDCs: None.","</li>")
      }
      
      # Developing S&DT
      if(iuu$sdt_developing == "YES"){
        
        iuu_sdt_developing_names <- unlist(wid$choices[wid$item_id == "w_iuu_sdt_what_developing"])
        iuu_sdt_developing_selected <- paste0(names(iuu_sdt_developing_names[iuu_sdt_developing_names %in% iuu$sdt_what_developing]), collapse = ", ")
        iuu_sdt_developing <- paste0("<li>", "Developing: ", iuu_sdt_developing_selected, "</li>")
        
      }else{
        iuu_sdt_developing <- paste0("<li>", "Developing: None.", "</li>")
      }
      
      # Developing S&DT
      if(iuu$sdt_sve == "YES"){
        
        iuu_sdt_sve_names <- unlist(wid$choices[wid$item_id == "w_iuu_sdt_what_sve"])
        iuu_sdt_sve_selected <- paste0(names(iuu_sdt_sve_names[iuu_sdt_sve_names %in% iuu$sdt_what_sve]), collapse = ", ")
        iuu_sdt_sve <- paste0("<li>", "SVEs: ", iuu_sdt_sve_selected, "</li>")
        
      }else{
        iuu_sdt_sve <- paste0("<li>", "SVEs: None.", "</li>")
      }
      
      iuu_sdt <- paste0("<small class = 'gray'>", "S&DT: ", "</br></small>",
                        "<small><ul>", 
                        iuu_sdt_ldc,
                        iuu_sdt_developing,
                        iuu_sdt_sve,
                        "</small></ul>")
      
    }
    
    # Output (at least one definition selected)
    iuu_out <- paste0(iuu_header,
                      iuu_disciplines,
                      iuu_scope,
                      iuu_sdt)
    
  } # /IUU
  
}