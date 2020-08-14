###---------------------------------
### IUU reactive policy summary
### --------------------------------

IUUSummaryText <- function(iuu,
                           wid,
                           text,
                           country_choices){
  
  ### IUU title
  iuu_header <- paste0("<b class = 'big'>", "Illegal, Unreported, & Unregulated", "</b>", "</br>")
  
  ### Nothing selected ---
  if(length(iuu$definitions) == 0){
    
    iuu_pro <- "None selected."
    iuu_disciplines <- paste0("<small class = 'gray'>", "DISCIPLINES:  ", "</small>",
                              "<small>", iuu_pro, "</br></small>")
    
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
                          "<small>", "Selected disciplines apply to all Members & vessels.", "</br></small>")
      
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
                        "<small>", iuu_sdt, "</br></small>")
      
    }else if(iuu$allow_sdt == "YES"){
      
      # LDC S&DT
      if(iuu$sdt_ldc == "YES"){
        
        iuu_sdt_ldc <- paste0("LDCs: ", "None.", "</br>")
        
      }else{
        iuu_sdt_ldc <- paste0("LDCs: ", "None.", "</br>")
      }
      
      # Developing S&DT
      if(iuu$sdt_developing == "YES"){
        
        iuu_sdt_developing <- paste0("Developing: ", "None.", "</br>")
        
      }else{
        iuu_sdt_developing <- paste0("Developing: ", "None.", "</br>")
      }
      
      # Developing S&DT
      if(iuu$sdt_sve == "YES"){
        
        iuu_sdt_sve <- paste0("SVEs: ", "None.", "</br>")
        
      }else{
        iuu_sdt_sve <- paste0("SVEs: ", "None.", "</br>")
      }
      
      iuu_sdt <- paste0("<small class = 'gray'>", "S&DT: ", "</br></small>",
                        "<small>", 
                        iuu_sdt_ldc,
                        iuu_sdt_developing,
                        iuu_sdt_sve,
                        "</small>")
      
    }
    
    # Output (at least one definition selected)
    iuu_out <- paste0(iuu_header,
                      iuu_disciplines,
                      iuu_scope,
                      iuu_sdt)
    
  } # /IUU
  
}