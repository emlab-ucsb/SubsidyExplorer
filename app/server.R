### -------------------------------------------------------------------
### SubsidyExplorer
### An interactive toolkit to explore the tradeoffs across proposed fisheries subsidies disciplines
### 
### Creator(s): Kat Millage and Vienna Saccomanno
### Release date (v1): July 2019
### Release date (v2): 
### 
### This script contains the the server logic of the app
### --------------------------------------------------------------------

shinyServer(function(input, output, session) {
  
  ### ----------------------
  ### Navigation buttons ---
  ### ----------------------
  
  ### Introduction ---
  
  # Navigation button to selected results
  observeEvent(input$ab_selected_results, {
    updateTabItems(session, "menu_items", "selected-results")
  })
  
  # Navigation button to more information
  observeEvent(input$ab_methods_process, {
    updateTabItems(session, "menu_items", "methods-process")
  })
  
  # Navigation button to fisheries subsidies background
  observeEvent(input$ab_global_subsidies, {
    updateTabItems(session, "menu_items", "global-subsidies")
  })
  
  # Navigation link to help 
  observeEvent(input$al_need_help, {
    updateTabItems(session, "menu_items", "need-help")
  })

})
