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
  
  ### --------------------
  ### 01. Introduction ---
  ### --------------------
  
  ### Navigation buttons ---------------------
  
  # Navigation button from introduction to selected-results
  observeEvent(input$ab_introduction_to_selected_results, {
    updateTabItems(session, "menu_items", "selected-results")
  })
  
  # Navigation button from introduction to methods-process
  observeEvent(input$ab_introduction_to_methods_process, {
    updateTabItems(session, "menu_items", "methods-process")
  })
  
  # Navigation button from introduction to global-subsidies
  observeEvent(input$ab_introduction_to_global_subsidies, {
    updateTabItems(session, "menu_items", "global-subsidies")
  })
  
  # Navigation link from introduction to need-help 
  observeEvent(input$al_introduction_to_need_help, {
    updateTabItems(session, "menu_items", "need-help")
  })
  
  ### -------------------------
  ### 04a. global-subsidies ---
  ### -------------------------
  
  ### Navigation buttons ---------------------
  
  # Navigation button from global-subsidies to introduction
  observeEvent(input$ab_global_subsidies_to_introduction, {
    updateTabItems(session, "menu_items", "introduction")
  })
  
  # Navigation button from global-subsidies to country-fishery-statistics
  observeEvent(input$ab_global_subsidies_to_country_fishery_stats, {
    updateTabItems(session, "menu_items", "country-fishery-stats")
  })
  
  ### Leaflet map: Global map of fisheries subsidies with hover boxes ---------------------
  
  # output$global_subsidies_map <- renderLeaflet({})
  

  
  ### ------------------------------
  ### 04b. country-fishery-stats ---
  ### ------------------------------
  
  ### Navigation buttons ---------------------
  
  # Navigation button from country-fishery-stats to global-subsidies
  observeEvent(input$ab_country_fishery_stats_to_global_subsidies, {
    updateTabItems(session, "menu_items", "global-subsidies")
  })
  
  # Navigation button from country-fishery-stats to compare-fishery-statistics
  observeEvent(input$ab_country_fishery_stats_to_compare_fishery_stats, {
    updateTabItems(session, "menu_items", "compare-fishery-stats")
  })
  
  ### Leaflet map: Global map of fisheries subsidies with hover boxes ---------------------
  
  # output$global_subsidies_map <- renderLeaflet({})
  


})
