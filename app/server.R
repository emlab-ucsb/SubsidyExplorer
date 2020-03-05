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
  
  ### -----------------------
  ### 03. methods-process ---
  ### -----------------------
  
  ### Navigation buttons ---------------------
  
  # Navigation button from methods-process to introduction
  observeEvent(input$ab_methods_process_to_introduction, {
    updateTabItems(session, "menu_items", "introduction")
  })
  
  ### Downlaod buttons ---------------------
  
  # Download methods PDF
  output$db_download_methods <- downloadHandler(
    filename = "SubsidyExplorer_methods.pdf",
    content = function(file) {
      file.copy("www/SubsidyExplorer_methods.pdf", file)
    }
  )
  
  
  ### -------------------------
  ### 04a. global-subsidies ---
  ### -------------------------
  
  ### Navigation buttons ---------------------
  
  # Navigation button from global-subsidies to country-fishery-statistics
  observeEvent(input$ab_global_subsidies_to_country_fishery_stats, {
    updateTabItems(session, "menu_items", "country-fishery-stats")
  })
  
  # Navigation button from global-subsidies to introduction
  observeEvent(input$ab_global_subsidies_to_introduction, {
    updateTabItems(session, "menu_items", "introduction")
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
  
  # Navigation button from country-fishery-stats to introduction
  observeEvent(input$ab_country_fishery_stats_to_introduction, {
    updateTabItems(session, "menu_items", "introduction")
  })
  
  ### Plotly figure: Fisheries subsidies by type ---------------------
  #[NEED]
  
  ### Plotly figure: FAO Marine Capture Production ---------------------
  #[NEED]
  
  ### ------------------------------
  ### 04c. compare-fishery-stats ---
  ### ------------------------------
  
  ### Navigation buttons ---------------------
  
  # Navigation button from compare-fishery-stats to country-fishery-stats
  observeEvent(input$ab_compare_fishery_stats_to_country_fishery_stats, {
    updateTabItems(session, "menu_items", "country-fishery-stats")
  })
  
  # Navigation button from compare-fishery-stats to global-fishing-footprint
  observeEvent(input$ab_compare_fishery_stats_to_global_fishing_footprint, {
    updateTabItems(session, "menu_items", "global-fishing-footprint")
  })
  
  # Navigation button from compare-fishery-stats to introduction
  observeEvent(input$ab_compare_fishery_stats_to_introduction, {
    updateTabItems(session, "menu_items", "introduction")
  })
  
  ### Plotly figure: Compare fishery stats ---------------------
  #[NEED]
  
  ### ---------------------------------
  ### 04d. global-fishing-footprint ---
  ### ---------------------------------
  
  ### Navigation buttons ---------------------
  
  # Navigation button from global-fishing-footprint to compare-fishery-stats
  observeEvent(input$ab_global_fishing_footprint_to_compare_fishery_stats, {
    updateTabItems(session, "menu_items", "compare-fishery-stats")
  })
  
  # Navigation button from global-fishing-footprint to introduction
  observeEvent(input$ab_global_fishing_footprint_to_introduction, {
    updateTabItems(session, "menu_items", "introduction")
  })
  

  ### -----------------
  ### 05. need-help ---
  ### -----------------
  
  ### Navigation buttons ---------------------
  
  # Navigation button from methods-process to introduction
  observeEvent(input$ab_need_help_to_introduction, {
    updateTabItems(session, "menu_items", "introduction")
  })
  
  ### Download buttons ---------------------
  
  # Download user guide (english) PDF
  output$db_download_user_guide_english <- downloadHandler(
    filename = "SubsidyExplorer_user_guide_english.pdf",
    content = function(file) {
      file.copy("www/SubsidyExplorer_user_guide_english.pdf", file)
    }
  )
  

})
