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
  ### 02a. selected-results ---
  ### -------------------------
  
  ### Navigation buttons ---------------------
  
  # Navigation button from selected-results to edit-policies
  observeEvent(input$ab_selected_results_to_edit_policies, {
    updateTabItems(session, "menu_items", "edit-policies")
  })
  
  # Navigation button from selected-results to introduction
  observeEvent(input$ab_selected_results_to_introduction, {
    updateTabItems(session, "menu_items", "introduction")
  })
  
  ### Plotly figure: Model results over time ---------------------
  #[NEED]
  
  ### ----------------------
  ### 02b. edit-policies ---
  ### ----------------------
  
  ### Navigation buttons ---------------------
  
  # Navigation button from edit-policies to selected-results
  observeEvent(input$ab_edit_policies_to_selected_results, {
    updateTabItems(session, "menu_items", "selected-results")
  })
  
  # Navigation button from edit-policies to introduction
  observeEvent(input$ab_edit_policies_to_introduction, {
    updateTabItems(session, "menu_items", "introduction")
  })
  
  ### Tabs
  # Navigation button from tab 0 to tab 1
  observeEvent(input$ab_edit_policies_tabs_instructions_to_iuu, {
    updateTabsetPanel(session, "policy_tabs", "iuu") 
  })
  
  # Navigation button from tab 1 to tab 0
  observeEvent(input$ab_edit_policies_tabs_iuu_to_instructions, {
    updateTabsetPanel(session, "policy_tabs", "instructions") 
  })
  
  # Navigation button from tab 1 to tab 2
  observeEvent(input$ab_edit_policies_tabs_iuu_to_oa, {
    updateTabsetPanel(session, "policy_tabs", "oa") 
  })
  
  # Navigation button from tab 2 to tab 1
  observeEvent(input$ab_edit_policies_tabs_oa_to_iuu, {
    updateTabsetPanel(session, "policy_tabs", "iuu") 
  })
  
  # Navigation button from tab 2 to tab 3
  observeEvent(input$ab_edit_policies_tabs_oa_to_overcap, {
    updateTabsetPanel(session, "policy_tabs", "overcap") 
  })
  
  # Navigation button from tab 3 to tab 2
  observeEvent(input$ab_edit_policies_tabs_overcap_to_oa, {
    updateTabsetPanel(session, "policy_tabs", "overcap") 
  })
  
  # Navigation button from tab 3 to tab 4
  observeEvent(input$ab_edit_policies_tabs_overcap_to_preview, {
    updateTabsetPanel(session, "policy_tabs", "preview") 
  })
  
  ### Text Output: IUU data warning -------------------
  output$o_iuu_warning <- renderText({
    
    if("iuu2" %in% input$iuu_definitions | "iuu3" %in% input$iuu_definitions | "iuu4" %in% input$iuu_definitions){
      "Note: At present, no data exists on a global scale to identify vessels listed as having engaged in IUU fishing activities by a coastal state, flag state, or subsidizing Member state."
    }else{
      ""
    }
  })
  
  
  
  ### Reactive data frame: All select policy inputs -------------
  
  
  
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
  
  ### Update selectInput: Only allow for selection of subsidy types from selected category ---------------------
  observe({
    
    # Only allow types from the selected category to be chosen
    new_choices <- subsidy_classification_sumaila$type[subsidy_classification_sumaila$category == input$w_global_subsidies_category]
    names(new_choices) <- subsidy_classification_sumaila$type_name[subsidy_classification_sumaila$category == input$w_global_subsidies_category]
    
    # Update input
    updateSelectizeInput(session, 
                         "w_global_subsidies_types",
                         choices = new_choices,
                         selected = new_choices[1])
  })
  
  
  ### Leaflet map: Global map of fisheries subsidies with hover boxes ---------------------

  output$global_subsidies_map <- renderLeaflet({
    
    req(input$w_global_subsidies_category)
    req(input$w_global_subsidies_types)
    
    # Color palette to use based off selected input
    global_subsidies_map_color <- switch(input$w_global_subsidies_category,
                                         "A" = list("Blues", 1, 2.2e9),
                                         "B" = list("Reds", 1, 6e9),
                                         "C" = list("Purples", 1, 1e9))
    
    global_subsidies_map_pal <- colorNumeric(palette = global_subsidies_map_color[[1]],
                                             log10(c(global_subsidies_map_color[[2]], global_subsidies_map_color[[3]])))
    
    # Filter data
    global_subsidies_map_dat <- subsidy_dat %>%
      dplyr::filter(variable == "subsidies_Sumaila") %>%
      dplyr::filter(type %in% c(input$w_global_subsidies_types)) %>%
      dplyr::filter(!is.na(value) & value > 0) %>%
      group_by(iso3, display_name, category, category_name, type, type_name) %>%
      summarize(value = sum(value, na.rm = T)) %>%
      group_by(iso3, display_name) %>%
      mutate(included_types = paste0(type_name[type_name != "Total"], collapse = ";</br>")) %>%
      ungroup() %>%
      group_by(iso3, display_name, category, category_name, included_types) %>%
      summarize(value = sum(value, na.rm = T))
      
    # Join to world polygons
    global_subsidies_map_dat_shp <- world %>%
      dplyr::filter(!admin_iso3 %in% eu_countries) %>%
      left_join(global_subsidies_map_dat, by = c("admin_iso3" = "iso3")) %>%
      na.omit()
    
    # Hover text for world polygons
    global_subsidies_map_text_shp <- paste0(
      "<b>","State: ", "</b>",  global_subsidies_map_dat_shp$display_name,
      "</br>", 
      "<b>", "Subsidy category: ", "</b>", global_subsidies_map_dat_shp$category_name,
      "</br>",
      "<b>", "Est. fisheries subsidies (2018 US$):", "</b>", " $", format(round(global_subsidies_map_dat_shp$value, 0), big.mark = ","),
      "</br>",
      "<b>", "Matching subsidy type(s): ", "</b>", global_subsidies_map_dat_shp$included_types
    ) %>%
      lapply(htmltools::HTML)
    
    # Join to points for small island nations
    global_subsidies_map_dat_points <- world_small_countries %>%
      dplyr::select(sov_iso3, admin_iso3, area_km, center) %>%
      left_join(global_subsidies_map_dat, by = c("admin_iso3" = "iso3")) %>%
      na.omit()
    st_geometry(global_subsidies_map_dat_points) <- global_subsidies_map_dat_points$center
    
    # Hover text for points
    global_subsidies_map_text_points <- paste0(
      "<b>","State: ", "</b>",  global_subsidies_map_dat_points$display_name,
      "</br>", 
      "<b>", "Subsidy category: ", "</b>", global_subsidies_map_dat_points$category_name,
      "</br>",
      "<b>", "Est. fisheries subsidies (2018 US$):", "</b>", " $", format(round(global_subsidies_map_dat_points$value, 0), big.mark = ","),
      "</br>",
      "<b>", "Matching subsidy type(s): ", "</b>", global_subsidies_map_dat_points$included_types
    ) %>%
      lapply(htmltools::HTML)
    
    # Map
    leaflet('global_subsidies_map', options = leafletOptions(minZoom = 2)) %>% 
      addProviderTiles("CartoDB.DarkMatterNoLabels") %>% 
      addCircles(data = global_subsidies_map_dat_points,
                 color = ~global_subsidies_map_pal(log10(value)),
                 fillOpacity = 0.5,
                 stroke = "white",
                 weight = 2,
                 radius = 200000,
                 highlight = highlightOptions(weight = 5,
                                              color = "#666",
                                              fillOpacity = 1,
                                              bringToFront = FALSE),
                 label = global_subsidies_map_text_points,
                 labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                          padding = "3px 8px"),
                                             textsize = "13px",
                                             direction = "auto")) %>%
      addPolygons(data = global_subsidies_map_dat_shp, 
                  fillColor = ~global_subsidies_map_pal(log10(value)),
                  fillOpacity = 0.8,
                  color= "white",
                  weight = 0.3,
                  highlight = highlightOptions(weight = 5,
                                               color = "#666",
                                               fillOpacity = 1,
                                               bringToFront = FALSE),
                  label = global_subsidies_map_text_shp,
                  labelOptions = labelOptions(style = list("font-weight" = "normal",
                                                           padding = "3px 8px"),
                                              textsize = "13px",
                                              direction = "auto")) %>%
      setView(0,20, zoom = 2) %>%
      addLegend("bottomright", 
                pal = global_subsidies_map_pal,
                values = log10(c(global_subsidies_map_color[[2]], global_subsidies_map_color[[3]])),
                labels = round(log10(c(global_subsidies_map_color[[2]], global_subsidies_map_color[[3]])), 0),
                title = "Est. fisheries subsidies<br>(2018 US$)",
                opacity = 1,
                labFormat = labelFormat(prefix = "$",
                                        transform = function(x) 10^(x)
                )
      )
    
  })
  
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
  output$country_fishery_stats_subsidies_plot <- renderPlotly({
    
    req(input$w_country_fishery_stats_selected_country)
    
    # Filter OECD data and add Sumaila data
    country_fishery_stats_subsidies_plot_dat <- subsidy_dat %>%
      dplyr::filter(iso3 == input$w_country_fishery_stats_selected_country)
    country_fishery_stats_subsidies_plot_dat$value[is.na(country_fishery_stats_subsidies_plot_dat$value)] <- 0
    
    # Make plot  
    country_fishery_stats_subsidies_plot <- ggplot()+
      geom_col(data = country_fishery_stats_subsidies_plot_dat, aes(x = source, y = value, fill = type_name, 
                                                        text = paste0("<b>","State: ","</b>", display_name,
                                                                      "<br>",
                                                                      "<b>","Type: ","</b>", type_name,
                                                                      "<br>",
                                                                      "<b>","Data source: ","</b>", source,
                                                                      "<br>",
                                                                      "<b>","Est. fisheries subsidies (US$):","</b>", format(round(value, 0), big.mark = ","),
                                                                      "<br>",
                                                                      "<b>", "Year: ", "</b>", year)))+
      scale_fill_manual(values = myColors[names(myColors) %in% country_fishery_stats_subsidies_plot_dat$type_name])+
      scale_y_continuous(expand = c(0,0), name = "Est. fisheries subsidies (US$)", 
                         labels = function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = FALSE))+
      geom_vline(xintercept = 0, size = 1)+
      coord_flip()+
      theme_bw()+
      labs(x = "")+
      theme(legend.title = element_blank(),
            legend.position = "none",
            axis.text.y = element_text(face = "bold", angle = 90))
    
    # Convert to plotly
    gg <- ggplotly(country_fishery_stats_subsidies_plot, tooltip="text")
    
    # Create legend
    leg <- list(font = list(size = 10, color = "#000"),
                x = 100,
                y = 0.9,
                yanchor = "top")
    
    # Add plotly legend
    gg <- gg %>%
      layout(legend = leg)
    
    # Return plot
    gg
    
  })
  
  
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
