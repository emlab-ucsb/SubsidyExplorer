### -------------------------------------------------------------------
### SubsidyExplorer
### An interactive toolkit to explore the tradeoffs across proposed fisheries subsidies disciplines
### 
### This script contains the content for the global-subsidies tab
### --------------------------------------------------------------------

GlobalSubsidies = function() 
  fluidPage(
    
    # Page style
    style = "background-color: #262626; color: #ffffff;",
    
    ### Top navigation buttons
    column(12,
           fluidRow(style = "padding: 5px 5px; background-color: #3c8dbc;",
                    
                    # Next to country-fishery-stats
                    column(3, offset = 9,
                           tags$button(id = "ab_global_subsidies_to_country_fishery_stats",
                                       class = "btn action-button nav-button-r",
                                       button_text$text[button_text$id == "ab_global_subsidies_to_country_fishery_stats"], icon("chevron-right")
                           )
                    )
                    
           )
    ),
    
    ### Title and introductory text 
    column(12, style = "padding: 25px 25px;",
           
           # Title
           tags$h3(style = "padding: 0; margin: 0 0 10px;", tab_text$tab[tab_text$id == "global-subsidies"]),
           
           # Text
           includeHTML("./text/04a_global_subsidies_intro.html"),
           
           # Select subsidy type(s) to plot (change choices back to "subsidy_types_all")
           selectizeInput("w_global_subsidies_included_subsidy_types",
                          label = tagList(tags$b(" Subsidy type(s) to include:    "),
                                          # Info button
                                          tags$button(id = "info_subsidy_type_to_plot",
                                                      class = "btn action-button info-button",
                                                      icon("info"))),
                          
                          choices = c("choice1" = "B1", "choice2" = "B2", "etc" = "B3"),
                          selected = 'B1',
                          width = "100%",
                          multiple = T)
           
           
    ),
    
    ### Global subsidies map (PLACEHOLDER - change back to leaflet object)
    column(12, style = "padding: 10px 0;",
           
           
           # Leaflet map
           #leafletOutput('global_subsidies_map', width = "auto", height = "70vh")
           
           # Temp image
           img(src = "/sample-images/04a_map_image.png", width = "100%")
           
    ),
    
    ### Map disclaimer
    column(12, style = "padding: 10px 25px; color: #ffffff;",
           
           includeHTML("./text/00_map_disclaimer.html")
    ),
    
    ### Bottom navigation buttons
    column(12,
           fluidRow(style = "padding: 5px 5px; background-color: #3c8dbc;",
                    
                    # Return to main menu
                    column(3,
                           tags$button(id = "ab_global_subsidies_to_introduction",
                                       class = "btn action-button nav-button-l",
                                       icon("undo"), button_text$text[button_text$id == "ab_global_subsidies_to_introduction"]
                           )
                    )
                    
           )
    )
            
  ) # /fluidPage
  
  
