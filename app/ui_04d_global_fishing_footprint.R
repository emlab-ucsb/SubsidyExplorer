### -------------------------------------------------------------------
### SubsidyExplorer
### An interactive toolkit to explore the tradeoffs across proposed fisheries subsidies disciplines
### 
### This script contains the content for the global-fishing-footprint tab
### --------------------------------------------------------------------

GlobalFishingFootprint = function() 
  fluidPage(
    
    # Page style
    style = "background-color: #262626; color: #ffffff;",
    
    ### Top navigation button
    column(12,
           fluidRow(style = "padding: 5px 5px; background-color: #3c8dbc;",
                    
                    # Return to main menu
                    column(3,
                           tags$button(id = "ab_global_fishing_footprint_to_introduction",
                                       class = "btn action-button nav-button-l",
                                       icon("undo"), text$item_label[text$item_id == "ab_global_fishing_footprint_to_introduction"]
                           )
                    )
                    
           )
    ),
    
    ### Title and introductory text 
    column(12, style = "padding: 25px 25px;",
           
           # Title
           tags$h3(style = "text-align: left; padding: 0; margin: 0 0 10px; color: #ffffff;", text$item_label[text$item_id == "global-fishing-footprint"], tags$button(id = "info_global_fishing_footprint",
                                                                                                                                 class = "btn action-button info-button",
                                                                                                                                 icon("info"))),
           
           # Text
           includeHTML("./text/04d_global_fishing_footprint_intro.html")
           
    ),
    
    ### Global fishing effort map (PLACEHOLDER - change back to leaflet object)
    column(12, style = "padding: 10px 0;",
           
           
           # Leaflet map
           #leafletOutput('global_subsidies_map', width = "auto", height = "70vh")
           
           # Temp image
           img(src = "/sample-images/04d_map_image.png", width = "100%")
           
    ),
    
    ### Map disclaimer
    column(12, style = "padding: 25px 25px; color: #ffffff;",
           
           tags$i(text$item_label[text$item_id == "map_disclaimer"])
    ),
    
    ### Bottom navigation buttons
    column(12,
           fluidRow(style = "padding: 5px 5px; background-color: #3c8dbc;",
                    
                    # Back to compare fishery stats
                    column(3,
                           tags$button(id = "ab_global_fishing_footprint_to_compare_fishery_stats",
                                       class = "btn action-button nav-button-l",
                                       icon("chevron-left"), text$item_label[text$item_id == "ab_global_fishing_footprint_to_compare_fishery_stats"]
                           )
                    )
                    
           )
    )
            
  ) # /fluidPage
  
  
