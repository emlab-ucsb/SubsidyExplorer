### -------------------------------------------------------------------
### SubsidyExplorer
### An interactive toolkit to explore the tradeoffs across proposed fisheries subsidies disciplines
### 
### This script contains the content for the compare-fishery-stats tab
### --------------------------------------------------------------------

CompareFisheryStats = function() 
  fluidPage(
    
    # Page style
    style = "background-color: #ffffff; color: #000000;",
    
    ### Top navigation buttons
    column(12,
           fluidRow(style = "padding: 5px 5px; background-color: #3c8dbc;",
                    
                    # Back to country-fishery-stats
                    column(3,
                           tags$button(id = "ab_compare_fishery_stats_to_country_fishery_stats",
                                       class = "btn action-button nav-button-l",
                                       icon("chevron-left"), button_text$text[button_text$id == "ab_compare_fishery_stats_to_country_fishery_stats"]
                           )
                    ),
                    
                    # Next to global-fishing-footprint
                    column(3, offset = 6,
                           tags$button(id = "ab_compare_fishery_stats_to_global_fishing_footprint",
                                       class = "btn action-button nav-button-r",
                                       button_text$text[button_text$id == "ab_compare_fishery_stats_to_global_fishing_footprint"], icon("chevron-right")
                           )
                    )
                    
           )
    ),
    
    ### Bottom navigation buttons
    column(12,
           fluidRow(style = "padding: 5px 5px; background-color: #3c8dbc;",
                    
                    # Return to main menu
                    column(3,
                           tags$button(id = "ab_compare_fishery_stats_to_introduction",
                                       class = "btn action-button nav-button-l",
                                       icon("undo"), button_text$text[button_text$id == "ab_compare_fishery_stats_to_introduction"]
                           )
                    )
                    
           )
    )
            
  ) # /fluidPage
  
  
