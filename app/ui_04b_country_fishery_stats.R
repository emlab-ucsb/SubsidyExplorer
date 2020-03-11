### -------------------------------------------------------------------
### SubsidyExplorer
### An interactive toolkit to explore the tradeoffs across proposed fisheries subsidies disciplines
### 
### This script contains the content for the country-fishery-stats tab
### --------------------------------------------------------------------

CountryFisheryStats = function(wto_members_and_observers) 
  fluidPage(
    
    # Page style
    style = "background-color: #ffffff; color: #000000;",
    
    ### Navigation buttons
    column(12,
           fluidRow(style = "padding: 5px 5px; background-color: #3c8dbc;",
                    
                    # Back to global-subsidies
                    column(3,
                           tags$button(id = "ab_country_fishery_stats_to_global_subsidies",
                                       class = "btn action-button nav-button-l",
                                       icon("chevron-left"), button_text$text[button_text$id == "ab_country_fishery_stats_to_global_subsidies"]
                           )
                    ),
                    
                    # Next to compare-fishery-stats
                    column(3, offset = 6,
                           tags$button(id = "ab_country_fishery_stats_to_compare_fishery_stats",
                                       class = "btn action-button nav-button-r",
                                       button_text$text[button_text$id == "ab_country_fishery_stats_to_compare_fishery_stats"], icon("chevron-right")
                           )
                    )
                    
           )
    ),
    
    ### Title and introductory text                                                  
    column(12, style = "padding: 25px 25px 0px;",
           
           # Title
           tags$h3(style = "padding: 0; margin: 0;", tab_text$tab[tab_text$id == "country-fishery-stats"]),
           
           # Text
           includeHTML("./text/04b_country_fishery_stats_intro.html"),
           
           # Select state
           selectizeInput("w_country_fishery_stats_selected_country",
                          label = tagList(tags$b(" WTO Member or Observer:   "),
                                          
                                          # Info button: subsidy types
                                          tags$button(id = "territory_info",
                                                      class = "btn action-button info-button",
                                                      icon("info")
                                          )
                          ), 
                          choices = wto_members_and_observers,
                          selected = NULL,
                          width = "50%",
                          options = list(placeholder = 'Select...')
           )
           
    ), # /column 12
    
    ### Section 1: Fishery subsidies
    column(12, style = "padding: 10px 25px;",
           
           # Header and text
           includeHTML("./text/04b_country_fishery_stats_section1.html"),
           
           # Fishery subsidies bar plot (PLACEHOLDER - REPLACE WITH PLOTLY OBJECT)
           #plotlyOutput("country_profile_bar_plot")
           
           # Temp image
           img(src = "/sample-images/04b_subsidy_plot_image.png", width = "100%")
           
    ),
    
    ### Section 2: Capture fisheries production                                                     
    column(12, style = "padding: 10px 25px;",
           
           # Header and text
           includeHTML("./text/04b_country_fishery_stats_section2.html"),
           
           # Capture fisheries production line plot (PLACEHOLDER - REPLACE WITH PLOTLY OBJECT)
           #plotlyOutput("country_profile_landings_plot")
           
           # Temp image
           img(src = "/sample-images/04b_capture_production_plot_image.png", width = "100%")
           
    ),
    
    ### Bottom navigation buttons
    column(12,
           fluidRow(style = "padding: 5px 5px; background-color: #3c8dbc;",
                    
                    # Return to main menu
                    column(3,
                           tags$button(id = "ab_country_fishery_stats_to_introduction",
                                       class = "btn action-button nav-button-l",
                                       icon("undo"), button_text$text[button_text$id == "ab_country_fishery_stats_to_introduction"]
                           )
                    )
                    
           )
    )
    
  ) # /fluidPage
  
  
