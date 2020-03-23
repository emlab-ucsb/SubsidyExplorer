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
    
    ### Top Navigation button
    column(12,
           fluidRow(style = "padding: 5px 5px; background-color: #3c8dbc;",
                    
                    # Return to main menu
                    column(3,
                           tags$button(id = "ab_country_fishery_stats_to_introduction",
                                       class = "btn action-button nav-button-l",
                                       icon("undo"), text$item_label[text$item_id == "ab_country_fishery_stats_to_introduction"]
                           )
                    )
                    
           )
    ),
    
    ### Title and introductory text                                                  
    column(12, style = "padding: 25px 25px 0px;",
           
           # Title
           tags$h3(style = "text-align: left; padding: 0; margin: 0;", text$item_label[text$item_id == "country-fishery-stats"]),
           
           # Text
           includeHTML("./text/04b_country_fishery_stats_intro.html"),
           
           # Select state
           selectizeInput("w_country_fishery_stats_selected_country",
                          label = tagList(tags$b(text$item_label[text$item_id == "w_country_fishery_stats_selected_country"]),
                                          
                                          # Info button: subsidy types
                                          tags$button(id = "territory_info",
                                                      class = "btn action-button info-button",
                                                      icon("info")
                                          )
                          ), 
                          choices = wto_members_and_observers,
                          selected = "",
                          width = "50%",
                          options = list(placeholder = 'Select...')
           )
           
    ), # /column 12
    
    ### Section 1: Fishery subsidies
    column(12, style = "padding: 10px 25px;",
           
           # Header and text
           includeHTML("./text/04b_country_fishery_stats_section1.html"),
           
           #Fishery subsidies bar plot
           plotlyOutput("country_fishery_stats_subsidies_plot")
           
    ),
    
    ### Section 2: Capture fisheries production                                                     
    column(12, style = "padding: 10px 25px;",
           
           # Header and text
           includeHTML("./text/04b_country_fishery_stats_section2.html"),
           
           # Capture fisheries production plot
           plotlyOutput("country_fishery_stats_production_plot"),
           
           # Total landed value plot
           plotlyOutput("country_fishery_stats_landed_value_plot")
          
           
    ),
    
    ### Section 3: Demographics                                                  
    column(12, style = "padding: 10px 25px;",
           
           # Header and text
           includeHTML("./text/04b_country_fishery_stats_section3.html"),
           
           # Top row of demographic plots
           fluidRow(
             
             column(6,
                    # Population plot
                    plotlyOutput("country_fishery_stats_pop_plot")
             ),
             
             column(6,
                    # Fishers plot
                    plotlyOutput("country_fishery_stats_fisher_plot")
                    
             )
           ),
           
           # Bottom row of demographic plots
           fluidRow(
             
             column(12,
                    # GDP plot
                    plotlyOutput("country_fishery_stats_gdp_plot")
             )
           )
    ),
    
    ### Bottom navigation buttons
    column(12,
           fluidRow(style = "padding: 5px 5px; background-color: #3c8dbc;",
                    
                    # Back to global-subsidies
                    column(3,
                           tags$button(id = "ab_country_fishery_stats_to_global_subsidies",
                                       class = "btn action-button nav-button-l",
                                       icon("chevron-left"), text$item_label[text$item_id == "ab_country_fishery_stats_to_global_subsidies"]
                           )
                    ),
                    
                    # Next to compare-fishery-stats
                    column(3, offset = 6,
                           tags$button(id = "ab_country_fishery_stats_to_compare_fishery_stats",
                                       class = "btn action-button nav-button-r",
                                       text$item_label[text$item_id == "ab_country_fishery_stats_to_compare_fishery_stats"], icon("chevron-right")
                           )
                    )
                    
           )
    )
    
  ) # /fluidPage
  
  
