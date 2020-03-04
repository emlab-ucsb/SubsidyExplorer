### -------------------------------------------------------------------
### SubsidyExplorer
### An interactive toolkit to explore the tradeoffs across proposed fisheries subsidies disciplines
### 
### This script contains the content for the country-fishery-stats tab
### --------------------------------------------------------------------

CountryFisheryStats = function(country_choices) 
  fluidPage(
    
    # Page style
    style = "background-color: #ffffff; color: #000000; padding-bottom: 40px; border-bottom: 10px solid #3c8dbc;",
    
    ### Navigation buttons
    column(12,
           fluidRow(style = "padding: 5px 5px; background-color: #3c8dbc;",
                    
                    # Back to global fisheries subsidies
                    column(3,
                           tags$button(id = "ab_country_fishery_stats_to_global_subsidies",
                                       class = "btn action-button nav-button-l",
                                       icon("chevron-left"), button_text$text[button_text$id == "ab_country_fishery_stats_to_global_subsidies"]
                           )
                    ),
                    
                    # Next to fishery profiles button
                    column(3, offset = 6,
                           tags$button(id = "ab_country_fishery_stats_to_compare_fishery_stats",
                                       class = "btn action-button nav-button-r",
                                       button_text$text[button_text$id == "ab_country_fishery_stats_to_compare_fishery_stats"], icon("chevron-right")
                           )
                    )
                    
           )
    ),
    
    ### Title and introductory text                                                  
    column(12, style = "padding: 15px 25px 0px;",
           
           # Title
           tags$h3(style = "padding: 0; margin: 0;", "View Fishery Profiles by State"),
           
           # Text
           includeHTML("./text/04b_country_fishery_stats_intro.html"),
           
           # Space
           br(),
           
           # Select state
           selectizeInput("country_profile_selected_country",
                          label = tagList(tags$b(" WTO Member or Observer:   "),
                                          
                                          # Info button: subsidy types
                                          tags$button(id = "territory_info",
                                                      class = "btn action-button info-button",
                                                      icon("info")
                                          )
                          ), 
                          choices = country_choices,
                          selected = NULL,
                          width = "50%",
                          options = list(placeholder = 'Select...')
           )
           
    ), # /column
    
    
            
    
    
  ) # /fluidPage
  
  
