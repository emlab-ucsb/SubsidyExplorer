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
    
    ### Page Header
    column(12, id = "page-title-div-underline-blue",
           
           # Title
           tags$h3(text$item_label[text$item_id == "country-fishery-stats"])
           
    ),
    
    ### Main content
    column(12,
           
           fluidRow(
             
             ####---------------------------------------------
             #### Left section - select a country
             #### --------------------------------------------
             column(3, id = "explore-results-left-column",
                  
                    ### Widgets
                    column(12, id = "spaced-div",
                           
                           # # Text
                           # includeHTML("./text/04b_country_fishery_stats_intro.html"),
                           
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
                                          options = list(placeholder = 'Select...'))
                           
                    )
             ),
             
             ####---------------------------------------------
             #### Right section - plots
             #### --------------------------------------------
             column(9,
                    
                    ### Section Title ---
                    column(12,
                           
                           tags$h3("Country Name")
                           
                    ),
                    
                    # tabBox
                    tabBox(width = 12, id = "country-tabs", 
                           
                           ### --------------------------
                           ### Tab # 1  - Fishery Subsidies
                           ### --------------------------
                           
                           tabPanel(value = "fishery-subsidy-tab",
                                    
                                    # Title
                                    tags$h4(text$item_label[text$item_id == "fishery-subsidy-tab"]),
                                    
                                    # Header and text
                                    includeHTML("./text/04b_country_fishery_stats_section1.html"),
                                    
                                    #Fishery subsidies bar plot
                                    plotlyOutput("country_fishery_stats_subsidies_plot")
                                            
                           ),
                           
                           ### --------------------------
                           ### Tab # 2  - Fishery Subsidies
                           ### --------------------------
                           
                           tabPanel(value = "marine-capture-tab",
                                    
                                    # Title
                                    tags$h4(text$item_label[text$item_id == "marine-capture-tab"]),
                                    
                                    # Header and text
                                    includeHTML("./text/04b_country_fishery_stats_section2.html"),
                                    
                                    # Capture fisheries production plot
                                    plotlyOutput("country_fishery_stats_production_plot"),
                                    
                                    # Total landed value plot
                                    plotlyOutput("country_fishery_stats_landed_value_plot")
                                    
                           ),
                           
                           ### --------------------------
                           ### Tab # 3  - Demographics
                           ### --------------------------
                           
                           tabPanel(value = "demographic-tab",
                                    
                                    # Title
                                    tags$h4(text$item_label[text$item_id == "demographic-tab"]),
                                    
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
                                    
                           )
                    )
             )
           )
    )
      
) # /fluidPage
  
  
