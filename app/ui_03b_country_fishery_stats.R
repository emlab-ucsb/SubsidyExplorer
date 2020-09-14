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
    
    ### Main content
    column(12,
           
           fluidRow(
             
             ####---------------------------------------------
             #### Left section - select a country
             #### --------------------------------------------
             column(4, id = "country-fishery-stats-left-column",
                  
                    ### Widget
                    column(12, id = "spaced-div",
                           
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
                                          selected = "USA",
                                          width = "100%",
                                          options = list(placeholder = 'Select...'))
                           
                    ),
                    
                    ### Download button
                    column(12, id = "spaced-div",
                           
                           # Button to save PDF of data for selected state
                           downloadButton("db_country_fishery_stats_generate_report",
                                          text$item_label[text$item_id == "db_country_fishery_stats_generate_report"])
                           
                           
                    )
                    
                    
             ),
             
             ####---------------------------------------------
             #### Right section - plots
             #### --------------------------------------------
             column(8, offset = 4, id = "country-fishery-stats-right-column",
                    
                    ### Section Title ---
                    column(12, id = "tb-spaced-div",
                           
                           uiOutput("country_fishery_stats_selected_country_name")
                           #tags$h3("Country Name")
                           
                    ),
                    
                    # tabBox
                    tabBox(width = 12, id = "country-tabs", 
                           
                           ### --------------------------
                           ### Tab # 1  - Fishery Subsidies
                           ### --------------------------
                           
                           tabPanel(value = "fishery-subsidy-tab",
                                    
                                    # Title
                                    tags$h4(text$item_label[text$item_id == "fishery-subsidy-tab"]),
                                    
                                    column(12, id = "lr-spaced-div",
                                           
                                           column(12, id = "tb-spaced-div",
                                                  
                                                  # Header and text
                                                  includeHTML("./text/03-more-about-subsidies/fisheries_subsidies.html")),
                                           
                                           column(12, id = "tb-spaced-div",
                                                  
                                                  #Fishery subsidies bar plot
                                                  plotlyOutput("country_fishery_stats_subsidies_plot")
                                                  
                                           )
                                    )
                                            
                           ),
                           
                           ### --------------------------
                           ### Tab # 2  - Fishery Subsidies
                           ### --------------------------
                           
                           tabPanel(value = "marine-capture-tab",
                                    
                                    # Title
                                    tags$h4(text$item_label[text$item_id == "marine-capture-tab"]),
                                    
                                    column(12, id = "lr-spaced-div",
                                           
                                           column(12, id = "tb-spaced-div",
                                                  
                                                  # Header and text
                                                  includeHTML("./text/03-more-about-subsidies/marine_capture.html")
                                                  
                                           ), 
                                    
                                           column(12, id = "tb-spaced-div",
                                                  
                                                  # Capture fisheries production plot
                                                  plotlyOutput("country_fishery_stats_production_plot"),
                                                  
                                                  # Total landed value plot
                                                  plotlyOutput("country_fishery_stats_landed_value_plot")
                                                  
                                           )
                                    )
                                    
                           ),
                           
                           ### --------------------------
                           ### Tab # 3  - Demographics
                           ### --------------------------
                           
                           tabPanel(value = "demographic-tab",
                                    
                                    # Title
                                    tags$h4(text$item_label[text$item_id == "demographic-tab"]),
                                    
                                    column(12, id = "lr-spaced-div",
                                           
                                           column(12, id = "tb-spaced-div",
                                                  
                                                  # Header and text
                                                  includeHTML("./text/03-more-about-subsidies/demographics.html")
                                                  
                                           ),
                                    
                                           column(12, id = "tb-spaced-div",
                                                  
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
                                             
                                                    
                                                  ), # /fluidRow
                                    
                                                  # Bottom row of demographic plots
                                                  fluidRow(
                                      
                                                    column(12,
                                                           # GDP plot
                                                           plotlyOutput("country_fishery_stats_gdp_plot")
                                             
                                                           
                                                    )
                                      
                                                  ) # /fluidRow
                                    
                                           ) #/column 12
                                           
                                    ) # /column 12
                           )
                    )
             )
           )
    )
      
) # /fluidPage
  
  
