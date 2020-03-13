### -------------------------------------------------------------------
### SubsidyExplorer
### An interactive toolkit to explore the tradeoffs across proposed fisheries subsidies disciplines
### 
### This script contains the content for the global-subsidies tab
### --------------------------------------------------------------------

GlobalSubsidies = function(subsidy_categories_sorted_sumaila, subsidy_types_sorted_sumaila) 
  fluidPage(
    
    # Page style
    style = "background-color: #262626; color: #ffffff;",
    
    ### Top navigation button
    column(12,
           fluidRow(style = "padding: 5px 5px; background-color: #3c8dbc;",
                    
                    # Return to main menu
                    column(3,
                           tags$button(id = "ab_global_subsidies_to_introduction",
                                       class = "btn action-button nav-button-l",
                                       icon("undo"), text$item_label[text$item_id == "ab_global_subsidies_to_introduction"]
                           )
                    )
                    
           )
    ),
    
    ### Title and introductory text 
    column(12, style = "padding: 25px 25px;",
           
           # Title
           tags$h3(style = "text-align: left; padding: 0; margin: 0 0 10px; color: #ffffff;", text$item_label[text$item_id == "global-subsidies"]),
           
           # Text
           includeHTML("./text/04a_global_subsidies_intro.html"),
           
           fluidRow(
             
             # Select category of subsidies to plot
             column(6, style = "padding: 0 12.5px 0 0;",
                    
                    selectInput("w_global_subsidies_category",
                                label = tags$b(text$item_label[text$item_id == "w_global_subsidies_category"]),
                                choices = setNames(c("All", subsidy_categories_sorted_sumaila), c("All subsidies", 
                                                                                                  "Beneficial subsidies only", 
                                                                                                  "Capacity-enhancing subsidies only",
                                                                                                  "Ambiguous subsidies only")),
                                selected = "Capacity-enhancing",
                                width = "100%")
             ),
             
             # Select subsidy type(s) within selected category to plot
             column(6, style = "padding: 0 0 0 12.5px;",
                    
                    conditionalPanel("input.w_global_subsidies_category != 'All'",
                                     
                    selectizeInput("w_global_subsidies_types",
                                   label = tagList(tags$b(text$item_label[text$item_id == "w_global_subsidies_types"]),
                                                   # Info button
                                                   tags$button(id = "info_subsidy_type_to_plot",
                                                               class = "btn action-button info-button",
                                                               icon("info"))),
                                   
                                   choices = subsidy_types_sorted_sumaila,
                                   width = "100%",
                                   multiple = T)
                    
                    ) # /conditionalPanel
             )
           )
           
    ),
    
    ### Global subsidies map
    column(12, style = "padding: 10px 0;",
           
           
           # Leaflet map
           leafletOutput('global_subsidies_map', width = "auto", height = "70vh")
        
           
    ),
    
    ### Map disclaimer
    column(12, style = "padding: 10px 25px; color: #ffffff;",
           
           includeHTML("./text/00_map_disclaimer.html")
    ),
    
    ### Bottom navigation buttons
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
    )
            
  ) # /fluidPage
  
  
