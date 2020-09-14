### -------------------------------------------------------------------
### SubsidyExplorer
### An interactive toolkit to explore the tradeoffs across proposed fisheries subsidies disciplines
### 
### This script contains the content for the compare-fishery-stats tab
### --------------------------------------------------------------------

CompareFisheryStats = function(wto_members_and_observers, subsidy_types_sorted_sumaila) 
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
                    
                    ### Widgets
                    column(12, id = "tb-spaced-div",
                           
                           column(12, id = "lr-spaced-div",
                                  
                                  # Select state
                                  selectizeInput("w_compare_fishery_stats_selected_country",
                                                 label = tagList(tags$b(text$item_label[text$item_id == "w_compare_fishery_stats_selected_country"]),
                                                                 # Info button: subsidy types
                                                                 tags$button(id = "territory_info",
                                                                             class = "btn action-button info-button",
                                                                             icon("info")
                                                                 )
                                                 ),
                                                 choices = wto_members_and_observers,
                                                 selected = "USA",
                                                 width = "100%",
                                                 options = list(placeholder = 'Select...')),
                                  
                                  # Conditional panel - country selected
                                  conditionalPanel(condition = "input.w_compare_fishery_stats_selected_country",
                                                   
                                                   # Input - select statistic to compare
                                                   selectizeInput("w_compare_fishery_stats_plot_variable",
                                                                  label = text$item_label[text$item_id == "w_compare_fishery_stats_plot_variable"],
                                                                  choices = unlist(wid$choices[wid$item_id == "w_compare_fishery_stats_plot_variable"]),
                                                                  selected = unlist(wid$selected[wid$item_id == "w_compare_fishery_stats_plot_variable"]),
                                                                  width = "100%"),
                                                   
                                                   # Input - Select type of comparison
                                                   selectizeInput("w_compare_fishery_stats_method",
                                                                  label = text$item_label[text$item_id == "w_compare_fishery_stats_method"],
                                                                  choices = unlist(wid$choices[wid$item_id == "w_compare_fishery_stats_method"]),
                                                                  selected = unlist(wid$selected[wid$item_id == "w_compare_fishery_stats_method"]),
                                                                  width = "100%"),
                                                   
                                                   # Conditional panel - comparison method is to countries of choice
                                                   conditionalPanel(condition = "input.w_compare_fishery_stats_method == 'select'",
                                                                    
                                                                    # Input: Select states with which to compare
                                                                    selectizeInput("w_compare_fishery_stats_select_manual",
                                                                                   label = text$item_label[text$item_id == "w_compare_fishery_stats_select_manual"],
                                                                                   choices = wto_members_and_observers,
                                                                                   selected = NULL,
                                                                                   width = "100%",
                                                                                   options = list(placeholder = 'Select...',
                                                                                                  maxItems = 10),
                                                                                   multiple = T
                                                                    )
                                                                    
                                                   ), # /conditionalPanel - comparison method is to countries of choice
                                                   
                                                   # Conditional Panel - statistic involves subsidies
                                                   conditionalPanel(condition = "input.w_compare_fishery_stats_plot_variable != 'landings' && input.w_compare_fishery_stats_plot_variable != 'revenue'",
                                                                  
                                                                   # Title 
                                                                   tagList(tags$b(text$item_label[text$item_id == "subsidy-types-to-include"]),
                                                                           # Info button
                                                                           tags$button(id = "info_button_global_subsidies_subsidy_types",
                                                                                       class = "btn action-button info-button",
                                                                                       icon("info"))),
                                                                           
                                                                   # Buttons
                                                                   column(12, id = "tb-spaced-div",
                                                                                  
                                                                          fluidRow(
                                                                                    
                                                                            column(6, style = "padding-right: 5px;",
                                                                                           
                                                                                   # Button to select all
                                                                                   tags$button(id = "ab_compare_fishery_stats_select_all",
                                                                                               
                                                                                               text$item_label[text$item_id == "ab_compare_fishery_stats_select_all"],
                                                                                               class = "btn action-button rounded-button-grey")

                                                                            ),
                                                                                    
                                                                            column(6, style = "padding-left: 5px;",
                                                                              
                                                                                   # Button to clear selection
                                                                                   tags$button(id = "ab_compare_fishery_stats_clear_all",
                                                                                               text$item_label[text$item_id == "ab_compare_fishery_stats_clear_all"],
                                                                                               class = "btn action-button rounded-button-grey")
                                                                              
                                                                            )
                                                                            
                                                                          )
                                                                   ),
                                                                           
                                                                   # Subsidy Type Widgets
                                                                   checkboxGroupInput("w_compare_fishery_stats_good_types",
                                                                                      label =  tags$b(text$item_label[text$item_id == "w_compare_fishery_stats_good_types"], 
                                                                                                      style = paste0("color:", goodColors[1], ";")),
                                                                                      choices = subsidy_types_sorted_sumaila[1:3],
                                                                                      selected = subsidy_types_sorted_sumaila[1:3],
                                                                                      width = "100%"),
                                                                   
                                                                   checkboxGroupInput("w_compare_fishery_stats_ugly_types",
                                                                                      label =  tags$b(text$item_label[text$item_id == "w_compare_fishery_stats_ugly_types"], 
                                                                                                      style = paste0("color:", ambigColors[1], ";")),
                                                                                      choices = subsidy_types_sorted_sumaila[11:13],
                                                                                      selected = subsidy_types_sorted_sumaila[11:13],
                                                                                      width = "100%"),
                                                                   
                                                                   checkboxGroupInput("w_compare_fishery_stats_bad_types",
                                                                                      label =  tags$b(text$item_label[text$item_id == "w_compare_fishery_stats_bad_types"], 
                                                                                                      style = paste0("color:", badColors[1], ";")),
                                                                                      choices = subsidy_types_sorted_sumaila[4:10],
                                                                                      selected = subsidy_types_sorted_sumaila[4:10],
                                                                                      width = "100%")
 
                                                   ) # /conditionalPanel - statistic inlvoes subsidies
                                                                    
                                  ) # /conditionalPanel - country selected
                                                   
                           ) 
                                  
                    )
             ),
             
             ####---------------------------------------------
             #### Right section - plots
             #### --------------------------------------------
             column(8, offset = 4, id = "country-fishery-stats-right-column",
                    
                    ### Section Title ---
                    column(12, id = "tb-spaced-div",
                           
                           uiOutput("compare_fishery_stats_selected_country_name")
                           #tags$h3("Country Name")
                           
                    ),
                    
                    ### plot
                    column(12,
                    
                           # Bar chart
                           plotlyOutput("compare_fishery_stats_bar_plot", width = "100%")
                           
                    )
                    
             )
           
           )
    )
            
  ) # /fluidPage
  
  
