### -------------------------------------------------------------------
### SubsidyExplorer
### An interactive toolkit to explore the tradeoffs across proposed fisheries subsidies disciplines
### 
### This script contains the content for the selected-results tab
### --------------------------------------------------------------------

ExploreResults = function(proposal_choices) 
  fluidPage(
    
    # Page style
    style = "background-color: #ffffff; color: #000000;",
    
    ### Page Header
    column(12, id = "page-title-div-underline-blue",

           # Title
           tags$h4(text$item_label[text$item_id == "explore-results"], tags$button(id = "info_explore_results",
                                                                                   class = "btn action-button info-button",
                                                                                   icon("info")))

    ),

    ## Main content
    column(12,

    fluidRow(

      ####---------------------------------------------
      #### Left section - model a selected scenario
      #### --------------------------------------------
      #### Fixed absolute panel to provide shading in background 
      column(4, id = "explore-results-left-column",
             
      ),
      
      #### Actual left column that will scroll appropriately
      column(4, id = "explore-results-real-left-column",

             column(12, id = "lr-spaced-div",

                    ### Section 1: Pre-populated proposals ---
                    # Proposal selection header
                    column(12, id = "section-title",

                           # Section Title
                           tags$h5(text$item_label[text$item_id == "explore_results_proposal_header"])

                    ),

                    # Proposal selection text
                    column(12,

                           text$item_label[text$item_id == "explore_results_proposal_text"] %>% lapply(htmltools::HTML)

                    ),

                    # Widgets
                    column(12, id = "t-spaced-div",

                           # Select proposal category
                           checkboxGroupInput("w_explore_results_proposal_category",
                                              label = tags$b(text$item_label[text$item_id == "w_explore_results_proposal_category"]),
                                              choices = proposal_categories,
                                              selected = proposal_categories,
                                              width = "100%",
                                              inline = T)
                    ),
                    
                    column(12, id = "t-spaced-div",

                           # Select proposal
                           selectizeInput("w_explore_results_proposal_selection",
                                          label = tags$b(text$item_label[text$item_id == "w_explore_results_proposal_selection"]),
                                          choices = proposal_choices,
                                          selected = "Default",
                                          width = "100%")

                    ),
                    
                    fluidRow(
                      # Conditional panel for overfished definition selection
                      conditionalPanel("input.w_explore_results_proposal_selection == 'RD/TN/RL/126/Rev.2' || input.w_explore_results_proposal_selection == 'RD/TN/RL/119/Rev.1'",
                                     
                                       column(6, id = "t-spaced-div",
                                            
                                            # Select overfished definition
                                            radioButtons("w_explore_results_overfished_multiple_options",
                                                         label = tags$b(text$item_label[text$item_id == "w_explore_results_overfished_multiple_options"]),
                                                         choices = c("Objective Definition" = "RD/TN/RL/79/Rev.1", # New Zealand/Iceland
                                                                     "Relevant Authorities" = "RD/TN/RL/77/Rev.2"), # Australia
                                                         selected = "RD/TN/RL/79/Rev.1",
                                                         width = "100%")
                                            
                                     )
                    ),

                    # Conditional panel for cap selection
                    conditionalPanel("input.w_explore_results_proposal_selection == 'RD/TN/RL/126/Rev.2'",
                                     
                                     column(6, id = "t-spaced-div",
                                            
                                            # Select overfished definition
                                            radioButtons("w_explore_results_cap_multiple_options",
                                                         label = tags$b(text$item_label[text$item_id == "w_explore_results_cap_multiple_options"]),
                                                         choices = c("None" = "Default",
                                                                     "De minimis" = "RD/TN/RL/81",
                                                                     "Tiers" = "TN/RL/GEN/197/Rev.2",
                                                                     "Optional criteria" = "TN/RL/GEN/199",
                                                                     "Formula" = "RD/TN/RL/124"),
                                                         selected = "Default",
                                                         width = "100%")
                                            
                                     )
                    ),
                    
                    # Conditional panel for sustainability scenario warning message
                    conditionalPanel("input.w_explore_results_proposal_selection == 'TN/RL/W/276/Rev.1 | Sustainability scenario'",
                                     
                                     column(12, id = "t-spaced-div",
                                            
                                            tags$i("Note: Previously this scenario contained an error exempting all Developing and LDC Members from OFOC disciplines. This has been corrected and the results from modeling this scenario have changed.", style = "color: green;")
                                     )
                    )
                    ),
                    
                    # Selected proposal info
                    column(12, id = "t-spaced-div", align = "center",
                           
                           # Reactive run model button
                           uiOutput("run_model_button")
                           
                    ),
                                     
                    # Selected proposal info
                    column(12, id = "div-underline",

                           htmlOutput("explore_results_proposal_selection_text")
                           
                    ),
                    
                    ### Section #2: Design custom proposal ---
                    # Custom policy header
                    column(12, id = "section-title",

                           tags$h5(text$item_label[text$item_id == "explore_results_custom_header"])

                    ),

                    # Custom policy intro text
                    column(12,

                           text$item_label[text$item_id == "explore_results_custom_text"] %>% lapply(htmltools::HTML)

                    ),

                    # Button
                    column(12, id = "tb-spaced-div",

                           # Button
                           tags$button(id = "ab_explore_results_custom",
                                       class = "btn action-button rounded-button",
                                       tags$b(text$item_label[text$item_id == "ab_explore_results_custom"], icon("caret-right")))

                    )
             )

      ),

      ####---------------------------------------------
      #### Right section - model a selected scenario
      #### --------------------------------------------

      column(8, id = "explore-results-right-column",

             ### Section Title ---
             column(12, id = "section-title-div-underline",
                    
                    # Reactive header with download buttons
                    tags$table(id = "compare-fishery-stats-table",
                                      
                               tags$tr(id = "compare-fishery-stats-table-row",
                                              
                                       tags$td(id = "compare-fishery-stats-table-cell-1",
                                                      
                                               tags$h4(text$item_label[text$item_id == "explore_results_plot_header"])
                                                      
                                              ),
                                              
                                       tags$td(id = "compare-fishery-stats-table-cell-2",
                                               
                                               downloadButton("db_explore_results_download_data_global",
                                                              text$item_label[text$item_id == "db_explore_results_download_data_global"] %>% lapply(htmltools::HTML))
                                               
                                               
                                       ),
                                       
                                       tags$td(id = "compare-fishery-stats-table-cell-2",
                                               
                                               downloadButton("db_explore_results_download_data_regional",
                                                              text$item_label[text$item_id == "db_explore_results_download_data_regional"] %>% lapply(htmltools::HTML))
                                               
                                               
                                       ),
                                              
                                       tags$td(id = "compare-fishery-stats-table-cell-2",
                                                      
                                               downloadButton("db_explore_results_download_figure_global",
                                                              text$item_label[text$item_id == "db_explore_results_download_figure_global"] %>% lapply(htmltools::HTML))
                                                      
                                       ),
                                       
                                       tags$td(id = "compare-fishery-stats-table-cell-3",
                                               
                                               downloadButton("db_explore_results_download_figure_regional",
                                                              text$item_label[text$item_id == "db_explore_results_download_figure_regional"] %>% lapply(htmltools::HTML))
                                               
                                       )
                               )
                    )

             ),

             ### Widgets ---
             column(12, id = "t-spaced-div", align = "center",

                      tags$table(id = "explore-results-table",

                                 tags$tr(id = "explore-results-table-table-row",

                                         tags$td(id = "explore-results-table-cell-l1",

                                                 tags$b(text$item_label[text$item_id == "w_explore_results_timeseries_plot_variable"])

                                         ),

                                         tags$td(id = "explore-results-table-cell-r",

                                                 # Input - timeseries plot variable
                                                 selectizeInput("w_explore_results_timeseries_plot_variable",
                                                                label = NULL,
                                                                choices = unlist(wid$choices[wid$item_id == "w_explore_results_timeseries_plot_variable"]),
                                                                selected = unlist(wid$selected[wid$item_id == "w_explore_results_timeseries_plot_variable"]),
                                                                width = "80%")
                                         ),

                                         tags$td(id = "explore-results-table-cell-l2",

                                                 tags$b(text$item_label[text$item_id == "w_explore_results_timeseries_plot_resolution"])

                                         ),

                                         tags$td(id = "explore-results-table-cell-r",

                                                 # Input - timeseries plot resolution
                                                 radioButtons("w_explore_results_timeseries_plot_resolution",
                                                              label = NULL,
                                                              choices = unlist(wid$choices[wid$item_id == "w_explore_results_timeseries_plot_resolution"]),
                                                              selected = unlist(wid$selected[wid$item_id == "w_explore_results_timeseries_plot_resolution"]),
                                                              inline = T,
                                                              width = "80%")
                                         )
                                 )
                      )
              ),

             ### Plot ---
             column(12, id = "t-spaced-div",

                    plotlyOutput("explore_results_timeseries_plot", height = "60vh")

             ),

             ### Checkbox group input for scenarios run
             column(12, id = "tb-spaced-div",

                    tags$table(id = "explore-results-table",

                               tags$tr(id = "explore-results-table-table-row",

                                       tags$td(style = "width: 100px; text-align: left;",

                                               text$item_label[text$item_id == "w_explore_results_show_ambitious"] %>% lapply(htmltools::HTML)

                                       ),

                                       tags$td(

                                         prettyCheckboxGroup("w_explore_results_show_ambitious",
                                                             label = NULL,
                                                             choices = c("Most ambitious scenario"),
                                                             selected = c("Most ambitious scenario"),
                                                             inline = TRUE,
                                                             status = "default",
                                                             icon = icon("check"),
                                                             fill = FALSE)

                                       )
                               ),
                               tags$tr(id = "explore-results-table-table-row",

                                       tags$td(style = "width: 100px;",


                                       ),

                                       tags$td(

                                         prettyCheckboxGroup("w_explore_results_show_policies",
                                                             label = NULL,
                                                             choices = "",
                                                             selected = "",
                                                             inline = TRUE,
                                                             status = "default",
                                                             icon = icon("check"),
                                                             fill = FALSE)

                                       )
                               ),
                               tags$tr(id = "explore-results-table-table-row",

                                      tags$td(style = "width: 100px;",


                                      ),

                                      tags$td(

                                        prettyCheckboxGroup("w_explore_results_show_custom",
                                                            label = NULL,
                                                            choices = "",
                                                            selected = "",
                                                            inline = TRUE,
                                                            status = "default",
                                                            icon = icon("check"),
                                                            fill = FALSE)

                                      )
                               )
                    )

             )

      )

    ) # /fluidRow

    ) #/column 12 - main
        
  ) # /fluidPage
