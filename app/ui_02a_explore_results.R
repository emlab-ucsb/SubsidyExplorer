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
      column(4, id = "explore-results-left-column",

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

                    # Selected proposal info
                    column(12, id = "t-spaced-div",

                           htmlOutput("explore_results_proposal_selection_text")

                    ),

                    # Button
                    column(12, align = "center", id = "div-underline",

                           # Run model button (pre-populated proposal)
                           tags$button(id = "ab_run_model_proposal",
                                       class = "btn action-button rounded-button",
                                       tags$b(text$item_label[text$item_id == "ab_run_model_proposal"], icon("caret-right")))

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

      column(8, offset = 4, id = "selected-results-right-column",

             ### Section Title ---
             column(12, id = "section-title-div-underline",
                    
                    # Reactive header with download buttons
                    tags$table(id = "compare-fishery-stats-table",
                                      
                               tags$tr(id = "compare-fishery-stats-table-row",
                                              
                                       tags$td(id = "compare-fishery-stats-table-cell-1",
                                                      
                                               tags$h4(text$item_label[text$item_id == "explore_results_plot_header"])
                                                      
                                              ),
                                              
                                       tags$td(id = "compare-fishery-stats-table-cell-2",
                                                      
                                               tags$button(id = "db_explore_results_download_data",
                                                           class = "btn action-button rounded-button-grey",
                                                           tags$b(icon("external-link-alt"),
                                                                  text$item_label[text$item_id == "db_explore_results_download_data"]))
                                               
                                       ),
                                              
                                       tags$td(id = "compare-fishery-stats-table-cell-3",
                                                      
                                               tags$button(id = "db_explore_results_download_figure",
                                                           class = "btn action-button rounded-button-grey",
                                                           tags$b(icon("download"),
                                                                  text$item_label[text$item_id == "db_explore_results_download_figure"]))
                                                      
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

                                       tags$td(style = "width: 100px;",

                                               tags$b(text$item_label[text$item_id == "w_explore_results_show_ambitious"])

                                       ),

                                       tags$td(

                                         prettyCheckboxGroup("w_explore_results_show_ambitious",
                                                             label = NULL,
                                                             choices = c("Most ambitious scenario"),
                                                             selected = c("Most ambitious scenario"),
                                                             inline = TRUE,
                                                             status = "danger",
                                                             fill = TRUE)

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
                                                             status = "primary",
                                                             fill = TRUE)

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
                                                            status = "warning",
                                                            fill = TRUE)

                                      )
                               )
                    )

             )

             ### ------------------------------------------------

             # ### Section Title ---
             # column(12, id = "section-title-div-underline",
             #
             #        tags$h4(text$item_label[text$item_id == "selected_results_selected_scenario_header"])
             #
             # ),
             #
             # ### Selected Policy Description ---
             # column(12, id = "tb-spaced-div",
             #
             #        uiOutput("selected_results_selected_policy_description")
             #
             # )

      )

    ) # /fluidRow

    ) #/column 12 - main
        
  ) # /fluidPage
  
  
