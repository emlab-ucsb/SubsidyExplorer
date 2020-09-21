### -------------------------------------------------------------------
### SubsidyExplorer
### An interactive toolkit to explore the tradeoffs across proposed fisheries subsidies disciplines
### 
### This script contains the content for the selected-results tab
### --------------------------------------------------------------------

ExploreResultsOld = function(proposal_choices) 
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
    
    ### Main content
    column(12,
    
    fluidRow(
      
      ####---------------------------------------------
      #### Left section - model a selected scenario
      #### --------------------------------------------
      column(4, id = "explore-results-left-column",
             
             column(12, id = "lr-spaced-div", 
                    
                    ### First section - text ---
                    column(12, id = "t-spaced-div",
                    column(12, id = "div-underline",
                    
                           text$item_label[text$item_id == "explore_results_intro_text"] %>% lapply(htmltools::HTML)

                    )),
             
                    ### Second section - Explore proposals ---
                    
                    column(12, id = "section-title",
                    
                           # Section Title
                           tags$h5(text$item_label[text$item_id == "explore_results_proposals_header"])
                    
                    ),
             
                    column(12,
                           
                           # Section text
                           text$item_label[text$item_id == "explore_results_proposals_text"] %>% lapply(htmltools::HTML)

                    ),
             
                    column(12, id = "div-underline",
                           
                           # Button
                           tags$button(id = "ab_explore_results_proposals",
                                       class = "btn action-button rounded-button",
                                       tags$b(text$item_label[text$item_id == "ab_explore_results_proposals"], icon("caret-right")))
                    
                    
                           
                    ),
             

                    ### Third section - Design custom proposal ---
                    column(12, id = "section-title",
                    
                           # Section Title
                           tags$h5(text$item_label[text$item_id == "explore_results_custom_header"])
                    
                    ),
             
                    column(12,
                           
                           # Section text
                           text$item_label[text$item_id == "explore_results_custom_text"] %>% lapply(htmltools::HTML)

                           
                    ),
             
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
      
      column(8, offset = 4, id = "explore-results-right-column",
             
             ### Section Title ---
             column(12, id = "section-title-div-underline",
                    
                    tags$h4(text$item_label[text$item_id == "explore_results_plot_header"])
                    
             ),
           
             ### Widgets ---
             fluidRow(
               
               ### Timeseries plot variable 
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
                                                        choices = unlist(wid$choices[wid$item_id == "w_selected_results_timeseries_plot_variable"]),
                                                        selected = unlist(wid$selected[wid$item_id == "w_selected_results_timeseries_plot_variable"]),
                                                        width = "80%")
                                       ),
                                       
                                       tags$td(id = "explore-results-table-cell-l2",
                                               
                                               tags$b(text$item_label[text$item_id == "w_explore_results_timeseries_plot_resolution"])
                                               
                                       ),
                                       
                                       tags$td(id = "explore-results-table-cell-r",
                                               
                                               # Input - timeseries plot resolution
                                               radioButtons("w_explore_results_timeseries_plot_resolution", 
                                                            label = NULL,
                                                            choices = unlist(wid$choices[wid$item_id == "w_selected_results_timeseries_plot_resolution"]),
                                                            selected = unlist(wid$selected[wid$item_id == "w_selected_results_timeseries_plot_resolution"]),
                                                            inline = T,
                                                            width = "80%")
                                       )
                               )
                    )
               )
              
             ),
           
             ### Plot ---
             column(12, id = "t-spaced-div",
                    
                    plotlyOutput("explore_results_timeseries_plot", height = "40vh")
                    
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
                               )
                    )
                    
             ),
             
             ### ------------------------------------------------
             
             ### Section Title ---
             column(12, id = "section-title-div-underline",
                    
                    tags$h4(text$item_label[text$item_id == "explore_results_selected_scenario_header"])
                    
             ),
             
             ### Selected Policy Description ---
             column(12, id = "tb-spaced-div",
                    
                    unlist(best_result$policy_description) %>% lapply(htmltools::HTML)

             )
      
      )
      
    ) # /fluidRow
    
    ) #/column 12 - main
        
  ) # /fluidPage
  
  
