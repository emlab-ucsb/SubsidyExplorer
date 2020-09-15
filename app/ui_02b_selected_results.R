### -------------------------------------------------------------------
### SubsidyExplorer
### An interactive toolkit to explore the tradeoffs across proposed fisheries subsidies disciplines
### 
### This script contains the content for the selected-results tab
### --------------------------------------------------------------------

SelectedResults = function(proposal_choices) 
  fluidPage(
    
    # Page style
    style = "background-color: #ffffff; color: #000000;",
    
    ### Page Header
    column(12, id = "page-title-div-underline-blue",
           
           # Title
           tags$h3(text$item_label[text$item_id == "selected-results"])
           
    ),
    
    ## Main content
    column(12,
    
    fluidRow(
      
      ####---------------------------------------------
      #### Left section - model a selected scenario
      #### --------------------------------------------
      column(4, id = "selected-results-left-column",
             
             
             column(12, id = "lr-spaced-div",
                    
                    ### Section Title ---
                    column(12, id = "section-title-div-underline",
                    
                           tags$h4(text$item_label[text$item_id == "select-proposal"])
                    
                    ),
             
                    ### Section 1 ---
                    column(12, id = "t-spaced-div",
                    
                           # Select proposal category
                           checkboxGroupInput("w_selected_results_proposal_category",
                                              label = tags$b(text$item_label[text$item_id == "w_selected_results_proposal_category"]),
                                              choices = proposal_categories,
                                              selected = proposal_categories,
                                              width = "100%",
                                              inline = T)
                    
                    ),
             
                    column(12, id = "t-spaced-div",
                           
                           # Select proposal
                           selectizeInput("w_selected_results_proposal_selection", 
                                          label = tags$b(text$item_label[text$item_id == "w_selected_results_proposal_selection"]),
                                          choices = proposal_choices,
                                          selected = "Default",
                                          width = "100%")
                           
                    ),
             
                    column(12, id = "t-spaced-div",
                           
                           # Summary outputs
                           htmlOutput("selected_results_proposal_selection_text")
                    
                    ),
             
                    column(12, align = "center", id = "div-underline",
                    
                           # IUU definitions info button
                           tags$button(id = "ab_run_model_proposal",
                                       class = "btn action-button rounded-button",
                                       tags$b(text$item_label[text$item_id == "ab_run_model_proposal"]))
                    
                    ),
             
                    ### Third section - Design custom proposal ---
                    column(12, id = "section-title",
                           
                           # Section Title
                           tags$h5(text$item_label[text$item_id == "design-custom-proposal"])
                           
                    ),
                    
                    column(12,
                           
                           includeHTML("./text/02-results/explore-results/left_panel_design_proposal.html")
                           
                           
                    ),
                    
                    column(12, id = "tb-spaced-div",
                           
                           # Button
                           tags$button(id = "ab_selected_results_design_custom_proposal",
                                       class = "btn action-button rounded-button",
                                       tags$b(text$item_label[text$item_id == "ab_selected_results_design_custom_proposal"], icon("caret-right")))
                           
                    )
             )
                  
      ),
    
      ####---------------------------------------------
      #### Right section - model a selected scenario
      #### --------------------------------------------
      
      column(8, offset = 4, id = "selected-results-right-column",
             
             ### Section Title ---
             column(12, id = "section-title-div-underline",
                    
                    tags$h4(text$item_label[text$item_id == "proposal-results"])
                    
             ),
             
             ### Widgets ---
             column(12, id = "t-spaced-div",
                      
                      tags$table(id = "explore-results-table",
                                 
                                 tags$tr(id = "explore-results-table-table-row",
                                         
                                         tags$td(id = "explore-results-table-cell-l1",
                                                 
                                                 tags$b(text$item_label[text$item_id == "w_explore_results_timeseries_plot_variable"])
                                                 
                                         ),
                                         
                                         tags$td(id = "explore-results-table-cell-r",
                                                 
                                                 # Input - timeseries plot variable
                                                 selectizeInput("w_selected_results_timeseries_plot_variable", 
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
                                                 radioButtons("w_selected_results_timeseries_plot_resolution", 
                                                              label = NULL,
                                                              choices = unlist(wid$choices[wid$item_id == "w_selected_results_timeseries_plot_resolution"]),
                                                              selected = unlist(wid$selected[wid$item_id == "w_selected_results_timeseries_plot_resolution"]),
                                                              inline = T,
                                                              width = "80%")
                                         )
                                 )
                      )
              ),
             
             ### Plot ---
             column(12, id = "t-spaced-div",
                    
                    plotlyOutput("model_results_timeseries_plot", height = "40vh")
                    
             ),
             
             ### Checkbox group input for scenarios run
             column(12, id = "tb-spaced-div",
                    
                    tags$table(id = "explore-results-table",
                               
                               tags$tr(id = "explore-results-table-table-row",
                                       
                                       tags$td(style = "width: 100px;",
                                               
                                               tags$b("Show on Plot: ")
                                               
                                       ),
                                       
                                       tags$td(
                                         
                                         prettyCheckboxGroup("w_selected_results_show_ambitious",
                                                             label = NULL,
                                                             choices = c("Most ambitious scenario"),
                                                             selected = c("Most ambitious scenario"),
                                                             inline = TRUE,
                                                             status = "primary",
                                                             fill = TRUE)
                                         
                                       )
                               ),
                               tags$tr(id = "explore-results-table-table-row",

                                       tags$td(style = "width: 100px;",


                                       ),

                                       tags$td(
                                         
                                         prettyCheckboxGroup("w_selected_results_show_policies",
                                                             label = NULL,
                                                             choices = "",
                                                             selected = "",
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
                    
                    tags$h4(text$item_label[text$item_id == "selected-scenario"])
                    
             ),
             
             ### Selected Policy Description ---
             column(12, id = "tb-spaced-div",
                    
                    uiOutput("selected_results_selected_policy_description")
                    
             )
             
      )
      
    ) # /fluidRow
    
    ) #/column 12 - main
        
  ) # /fluidPage
  
  
