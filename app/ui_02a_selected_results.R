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
    
    fluidRow(
      
      ####---------------------------------------------
      #### Left section - model a selected scenario
      #### --------------------------------------------
      column(4, style = "padding: 15px;",
             
             column(12, id = "section-title-div-underline-black",
                    
                    # Section Title
                    tags$h3("Select a Proposal")
                    
             ),

             column(12, id = "spaced-div",
                    
                    # Select proposal category
                    checkboxGroupInput("w_selected_results_proposal_category",
                                       label = tags$b(text$item_label[text$item_id == "w_selected_results_proposal_category"]),
                                       choices = proposal_categories,
                                       selected = proposal_categories,
                                       width = "100%",
                                       inline = T)
                    
             ),
             
             column(12, id = "spaced-div",
                      
                      # Select proposal
                      selectizeInput("w_selected_results_proposal_selection", 
                                     label = tags$b(text$item_label[text$item_id == "w_selected_results_proposal_selection"]),
                                     choices = proposal_choices,
                                     selected = "Default",
                                     width = "100%")
               ),
             
             column(12, id = "spaced-div",
                    
                    # Summary outputs
                    htmlOutput("selected_results_proposal_selection_text")
                    
             ),
             
             column(12, align = "center", id = "spaced-div",
                    
                    # IUU definitions info button
                    tags$button(id = "ab_run_model_proposal",
                                class = "btn action-button rounded-button",
                                tags$b(text$item_label[text$item_id == "ab_run_model_proposal"]))
                    
             )
               
      ),
    
      ####---------------------------------------------
      #### Right section - model a selected scenario
      #### --------------------------------------------
      
      column(8, style = "padding: 15px;",
             
             ### Section Title ---
             column(12, id = "section-title-div-underline-black",
                    
                    tags$h3("Select a Proposal")
                    
             ),
           
             ### Widgets ---
             fluidRow(
               
               ### Timeseries plot variable 
               column(6, id = "spaced-div", align = "center",
                    
                    tags$table(id = "selected-results-table",
                               
                               tags$tr(id = "selected-results-table-table-row",
                                       
                                       tags$td(id = "selected-results-table-cell-l",
                                               
                                               tags$b(text$item_label[text$item_id == "w_selected_results_timeseries_plot_variable"])
                                               
                                       ),
                                       
                                       tags$td(
                                         
                                         # Input - timeseries plot variable
                                         selectizeInput("w_selected_results_timeseries_plot_variable", 
                                                        label = NULL,
                                                        choices = unlist(wid$choices[wid$item_id == "w_selected_results_timeseries_plot_variable"]),
                                                        selected = unlist(wid$selected[wid$item_id == "w_selected_results_timeseries_plot_variable"]),
                                                        width = "100%")
                                       )
                               )
                    )
               ),
               
               column(6, id = "spaced-div", align = "center",
                      
                      tags$table(id = "selected-results-table",
                                 
                                 tags$tr(id = "selected-results-table-table-row",
                                         
                                         tags$td(id = "selected-results-table-cell-l",
                                                 
                                                 tags$b(text$item_label[text$item_id == "w_selected_results_timeseries_plot_resolution"])
                                                 
                                         ),
                                         
                                         tags$td(
                                           
                                           # Input - timeseries plot resolution
                                           radioButtons("w_selected_results_timeseries_plot_resolution", 
                                                        label = NULL,
                                                        choices = unlist(wid$choices[wid$item_id == "w_selected_results_timeseries_plot_resolution"]),
                                                        selected = unlist(wid$selected[wid$item_id == "w_selected_results_timeseries_plot_resolution"]),
                                                        inline = T)
                                         )
                                 )
                      )
               )
             ),
           
             ### Plot ---
             plotlyOutput("model_results_timeseries_plot"),
             
             
             ### ------------------------------------------------
             
             ### Section Title ---
             column(12, id = "section-title-div-underline-black",
                    
                    tags$h3(text$item_label[text$item_id == "selected_scenario"])
                    
             ),
             
             ### Selected Policy Description ---
             column(12,
                    
                    uiOutput("selected_policy_description")
                    
             )
      
      )
      
    ) # /fluidRow
        
  ) # /fluidPage
  
  
