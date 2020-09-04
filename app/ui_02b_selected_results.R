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
      column(4, id = "selected-results-left-column",
             
             ### Section Title ---
             column(12, id = "section-title",
                    
                    tags$h4(text$item_label[text$item_id == "select-proposal"])
                    
             ),
             
             ### Section 1 ---
             column(12,
                    
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
                    tags$h4(text$item_label[text$item_id == "design-custom-proposal"])
                    
             ),
             
             column(12,
                    
                    tags$p("Advanced users can design their own subsidy reform proposal and plot how it affects Biomass, Fish Catch, and Revenue over time.")
                    
             ),
             
             ### Section 2 ---
             column(12, align = "center", id = "tb-spaced-div",
                    
                    # IUU definitions info button
                    tags$button(id = "ab_selected_results_to_edit_policies",
                                class = "btn action-button rounded-button",
                                tags$b("Text"))
                    
             )
               
      ),
    
      ####---------------------------------------------
      #### Right section - model a selected scenario
      #### --------------------------------------------
      
      column(8, id = "selected-results-right-column",
             
             ### Section Title ---
             column(12, id = "section-title",
                    
                    tags$h4(text$item_label[text$item_id == "proposal-results"])
                    
             ),
             
             ### Widgets ---
             fluidRow(
               
               ### Timeseries plot variable 
               column(12, align = "center",
                      
                      tags$table(id = "selected-results-table",
                                 
                                 tags$tr(id = "selected-results-table-table-row",
                                         
                                         tags$td(id = "selected-results-table-cell-l1",
                                                 
                                                 tags$b(text$item_label[text$item_id == "w_explore_results_timeseries_plot_variable"])
                                                 
                                         ),
                                         
                                         tags$td(id = "selected-results-table-cell-r",
                                                 
                                                 # Input - timeseries plot variable
                                                 selectizeInput("w_selected_results_timeseries_plot_variable", 
                                                                label = NULL,
                                                                choices = unlist(wid$choices[wid$item_id == "w_selected_results_timeseries_plot_variable"]),
                                                                selected = unlist(wid$selected[wid$item_id == "w_selected_results_timeseries_plot_variable"]),
                                                                width = "80%")
                                         ),
                                         
                                         tags$td(id = "selected-results-table-cell-l2",
                                                 
                                                 tags$b(text$item_label[text$item_id == "w_explore_results_timeseries_plot_resolution"])
                                                 
                                         ),
                                         
                                         tags$td(id = "selected-results-table-cell-r",
                                                 
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
               )
               
             ),
             
             ### Plot ---
             column(12, id = "div-underline",
                    
                    plotlyOutput("model_results_timeseries_plot", height = "50vh")
                    
             ),
             
             ### ------------------------------------------------
             
             ### Section Title ---
             column(12, id = "section-title",
                    
                    tags$h4(text$item_label[text$item_id == "selected-scenario"])
                    
             ),
             
             ### Selected Policy Description ---
             column(12,
                    
                    uiOutput("selected_results_policy_description")
                    
             )
             
      )
      
    ) # /fluidRow
        
  ) # /fluidPage
  
  
