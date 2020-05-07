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
    
    ### Top navigation button
    column(12,
           fluidRow(style = "padding: 5px 5px; background-color: #3c8dbc;",
                    
                    # Return to main menu
                    column(3,
                           tags$button(id = "ab_selected_results_to_introduction",
                                       class = "btn action-button nav-button-l",
                                       icon("undo"), text$item_label[text$item_id == "ab_selected_results_to_introduction"]
                           )
                    )
                    
           )
    ),
    
    ####-----------------------------------------------------------------------------------------
    #### Top section - Results with selected scenario panel
    #### ----------------------------------------------------------------------------------------
    
    column(12,
           
           column(12, style = "padding: 25px;",
           
                  # Title
                  tags$h3(style = "text-align: left; padding: 0; margin: 0 0 10px;", text$item_label[text$item_id == "selected-results"]),
           
                  # Text
                  includeHTML("./text/02a_selected_results_intro.html")
           
           ),

           fluidRow(
             
             ### Left column - results
             column(8,
                    
                    # Widgets
                    fluidRow(
                      
                      column(6, align = "center",
                             
                             # Input - timeseries plot variable
                             selectizeInput("w_selected_results_timeseries_plot_variable", 
                                            label = tags$b(text$item_label[text$item_id == "w_selected_results_timeseries_plot_variable"]),
                                            choices = unlist(wid$choices[wid$item_id == "w_selected_results_timeseries_plot_variable"]),
                                            selected = unlist(wid$selected[wid$item_id == "w_selected_results_timeseries_plot_variable"]))
                             
                             
                      ),
                      
                      column(6, align = "center",
                             
                             # Input - timeseries plot resolution
                             radioButtons("w_selected_results_timeseries_plot_resolution", 
                                          label = tags$b(text$item_label[text$item_id == "w_selected_results_timeseries_plot_resolution"]),
                                          choices = unlist(wid$choices[wid$item_id == "w_selected_results_timeseries_plot_resolution"]),
                                          selected = unlist(wid$selected[wid$item_id == "w_selected_results_timeseries_plot_resolution"]),
                                          inline = T)
                             
                             
                      )
                    ),
                    
                    # Figure placeholder
                    img(src = "/sample-images/02a_plot_image.png", width = "100%")
             ),
             
             ### Right column - selected scenario
             column(4, style = "background-color: rgba(40, 97, 130, 0.8); color: #ffffff; padding: 10px 10px; height: 100%;",
                    
                    tags$h4(text$item_label[text$item_id == "selected_scenario"]),
                    
                    uiOutput("proposal_selection_text")
             )
             
           )
    ),
    
    ####-----------------------------------------------------------------------------------------
    #### Bottom section - model a selected scenario
    #### ----------------------------------------------------------------------------------------
                    
    ### Select a proposal
    column(12, style = "padding: 0px 25px 25px;",
           
           column(12, style = "padding: 25px; border: 1px solid black;",
                  
                  # Title
                  tags$h4(style = "text-align: left; padding: 0; margin: 0 0 10px;", text$item_label[text$item_id == "select_proposal"]),
                  
                  # Widgets
                  fluidRow(
                    
                    column(6,
                           
                           # Select proposal
                           selectizeInput("w_selected_results_proposal_selection", 
                                          label = tags$b(text$item_label[text$item_id == "w_selected_results_proposal_selection"]),
                                          choices = proposal_choices,
                                          selected = "Default",
                                          width = "100%")
                    ),
                    
                    column(6, 
                           
                           # Select proposal category
                           checkboxGroupInput("w_selected_results_proposal_category",
                                              label = tags$b(text$item_label[text$item_id == "w_selected_results_proposal_category"]),
                                              choices = proposal_categories,
                                              selected = proposal_categories,
                                              width = "100%",
                                              inline = T)
                           
                    )
                    
                  ),
                  
                  # Summary outputs
                  htmlOutput("selected_results_proposal_selection_text")

           )
    ),
           
    
    ### Bottom navigation buttons
    column(12,
           fluidRow(style = "padding: 5px 5px; background-color: #3c8dbc;",
                    
                    # Next to edit-policies
                    column(3, offset = 9,
                           tags$button(id = "ab_selected_results_to_edit_policies",
                                       class = "btn action-button nav-button-r",
                                       text$item_label[text$item_id == "ab_selected_results_to_edit_policies"], icon("chevron-right")
                           )
                    )
                    
           )
    )
            
  ) # /fluidPage
  
  
