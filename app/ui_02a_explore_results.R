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
           tags$h3(text$item_label[text$item_id == "explore-results"])
           
    ),
    
    ### Main content
    column(12,
    
    fluidRow(
      
      ####---------------------------------------------
      #### Left section - model a selected scenario
      #### --------------------------------------------
      column(4, id = "explore-results-left-column",
             
             ### Section Title ---
             column(12, id = "section-title-div-underline",
                    
                    tags$h4("Instructions")
                    
             ),
             
             ### First section - text ---
             
             column(12, id = "div-underline",
                    
                    tags$p("The", tags$b("Proposal Results"), "graph on this page shows the Most Ambitious Scenario in terms of subsidy reform: a complete removal of capacity-enhancing subsidies. By following the chart line, you can see how the scenario affects Biomass, Fish Catch, and Revenue over time.")
                    
             ),
             
             ### Second section - Explore proposals ---
        
             column(12, id = "section-title",
                    
                    # Section Title
                    tags$h5(text$item_label[text$item_id == "explore-proposals"])
                    
             ),
             
             column(12,
                    
                    tags$p("In addition to the Most Ambitious Scenario displayed here, the SubsidyExplorer contains many more pre-populated proposals to explore.")
                    
             ),
             
             column(12, id = "div-underline",
                    
                    # Button
                    tags$button(id = "ab_explore_proposals",
                                class = "btn action-button rounded-button",
                                tags$b(text$item_label[text$item_id == "ab_explore_proposals"], icon("caret-right")))
                    
                    
             ),
             

             ### Third section - Design custom proposal ---
             
             column(12, id = "section-title",
                    
                    # Section Title
                    tags$h5(text$item_label[text$item_id == "design-custom-proposal"])
                    
             ),
             
             column(12,
                    
                    tags$p("Advanced users can design their own subsidy reform proposal and plot how it affects Biomass, Fish Catch, and Revenue over time.")
                    
             ),
             
             column(12, id = "tb-spaced-div",
                    
                    # Button
                    tags$button(id = "ab_design_custom_proposal",
                                class = "btn action-button rounded-button",
                                tags$b(text$item_label[text$item_id == "ab_design_custom_proposal"], icon("caret-right")))
                    
             )
      ),
    
      ####---------------------------------------------
      #### Right section - model a selected scenario
      #### --------------------------------------------
      
      column(8, offset = 4, id = "explore-results-right-column",
             
             ### Section Title ---
             column(12, id = "section-title-div-underline",
                    
                    tags$h4(text$item_label[text$item_id == "proposal-results"])
                    
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
             #plotlyOutput("model_results_timeseries_plot", height = "50vh"),
             
             
             ### ------------------------------------------------
             
             ### Section Title ---
             column(12, id = "section-title-div-underline",
                    
                    tags$h4(text$item_label[text$item_id == "selected-scenario"])
                    
             ),
             
             ### Selected Policy Description ---
             column(12, id = "tb-spaced-div",
                    
                    "policy description"
                    #uiOutput("selected_policy_description")
                    
             )
      
      )
      
    ) # /fluidRow
    
    ) #/column 12 - main
        
  ) # /fluidPage
  
  
