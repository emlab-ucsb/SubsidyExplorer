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

             tags$h3("Select a Proposal"),
             
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
             
             tags$h3("Proposal Results"),
           
             ### Widgets
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
           
             # Plot
             plotlyOutput("model_results_timeseries_plot")
             
      )
      
    ) # /fluidRow
      
      
           
# 
#              
#            ### Right column
#            column(3,
#                   style = "position: absolute;
#                   background-color: rgba(40, 97, 130, 0.8);
#                   color: #ffffff;
#                   padding: 10px; 10px;
#                   top:0;
#                   bottom:0;
#                   right:0;
#                   overflow: hidden;", 
#                   
#                   # Title
#                   tags$h4(text$item_label[text$item_id == "selected_scenario"]),
#                     
#                   # Selected policy description
#                   uiOutput("selected_policy_description")
#                   
#            )
#     ),
        
  ) # /fluidPage
  
  
