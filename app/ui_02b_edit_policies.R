### -------------------------------------------------------------------
### SubsidyExplorer
### An interactive toolkit to explore the tradeoffs across proposed fisheries subsidies disciplines
### 
### This script contains the content for the "edit-policies" tab
### --------------------------------------------------------------------

EditPolicies = function(wto_members_and_observers, subsidy_types_sorted_sumaila) 
  fluidPage(
    
    # Page style
    style = "background-color: #ffffff; color: #000000;",
    
    ### Custom formatting for tabs
    tags$style(
    ),
    
    ### Top navigation buttons
    column(12,
           fluidRow(style = "padding: 5px 5px; background-color: #3c8dbc;",
                    
                    # Return to main menu
                    column(3,
                           tags$button(id = "ab_edit_policies_to_introduction",
                                       class = "btn action-button nav-button-l",
                                       icon("undo"), text$item_label[text$item_id == "ab_edit_policies_to_introduction"]
                           )
                    )
                    
           )
    ),
    
    ### Page Header
    column(12, id = "page-title-div-underline-blue",
             
           # Title
           tags$h3(text$item_label[text$item_id == "edit-policies"])

    ),
    
    ## Main content
    column(12, id = "edit-policies-main",
           
           ####----------------------------------------------------
           #### Left column - tabBox with manual policy selection
           #### ----------------------------------------------------
           
           column(9, id = "edit-policies-left-column",
                  
                  ### Text with name your policy and select displiplines intro
                  column(12, id = "spaced-div",
                         
                         ### Text
                         includeHTML("./text/02b_edit_policies_intro.html"),
                         
                         tags$br(),
                         
                         # Step 1
                         tags$table(id = "edit-policies-table",

                           tags$tr(id = "edit-policies-table-row",
                                   
                                   tags$td(id = "edit-policies-table-cell-l",
                                           
                                           tags$button(class = "btn action-button step-number", "1")
                                           
                                   ),
                                   
                                   tags$td(id = "edit-policies-table-cell-c",
                                           
                                           tags$b(class = "big", text$item_label[text$item_id == "run_name"])
                                     
                                   ),
                                   
                                   tags$td(id = "edit-policies-table-cell-r",
                                           
                                     textInput("w_run_name",
                                               label = NULL,
                                               value = "",
                                               width = "100%")
                                   )
                           )
                         ),
                         
                         tags$br(),
                         
                         # Step 2
                         tags$table(id = "edit-policies-table",
                                    
                                    tags$tr(id = "edit-policies-table-row",
                                            
                                            tags$td(id = "edit-policies-table-cell-l",
                                                    
                                                    tags$button(class = "btn action-button step-number", "2")
                                                    
                                            ),
                                          
                                            tags$td(id = "edit-policies-table-cell-cr",
                                                    
                                                    text$item_label[text$item_id == "select_disciplines"] %>% lapply(htmltools::HTML)
                                            )
                                    )
                         )
                         
                  ),
    
                  ### tabBox container
                  column(12, id = "spaced-div",

                         # tabBox
                         tabBox(width = 12, id = "policy-tabs", 
                         
                                ### --------------------------
                                ### Tab # 1  - IUU
                                ### --------------------------
                                
                                tabPanel(text$item_label[text$item_id == "iuu"] %>% lapply(htmltools::HTML), 
                                         value = "iuu",
                                         
                                         IUU(wto_members_and_observers)
                                         
                                ), # /tabPanel #0
                         
                                ### --------------------------
                                ### Tab # 2  - Overfished stock disciplines
                                ### --------------------------
                                
                                tabPanel(text$item_label[text$item_id == "oa"] %>% lapply(htmltools::HTML), 
                                         value = "oa",
                                         
                                         OA(wto_members_and_observers)
                                         
                                ), #/tabPanel 1
                         
                                ### ------------------------------------------
                                ### Tab # 3  - Overcapacity and overfishing disciplines
                                ### ------------------------------------------
                                
                                tabPanel(text$item_label[text$item_id == "overcap"] %>% lapply(htmltools::HTML), 
                                         value = "overcap",
                                         
                                         Overcap(wto_members_and_observers, subsidy_types_sorted_sumaila)
                                         
                                ) # /tabPanel #3  
                         
                         ) # /tabBox
                  
                  ) # /column 12 - tabBox container
           
           ), # /column 9 - Left column
    
           
           ####-----------------------------------------------------------------------------------------
           #### Right column - Menu of selected policies
           #### ----------------------------------------------------------------------------------------
           column(3, id = "edit-policies-right-column",

                  column(12, id = "spaced-div",
                         
                        tags$b(class = "big", text$item_label[text$item_id == "selected-policy"]),

                        # Reactive policy summary
                        uiOutput("custom_policy"),

                        # Run model button
                        column(12, align = "center", style = "padding: 25px 0 0 0;",

                               
                               actionButton("ab_run_model_custom",
                                            tags$b(text$item_label[text$item_id == "ab_run_model_custom"]),
                                            style = "color: black;
                                            background-color: rgba(255,255,255,0.7);
                                            border: 3px #3c8dbc solid;
                                            white-space: normal;"),


                         # Warning about missing name
                         uiOutput("custom_name_warning")

                        )
                        
                  )

           ), # /column 3 - Right column
 
    ), # /column 12 - content
                  
    ### Bottom navigation buttons
    column(12,
           
           fluidRow(style = "padding: 5px 5px; background-color: #3c8dbc;",
                
                    # Back to compare fishery stats
                    column(3,
                           tags$button(id = "ab_edit_policies_to_selected_results",
                                       class = "btn action-button nav-button-l",
                                       icon("chevron-left"), text$item_label[text$item_id == "ab_edit_policies_to_selected_results"])
                    )
                
           )
    )
    
) # /fluidPage
  
  
