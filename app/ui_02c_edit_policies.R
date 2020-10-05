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
    
    ### Page Header
    column(12, id = "page-title-div-underline-blue",
             
           # Title
           tags$h4(text$item_label[text$item_id == "edit-policies"])

    ),
    
    ## Main content
    column(12, id = "edit-policies-main",
           
           fluidRow(
           
           ####----------------------------------------------------
           #### Left column - tabBox with manual policy selection
           #### ----------------------------------------------------
           
           column(8, id = "edit-policies-left-column",
                  
                  ### Step 1
                  column(12, id = "div-underline",
                         
                         # Step 1
                         tags$table(id = "edit-policies-table",

                           tags$tr(id = "edit-policies-table-row",
                                   
                                   tags$td(id = "edit-policies-table-cell-number",
                                           
                                           tags$div(class = "step-number", "1")
                                           
                                   ),
                                   
                                   tags$td(id = "edit-policies-table-cell-c",
                                     
                                           text$item_label[text$item_id == "run-name"] %>% lapply(htmltools::HTML)
                                           
                                     
                                   ),
                                   
                                   tags$td(
                                           
                                     textInput("w_run_name",
                                               label = NULL,
                                               value = "",
                                               width = "100%",
                                               placeholder = "Enter a policy name")
                                   )
                           )
                         )
                         
                  ),
                  
                  ### Step 2
                  column(12, id = "t-spaced-div", 
                         
                         tags$table(id = "edit-policies-table",
                                    
                                    tags$tr(id = "edit-policies-table-row",
                                            
                                            tags$td(id = "edit-policies-table-cell-number",
                                                    
                                                    tags$div(class = "step-number", "2")
                                                    
                                            ),
                                          
                                            tags$td(
                                                    
                                                    text$item_label[text$item_id == "select-disciplines"] %>% lapply(htmltools::HTML)
                                            )
                                    )
                         )
                  ),
    
                  ### tabBox container
                  column(12, id = "t-spaced-div",

                         # tabBox
                         tabBox(width = 12, id = "policy-tabs", 
                         
                                ### --------------------------
                                ### Tab # 1  - IUU
                                ### --------------------------
                                
                                tabPanel(
                                  # Tab title
                                  tagList(
                                    tags$table(id = "edit-policies-table",
                                               
                                               tags$tr(id = "edit-policies-table-row",
                                                       
                                                       tags$td(id = "edit-policies-table-cell-number",
                                                               
                                                               tags$div(class = "step-number", "a")
                                                               
                                                       ),
                                                       
                                                       tags$td(id = "edit-policies-table-cell-r",
                                                               
                                                               text$item_label[text$item_id == "iuu"] %>% lapply(htmltools::HTML))
                                                       )
                                               )
                                    ),
   
                                  value = "iuu",
                                  
                                  IUU(wto_members_and_observers)
                                         
                                ), # /tabPanel #0
                         
                                ### --------------------------
                                ### Tab # 2  - Overfished stock disciplines
                                ### --------------------------
                                
                                tabPanel(
                                  # Tab title
                                  tagList(
                                    tags$table(id = "edit-policies-table",
                                               
                                               tags$tr(id = "edit-policies-table-row",
                                                       
                                                       tags$td(id = "edit-policies-table-cell-number",
                                                               
                                                               tags$div(class = "step-number", "b")
                                                               
                                                       ),
                                                       
                                                       tags$td(id = "edit-policies-table-cell-r",
                                                               
                                                               text$item_label[text$item_id == "oa"] %>% lapply(htmltools::HTML))
                                               )
                                    )
                                  ),
                                  value = "oa",
                                         
                                  OA(wto_members_and_observers)
                                         
                                ), #/tabPanel 1
                         
                                ### ------------------------------------------
                                ### Tab # 3  - Overcapacity and overfishing disciplines
                                ### ------------------------------------------
                                
                                tabPanel(
                                  # Tab title
                                  tagList(
                                    tags$table(id = "edit-policies-table",
                                               
                                               tags$tr(id = "edit-policies-table-row",
                                                       
                                                       tags$td(id = "edit-policies-table-cell-number",
                                                               
                                                               tags$div(class = "step-number", "c")
                                                               
                                                       ),
                                                       
                                                       tags$td(id = "edit-policies-table-cell-r",
                                                               
                                                               text$item_label[text$item_id == "overcap"] %>% lapply(htmltools::HTML))
                                               )
                                    )
                                  ),
                                 
                                  value = "overcap",
                                  
                                  Overcap(wto_members_and_observers, subsidy_types_sorted_sumaila)
                                         
                                ) # /tabPanel #3  
                         
                         ) # /tabBox
                  
                  ) # /column 12 - tabBox container
           
           ), # /column 9 - Left column
    
           
           ####-----------------------------------------------------------------------------------------
           #### Right column - Menu of selected policies
           #### ----------------------------------------------------------------------------------------
           ###Fixed right column to provide background shading
           column(4, id = "edit-policies-right-column",
                  
           ),
           
           #### Actual right column that will scroll appropriately
           column(4, id = "edit-policies-real-right-column",
           
                  column(12, id = "t-spaced-div",
                         
                         # Step 3
                         tags$table(id = "edit-policies-table",
                                    
                                    tags$tr(id = "edit-policies-table-row",
                                            
                                            tags$td(id = "edit-policies-table-cell-number",
                                                    
                                                    tags$div(class = "step-number", "3")
                                                    
                                            ),
                                            
                                            tags$td(
                                                    
                                              text$item_label[text$item_id == "selected-policy"] %>% lapply(htmltools::HTML)
                                              
                                            )
                                    )
                         )
                  ),
                  
                  column(12, id = "div-underline",
                         
                        # Reactive policy summary
                         uiOutput("custom_policy")
                        
                  ),
                  
                  column(12, id = "t-spaced-div",
                         
                         # Step 4
                         tags$table(id = "edit-policies-table",
                                    
                                    tags$tr(id = "edit-policies-table-row",
                                            
                                            tags$td(id = "edit-policies-table-cell-number",
                                                    
                                                    tags$div(class = "step-number", "4")
                                                    
                                            ),
                                            
                                            tags$td(
                                              
                                              text$item_label[text$item_id == "run-model"] %>% lapply(htmltools::HTML)
                                              
                                            )
                                            
                                           
                                    )
                         )
                  ),
                  
                  column(12, id = "t-spaced-div", align = "center",
                         
                         # Warning about missing name
                         uiOutput("custom_name_warning")
                         
                  ),
                  
                  column(12, id = "tb-spaced-div", align = "center",
                         
                         # Button
                         tags$button(id = "ab_run_model_custom",
                                     class = "btn action-button rounded-button",
                                     tags$b(text$item_label[text$item_id == "ab_run_model_custom"]))

                  )

           ) # /column 4 - Right column
           
           ) # /fluidRow
 
    ) # /column 12 - content
                
) # /fluidPage
  
  
