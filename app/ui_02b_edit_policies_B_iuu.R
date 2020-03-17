### -------------------------------------------------------------------
### SubsidyExplorer
### An interactive toolkit to explore the tradeoffs across proposed fisheries subsidies disciplines
### 
### This script contains the content for the "iuu" box on the edit-policies" tab
### --------------------------------------------------------------------

IUU = function(wto_members_and_observers) 
  
  # Column container for tab panel        
  column(12, style = "border-style: solid; border-width: 2px 1px 1px 1px; border-color: #28292C;",
                  
         # Introductory text about the IUU disciplines
         column(12, style = "padding: 15px 25px 15px;",
                
                includeHTML("./text/02b_edit_policies_iuu_intro.html")
                
         ),
         
         # Manual IUU discipline selection
         column(12, style = "padding: 0px 25px 15px;", 
                
                fluidRow( 
                  
                  ### ------------------------
                  ### Left column: Definitions
                  ### ------------------------
                  column(5, style = "padding: 0 10px;",
                         
                         # Input - Select IUU discipline(s)
                         checkboxGroupInput("w_iuu_definitions", 
                                            
                                            label = tagList(
                                              tags$b(text$item_label[text$item_id == "w_iuu_definitions"]),
                                              # IUU definitions info button
                                              tags$button(id = "info_iuu",
                                                          class = "btn action-button info-button",
                                                          icon("info"))
                                              ), 
                                            choices = c("iuu1", "iuu2", "iuu3", "iuu4"),
                                            selected = c(""),
                                            width = "100%",
                                            inline = FALSE), 
                         
                         # IUU missing data warning
                         tags$i(textOutput("iuu_warning")),
                         
                         br(),
                         
                         # Conditional panel - IUU discipline(s) with no data selected
                         conditionalPanel(condition = "input.w_iuu_definitions.includes('iuu2') | input.w_iuu_definitions.includes('iuu3') | input.w_iuu_definitions.includes('iuu4')",
                                          
                                          # Input - Make IUU assumption
                                          radioButtons("w_iuu_assumption",
                                                       label = tagList(
                                                         tags$b(text$item_label[text$item_id == "w_iuu_assumption"]),
                                                         # IUU assumption info button
                                                         tags$button(id = "info_iuu_assumption",
                                                                     class = "btn action-button info-button",
                                                                     icon("info"))
                                                         ), 
                                                       choices = c("Yes", "No"),
                                                       selected = "No",
                                                       width = "100%",
                                                       inline = FALSE), 
                                          
                                          # Conditional panel - Make IUU assumption selected
                                          conditionalPanel(condition = "input.w_iuu_assumption == 'Yes'",
                                                           
                                                           # Input - Set assumed level of IUU fishing
                                                           sliderInput("w_iuu_percent", 
                                                                       label = tags$b(text$item_label[text$item_id == "w_iuu_percent"]),
                                                                       min = text$min[text$item_id == "w_iuu_percent"],
                                                                       max = text$max[text$item_id == "w_iuu_percent"],
                                                                       value = text$value[text$item_id == "w_iuu_percent"],
                                                                       width = "100%")
                                                           
                                          ) # /conditionalPanel - Make IUU assumption selected
                         ) # /conditionalPanel - IUU discipline(s) with no data selected
                  ), # /column 5 - Left column 
                  
                  ### -------------------------------
                  ### Middle column: scope/allow S&DT
                  ### -------------------------------
                  column(4, style = "padding: 0 10px;",
                         
                         # Conditional panel - At least one IUU discipline(s) selected
                         conditionalPanel('input.w_iuu_definitions.length > 0',
                                          
                                          # Input - Set IUU scope
                                          radioButtons("w_iuu_scope",
                                                       label = tags$b(text$item_label[text$item_id == "w_iuu_scope"]),
                                                       choices = c("all", "other"),
                                                       selected = "all",
                                                       width = "100%",
                                                       inline = FALSE),
                                          
                                          # Conditional panel - Manually select members
                                          conditionalPanel("input.w_iuu_scope == 'select'",
                                                           
                                                           # Input: Manual selection of Members
                                                           selectizeInput("w_iuu_scope_manual",
                                                                          label = tags$b(text$item_label[text$item_id == "w_iuu_scope_manual"]),
                                                                          choices = wto_members_and_observers,
                                                                          selected = NULL,
                                                                          width = "100%",
                                                                          options = list(placeholder = 'Select...'),
                                                                          multiple = T)
                                                           
                                          ), # /conditionalPanel - Manually select members
                                          
                                          tags$hr(),
                                          
                                          # Input: Allow S&DT
                                          radioButtons("w_iuu_allow_sdt",
                                                       label = tagList(
                                                         tags$b(text$item_label[text$item_id == "w_iuu_allow_sdt"]),
                                                         # Info button
                                                         tags$button(id = "info_iuu_sdt",
                                                                     class = "btn action-button info-button",
                                                                     icon("info"))
                                                       ),
                                                       choices = c("Yes", "No"),
                                                       selected = "No",
                                                       width = "100%",
                                                       inline = FALSE)
                                          
                         ) # /conditionalPanel - At least one IUU discipline(s) selected
                  ), # # /column 4 - Middle column 
                  
                  ### ----------------------
                  ### Right column: set S&DT
                  ### ----------------------
                  column(3, style = "padding: 0 10px;",
                         
                         # Conditional panel - S&DT should be allowed
                         conditionalPanel("(input.w_iuu_allow_sdt == 'Yes' && input.w_iuu_definitions.length > 0)",
                                          
                                          # Input - allow S&DT for LDCs
                                          radioButtons("w_iuu_sdt_ldc",
                                                       label = tags$b(text$item_label[text$item_id == "w_iuu_sdt_ldc"]),
                                                       choices = c("Yes", "No"),
                                                       selected = "No",
                                                       width = "100%",
                                                       inline = FALSE),
                                          
                                          # Conditional panel - S&DT should be allowed for LDCs
                                          conditionalPanel("input.w_iuu_sdt_ldc == 'Yes'",
                                                           
                                                           # Input - LDC S&DT
                                                           checkboxGroupInput("w_iuu_sdt_what_ldc",
                                                                              label = tags$b(text$item_label[text$item_id == "w_iuu_sdt_what_ldc"]),
                                                                              choices = c("all", "domestic", "time"),
                                                                              selected = "",
                                                                              width = "100%",
                                                                              inline = FALSE),
                                                           
                                                           # Conditional panel - Time delay for LDCs allowed
                                                           conditionalPanel("input.w_iuu_sdt_what_ldc.includes('time')",
                                                                            
                                                                            # Input - Time delay for LDCs
                                                                            sliderInput("w_iuu_sdt_time_delay_ldc",
                                                                                        label = tags$b(text$item_label[text$item_id == "w_iuu_sdt_time_delay_ldc"]),
                                                                                        min = 0,
                                                                                        max = 5,
                                                                                        value = 1,
                                                                                        width = "100%")
                                                                            
                                                           ) # /conditionalpanel - Time delay for LDCs allowed
                                          ), # /conditionalPanel - S&DT should be allowed for LDCs
                                          
                                          tags$hr(),
                                          
                                          # Input - allow S&DT for developing
                                          radioButtons("w_iuu_sdt_developing",
                                                       label = tags$b(text$item_label[text$item_id == "w_iuu_sdt_developing"]),
                                                       choices = c("Yes", "No"),
                                                       selected = "No",
                                                       width = "100%",
                                                       inline = FALSE),
                                          
                                          # Conditional panel - S&DT should be allowed for developing
                                          conditionalPanel("input.w_iuu_sdt_developing == 'Yes'",
                                                           
                                                           # Input - developing S&DT
                                                           checkboxGroupInput("w_iuu_sdt_what_developing",
                                                                              label = tags$b(text$item_label[text$item_id == "w_iuu_sdt_what_developing"]),
                                                                              choices = c("all", "domestic", "time"),
                                                                              selected = "",
                                                                              width = "100%",
                                                                              inline = FALSE),
                                                           
                                                           # Conditional panel - Time delay for developing allowed
                                                           conditionalPanel("input.w_iuu_sdt_what_developing.includes('time')",
                                                                            # Input - Time delay for developing
                                                                            sliderInput("w_iuu_sdt_time_delay_developing",
                                                                                        label = tags$b(text$item_label[text$item_id == "w_iuu_sdt_time_delay_developing"]),
                                                                                        min = 0,
                                                                                        max = 5,
                                                                                        value = 1,
                                                                                        width = "100%")
                                                                            
                                                           ) # /conditionalpanel - Time delay for developing allowed
                                          ), # /conditionalPanel - S&DT should be allowed for developing
                                          
                                          tags$hr(),
                                          
                                          # Input - allow S&DT for SVEs
                                          radioButtons("w_iuu_sdt_sve",
                                                       label = tags$b(text$item_label[text$item_id == "w_iuu_sdt_sve"]),
                                                       choices = c("Yes", "No"),
                                                       selected = "No",
                                                       width = "100%",
                                                       inline = FALSE),
                                          
                                          # Conditional panel - S&DT should be allowed for SVEs
                                          conditionalPanel("input.w_iuu_sdt_sve == 'Yes'",
                                                           
                                                           # Input - SVE S&DT
                                                           checkboxGroupInput("w_iuu_sdt_what_sve",
                                                                              label = tags$b(text$item_label[text$item_id == "w_iuu_sdt_what_sve"]),
                                                                              choices = c("all", "domestic", "time"),
                                                                              selected = "",
                                                                              width = "100%",
                                                                              inline = FALSE),
                                                           
                                                           # Conditional panel - Time delay for SVE allowed
                                                           conditionalPanel("input.w_iuu_sdt_what_sve.includes('time')",
                                                                            
                                                                            # Input - Time delay for SVE
                                                                            sliderInput("w_iuu_sdt_time_delay_sve",
                                                                                        label = tags$b(text$item_label[text$item_id == "w_iuu_sdt_time_delay_sve"]),
                                                                                        min = 0,
                                                                                        max = 5,
                                                                                        value = 1,
                                                                                        width = "100%")
                                                                            
                                                           ) # /conditionalpanel - Time delay for SVE allowed
                                          ) # /conditionalPanel - S&DT should be allowed for SVE
                                          
                         ) # /conditionalPanel - S&DT should be allowed
     
                  ) # /column 3 - Right column 
                  
                ) # /fluidRow
                
         ), # /column 12 - Manual IUU discipline selection
         
         # Bottom navigation buttons
         fluidRow(
           
           # Previous tab
           column(3, style = "padding: 5px;",
                  
                  tags$button(id = "ab_edit_policies_tabs_iuu_to_instructions",
                              class = "btn action-button nav-button-white-l",
                              icon("chevron-left"), text$item_label[text$item_id == "ab_edit_policies_tabs_iuu_to_instructions"]
                  )
                  
           ),
           
           # Next tab
           column(3, offset = 6, style = "padding: 5px;",
                  
                  tags$button(id = "ab_edit_policies_tabs_iuu_to_oa",
                              class = "btn action-button nav-button-white-r",
                              text$item_label[text$item_id == "ab_edit_policies_tabs_iuu_to_oa"], icon("chevron-right") 
                  )
                  
           )
           
         ) # /fluidRow - Bottom navigation buttons
         
  ) # /column - container for IUU tab box
