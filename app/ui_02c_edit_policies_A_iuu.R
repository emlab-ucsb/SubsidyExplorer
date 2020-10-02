### -------------------------------------------------------------------
### SubsidyExplorer
### An interactive toolkit to explore the tradeoffs across proposed fisheries subsidies disciplines
### 
### This script contains the content for the "iuu" box on the edit-policies" tab
### --------------------------------------------------------------------

IUU = function(wto_members_and_observers) 
  
  # Column container for tab panel        
  column(12, id = "edit-policies-tab-panel",
         
         # Manual IUU discipline selection
         column(12, id = "spaced-div", 
                
                ### ------------------------
                ### Top Row: Disciplines
                ### ------------------------
                fluidRow(
                  
                  tags$h4("DISCIPLINES: "),
                  
                  column(6, style = "padding-right: 10px;",
                         
                         # Input - Select IUU discipline(s)
                         checkboxGroupInput("w_iuu_definitions", 
                                            
                                            label = tagList(
                                              tags$b(text$item_label[text$item_id == "w_iuu_definitions"])
                                              # # IUU definitions info button
                                              # tags$button(id = "info_iuu",
                                              #             class = "btn action-button info-button",
                                              #             icon("info"))
                                            ), 
                                            choices = unlist(wid$choices[wid$item_id == "w_iuu_definitions"]),
                                            selected = unlist(wid$selected[wid$item_id == "w_iuu_definitions"]),
                                            width = "100%",
                                            inline = FALSE), 
                         
                         # IUU missing data warning
                         tags$i(textOutput("iuu_warning"))
                    
                  ),
                  
                  column(6, style = "padding-left: 10px;",
                         
                         conditionalPanel(condition = "input.w_iuu_definitions.includes('IUU2') | input.w_iuu_definitions.includes('IUU3') | input.w_iuu_definitions.includes('IUU4') | input.w_iuu_definitions.includes('IUU5') | input.w_iuu_definitions.includes('IUU6')",
                                          
                                          # Input - Make IUU assumption
                                          radioButtons("w_iuu_assumption",
                                                       label = tagList(
                                                         tags$b(text$item_label[text$item_id == "w_iuu_assumption"])
                                                         # # IUU assumption info button
                                                         # tags$button(id = "info_iuu_assumption",
                                                         #             class = "btn action-button info-button",
                                                         #             icon("info"))
                                                       ), 
                                                       choices = unlist(wid$choices[wid$item_id == "w_iuu_assumption"]),
                                                       selected = unlist(wid$selected[wid$item_id == "w_iuu_assumption"]),
                                                       width = "100%",
                                                       inline = FALSE), 
                                          
                                          # Conditional panel - Make IUU assumption selected
                                          conditionalPanel(condition = "input.w_iuu_assumption == 'YES'",
                                                           
                                                           # Input - Set assumed level of IUU fishing
                                                           sliderInput("w_iuu_percent", 
                                                                       label = tags$b(text$item_label[text$item_id == "w_iuu_percent"]),
                                                                       min = wid$min[wid$item_id == "w_iuu_percent"],
                                                                       max = wid$max[wid$item_id == "w_iuu_percent"],
                                                                       value = wid$value[wid$item_id == "w_iuu_percent"],
                                                                       width = "100%")
                                                           
                                          ) # /conditionalPanel - Make IUU assumption selected
                         ) # /conditionalPanel - IUU discipline(s) with no data selected
                         
                  ) #/column 4
                  
                ), #/fluidRow - top Row
                
                ### ------------------------
                ### Middle Row: Scope
                ### ------------------------
                fluidRow(
                  
                  # Conditional panel - At least one IUU discipline(s) selected
                  conditionalPanel('input.w_iuu_definitions.length > 0',
                                   
                                   tags$hr(),
                                   tags$h4("SCOPE: "),

                                   column(6, style = "padding-right: 10px;",
                                          # Input - Should these prohibitions apply to all, or only to selected states/fishing activity
                                          radioButtons("w_iuu_scope",
                                                       label = tags$b(text$item_label[text$item_id == "w_iuu_scope"]),
                                                       choices = unlist(wid$choices[wid$item_id == "w_iuu_scope"]),
                                                       selected = unlist(wid$selected[wid$item_id == "w_iuu_scope"]),
                                                       width = "100%",
                                                       inline = FALSE)
                                          
                                   ),
                                   
                                   column(6, style = "padding-left: 10px;",
                                          
                                          # Conditional panel - Only certain states and/or vessel characteristics
                                          conditionalPanel("input.w_iuu_scope == 'SELECT'",
                                                           
                                                           # Input - Set OA scope (only certain states and/or vessel characteristics)
                                                           checkboxGroupInput("w_iuu_scope_select",
                                                                              label = tags$b(text$item_label[text$item_id == "w_iuu_scope_select"]),
                                                                              choices = unlist(wid$choices[wid$item_id == "w_iuu_scope_select"]),
                                                                              selected = unlist(wid$selected[wid$item_id == "w_iuu_scope_select"]),
                                                                              width = "100%",
                                                                              inline = FALSE),
                                                           
                                                           # Conditional panel - Manually select members
                                                           conditionalPanel("input.w_iuu_scope_select.includes('MANUAL')",
                                                                            
                                                                            # Input: Manual selection of Members
                                                                            selectizeInput("w_iuu_scope_manual",
                                                                                           label = tags$b(text$item_label[text$item_id == "w_iuu_scope_manual"]),
                                                                                           choices = wto_members_and_observers,
                                                                                           selected = "",
                                                                                           width = "100%",
                                                                                           options = list(placeholder = 'Select...'),
                                                                                           multiple = T)
                                                                            
                                                                            
                                                           ) # /conditionalPanel - Manually select members
                                          ) #/select scope
                                   ) # /column 8
                  ) #/conditionalPanel
                ), # /fluidRow
                
                ### ------------------------
                ### Bottom Row: S&DT
                ### ------------------------
                fluidRow(
                  
                  # Conditional panel - At least one IUU discipline(s) selected
                  conditionalPanel('input.w_iuu_definitions.length > 0',
                                   
                                   tags$hr(),
                                   tags$h4("S&DT: "),
                  
                                   # First column
                                   column(6, style = "padding-right: 10px;",

                                          # Input: Allow S&DT
                                          radioButtons("w_iuu_allow_sdt",
                                                       label = tagList(
                                                         tags$b(text$item_label[text$item_id == "w_iuu_allow_sdt"])
                                                         # # Info button
                                                         # tags$button(id = "info_iuu_sdt",
                                                         #             class = "btn action-button info-button",
                                                         #             icon("info"))
                                                       ),
                                                       choices = unlist(wid$choices[wid$item_id == "w_iuu_allow_sdt"]),
                                                       selected = unlist(wid$selected[wid$item_id == "w_iuu_allow_sdt"]),
                                                       width = "100%",
                                                       inline = FALSE),
                                          
                                          # Conditional: Allow S&DT
                                          conditionalPanel("input.w_iuu_allow_sdt == 'YES'",
                                                           
                                                           # Input - allow S&DT for LDCs
                                                           radioButtons("w_iuu_sdt_ldc",
                                                                        label = tags$b(text$item_label[text$item_id == "w_iuu_sdt_ldc"]),
                                                                        choices = unlist(wid$choices[wid$item_id == "w_iuu_sdt_ldc"]),
                                                                        selected = unlist(wid$selected[wid$item_id == "w_iuu_sdt_ldc"]),
                                                                        width = "100%",
                                                                        inline = FALSE),
                                                           
                                                           # Input - allow S&DT for developing
                                                           radioButtons("w_iuu_sdt_developing",
                                                                        label = tags$b(text$item_label[text$item_id == "w_iuu_sdt_developing"]),
                                                                        choices = unlist(wid$choices[wid$item_id == "w_iuu_sdt_developing"]),
                                                                        selected = unlist(wid$selected[wid$item_id == "w_iuu_sdt_developing"]),
                                                                        width = "100%",
                                                                        inline = FALSE),
                                                           
                                                           # Input - allow S&DT for SVEs
                                                           radioButtons("w_iuu_sdt_sve",
                                                                        label = tags$b(text$item_label[text$item_id == "w_iuu_sdt_sve"]),
                                                                        choices = unlist(wid$choices[wid$item_id == "w_iuu_sdt_sve"]),
                                                                        selected = unlist(wid$selected[wid$item_id == "w_iuu_sdt_sve"]),
                                                                        width = "100%",
                                                                        inline = FALSE)
                                                           
                                          ) # /conditional - allow S&DT 
                                   ), # /column 4
                                   
                                   # Second column
                                   column(6, style = "padding-left: 10px;",
                                          
                                          # Conditional - Allow S&DT for LDCS
                                          conditionalPanel("(input.w_iuu_allow_sdt == 'YES' & input.w_iuu_sdt_ldc == 'YES')",
                                                                            
                                                           # Input - LDC S&DT
                                                           checkboxGroupInput("w_iuu_sdt_what_ldc",
                                                                              label = tags$b(text$item_label[text$item_id == "w_iuu_sdt_what_ldc"]),
                                                                              choices = unlist(wid$choices[wid$item_id == "w_iuu_sdt_what_ldc"]),
                                                                              selected = unlist(wid$selected[wid$item_id == "w_iuu_sdt_what_ldc"]),
                                                                              width = "100%",
                                                                              inline = FALSE),
                                                                            
                                                           # Conditional panel - Time delay for LDCs allowed
                                                           conditionalPanel("input.w_iuu_sdt_what_ldc.includes('TIME')",
                                                                                             
                                                                            # Input - Time delay for LDCs
                                                                            sliderInput("w_iuu_sdt_time_delay_ldc",
                                                                                        label = tags$b(text$item_label[text$item_id == "w_iuu_sdt_time_delay_ldc"]),
                                                                                        min = wid$min[wid$item_id == "w_iuu_sdt_time_delay_ldc"],
                                                                                        max = wid$max[wid$item_id == "w_iuu_sdt_time_delay_ldc"],
                                                                                        value = wid$value[wid$item_id == "w_iuu_sdt_time_delay_ldc"],
                                                                                        width = "100%")
                                                                                             
                                                           ) # /conditionalpanel - Time delay for LDCs allowed
                                          ), # /conditionalPanel - S&DT should be allowed for LDCs
                                          
                                          conditionalPanel("(input.w_iuu_allow_sdt == 'YES' & input.w_iuu_sdt_developing == 'YES')",
                                                                            
                                                           # Input - developing S&DT
                                                           checkboxGroupInput("w_iuu_sdt_what_developing",
                                                                              label = tags$b(text$item_label[text$item_id == "w_iuu_sdt_what_developing"]),
                                                                              choices = unlist(wid$choices[wid$item_id == "w_iuu_sdt_what_developing"]),
                                                                              selected = unlist(wid$selected[wid$item_id == "w_iuu_sdt_what_developing"]),
                                                                              width = "100%",
                                                                              inline = FALSE),
                                                                            
                                                           
                                                           # Conditional panel - Time delay for developing allowed
                                                           conditionalPanel("input.w_iuu_sdt_what_developing.includes('TIME')",
                                                                                             
                                                                            # Input - Time delay for developing
                                                                            sliderInput("w_iuu_sdt_time_delay_developing",
                                                                                        label = tags$b(text$item_label[text$item_id == "w_iuu_sdt_time_delay_developing"]),
                                                                                        min = wid$min[wid$item_id == "w_iuu_sdt_time_delay_developing"],
                                                                                        max = wid$max[wid$item_id == "w_iuu_sdt_time_delay_developing"],
                                                                                        value = wid$value[wid$item_id == "w_iuu_sdt_time_delay_developing"],
                                                                                        width = "100%")
                                                                                             
                                                                            
                                                           ) # /conditionalpanel - Time delay for developing allowed
                                          ), # /conditionalPanel - S&DT should be allowed for developing
                                          
                                          # Conditional panel - S&DT should be allowed for SVEs
                                          conditionalPanel("(input.w_iuu_allow_sdt == 'YES' & input.w_iuu_sdt_sve == 'YES')",
                                                                            
                                                           # Input - SVE S&DT
                                                           checkboxGroupInput("w_iuu_sdt_what_sve",
                                                                              label = tags$b(text$item_label[text$item_id == "w_iuu_sdt_what_sve"]),
                                                                              choices = unlist(wid$choices[wid$item_id == "w_iuu_sdt_what_sve"]),
                                                                              selected = unlist(wid$selected[wid$item_id == "w_iuu_sdt_what_sve"]),
                                                                              width = "100%",
                                                                              inline = FALSE),
                                                                            
                                                           
                                                           # Conditional panel - Time delay for SVE allowed
                                                           conditionalPanel("input.w_iuu_sdt_what_sve.includes('TIME')",
                                                                                             
                                                                            # Input - Time delay for SVE
                                                                            sliderInput("w_iuu_sdt_time_delay_sve",
                                                                                        label = tags$b(text$item_label[text$item_id == "w_iuu_sdt_time_delay_sve"]),
                                                                                        min = wid$min[wid$item_id == "w_iuu_sdt_time_delay_sve"],
                                                                                        max = wid$max[wid$item_id == "w_iuu_sdt_time_delay_sve"],
                                                                                        value = wid$value[wid$item_id == "w_iuu_sdt_time_delay_sve"],
                                                                                        width = "100%")
                                                                                             
                                                                            
                                                           ) # /conditionalpanel - Time delay for SVE allowed
                                          ) # /conditionalPanel - S&DT should be allowed for SVE
                                   ) # /column 8
                  ) # / conditional - length of definitions > 0

                ) # /fluidRow
                
         ), # /column 12 - Manual IUU discipline selection
         
         # Bottom navigation buttons
         fluidRow(
           
           # Next tab
           column(3, offset = 9, id = "spaced-div",
                  
                  tags$button(id = "ab_edit_policies_tabs_iuu_to_oa",
                              class = "btn action-button rounded-button-grey",
                              text$item_label[text$item_id == "ab_edit_policies_tabs_iuu_to_oa"]
                  )
                  
           )
           
         ) # /fluidRow - Bottom navigation buttons
         
  ) # /column - container for IUU tab box
