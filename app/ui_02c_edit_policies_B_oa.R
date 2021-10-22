### -------------------------------------------------------------------
### SubsidyExplorer
### An interactive toolkit to explore the tradeoffs across proposed fisheries subsidies disciplines
### 
### This script contains the content for the "Overfished" box on the edit-policies" tab
### --------------------------------------------------------------------

OA = function(wto_members_and_observers) 
  
  # Column container for tab panel        
  column(12, id = "edit-policies-tab-panel",
                  
         # Manual IUU discipline selection
         column(12, id = "spaced-div", 
         
                ### ------------------------
                ### Top Row: Disciplines
                ### ------------------------
                fluidRow( 
                  
                  tags$h4("DISCIPLINES: "),
                  
                  column(12,
                         
                         # Input - Select OA discipline(s)
                         checkboxGroupInput("w_oa_definitions", 
                                            
                                            label = tagList(
                                              tags$b(text$item_label[text$item_id == "w_oa_definitions"])
                                            ), 
                                            choices = unlist(wid$choices[wid$item_id == "w_oa_definitions"]),
                                            selected = unlist(wid$selected[wid$item_id == "w_oa_definitions"]),
                                            width = "100%",
                                            inline = FALSE)
                         
                         
                  ) # /column 12
                ), #/fluidRow - top Row
                
                ### ------------------------
                ### Middle Row: Scope
                ### ------------------------
                fluidRow(
                  
                  # Conditional panel - At least one OA discipline(s) selected
                  conditionalPanel('input.w_oa_definitions.length > 0',
                                   
                                   tags$hr(),
                                   tags$h4("SCOPE: "),
                                   
                                   ### Left column
                                   column(6, style = "padding-right: 10px;",
                                          # Input - Should these prohibitions apply to all, or only to selected states/fishing activity
                                          radioButtons("w_oa_scope",
                                                       label = tags$b(text$item_label[text$item_id == "w_oa_scope"]),
                                                       choices = unlist(wid$choices[wid$item_id == "w_oa_scope"]),
                                                       selected = unlist(wid$selected[wid$item_id == "w_oa_scope"]),
                                                       width = "100%",
                                                       inline = FALSE)
                                          
                                   ),
                                   
                                   ### Right column
                                   column(6, style = "padding-left: 10px;",
                                          
                                          # Conditional panel - Only certain states and/or vessel characteristics
                                          conditionalPanel("input.w_oa_scope == 'SELECT'",
                                                           
                                                           # Input - Set OA scope (only certain states and/or vessel characteristics)
                                                           checkboxGroupInput("w_oa_scope_select",
                                                                              label = tags$b(text$item_label[text$item_id == "w_oa_scope_select"]),
                                                                              choices = unlist(wid$choices[wid$item_id == "w_oa_scope_select"]),
                                                                              selected = unlist(wid$selected[wid$item_id == "w_oa_scope_select"]),
                                                                              width = "100%",
                                                                              inline = FALSE),
                                                           
                                                           # Conditional panel - Manually select members
                                                           conditionalPanel("input.w_oa_scope_select.includes('MANUAL')",
                                                                            
                                                                            # Input: Manual selection of Members
                                                                            selectizeInput("w_oa_scope_manual",
                                                                                           label = tags$b(text$item_label[text$item_id == "w_oa_scope_manual"]),
                                                                                           choices = wto_members_and_observers,
                                                                                           selected = NULL,
                                                                                           width = "100%",
                                                                                           options = list(placeholder = 'Select...'),
                                                                                           multiple = T)
                                                                            
                                                                            
                                                           ), # /conditionalPanel - Manually select members
                                                           
                                                           # Conditional panel - High seas scope
                                                           conditionalPanel("input.w_oa_scope_select.includes('HS') || input.w_oa_scope_select.includes('OUT')",
                                                                            
                                                                            # Input: High seas cutoff
                                                                            sliderInput("w_oa_hs_cutoff",
                                                                                        label = tagList(
                                                                                          tags$b(text$item_label[text$item_id == "w_oa_hs_cutoff"])
                                                                                        ),
                                                                                        min = wid$min[wid$item_id == "w_oa_hs_cutoff"],
                                                                                        max = wid$max[wid$item_id == "w_oa_hs_cutoff"],
                                                                                        value = wid$value[wid$item_id == "w_oa_hs_cutoff"],
                                                                                        width = "100%")
                                                                            
                                                                            
                                                           ), # /conditionalPanel - High seas scope
                                                           
                                                           # Conditional panel - Length cutoff
                                                           conditionalPanel("input.w_oa_scope_select.includes('LENGTH')",
                                                                            
                                                                            # Input: High seas cutoff
                                                                            sliderInput("w_oa_length_cutoff",
                                                                                        label = tags$b(text$item_label[text$item_id == "w_oa_length_cutoff"]),
                                                                                        min = wid$min[wid$item_id == "w_oa_length_cutoff"],
                                                                                        max = wid$max[wid$item_id == "w_oa_length_cutoff"],
                                                                                        value = wid$value[wid$item_id == "w_oa_length_cutoff"],
                                                                                        width = "100%")
                                                                            
                                                                            
                                                           ), # /conditionalPanel - Length cutoff
                                                           
                                                           # Conditional panel - tonnage cutoff
                                                           conditionalPanel("input.w_oa_scope_select.includes('TONNAGE')",
                                                                            
                                                                            # Input: High seas cutoff
                                                                            sliderInput("w_oa_tonnage_cutoff",
                                                                                        label = tags$b(text$item_label[text$item_id == "w_oa_tonnage_cutoff"]),
                                                                                        min = wid$min[wid$item_id == "w_oa_tonnage_cutoff"],
                                                                                        max = wid$max[wid$item_id == "w_oa_tonnage_cutoff"],
                                                                                        value = wid$value[wid$item_id == "w_oa_tonnage_cutoff"],
                                                                                        width = "100%")
                                                                            
                                                                            
                                                           ), # /conditionalPanel - tonnage cutoff
                                                           
                                                           # Conditional panel - engine power cutoff
                                                           conditionalPanel("input.w_oa_scope_select.includes('ENGINE')",
                                                                            
                                                                            # Input: High seas cutoff
                                                                            sliderInput("w_oa_engine_cutoff",
                                                                                        label = tags$b(text$item_label[text$item_id == "w_oa_engine_cutoff"]),
                                                                                        min = wid$min[wid$item_id == "w_oa_engine_cutoff"],
                                                                                        max = wid$max[wid$item_id == "w_oa_engine_cutoff"],
                                                                                        value = wid$value[wid$item_id == "w_oa_engine_cutoff"],
                                                                                        width = "100%")
                                                                            
                                                                            
                                                           ) # /conditionalPanel - engine power cutoff
                                          ) #/select scope
                                   ) # /column 6
                  ) #/conditionalPanel
                ), # /fluidRow
                
                ### ------------------------
                ### Bottom Row: S&DT
                ### ------------------------
                fluidRow(
                  
                  # Conditional panel - At least one OA discipline(s) selected
                  conditionalPanel('input.w_oa_definitions.length > 0',
                    
                                   tags$hr(),
                                   tags$h4("S&DT: "),
                                   
                                   # First column
                                   column(6, style = "padding-right: 10px;",
                                          
                                          # Input: Allow S&DT
                                          radioButtons("w_oa_allow_sdt",
                                                       label = tagList(
                                                         tags$b(text$item_label[text$item_id == "w_oa_allow_sdt"])
                                                       ),
                                                       choices = unlist(wid$choices[wid$item_id == "w_oa_allow_sdt"]),
                                                       selected = unlist(wid$selected[wid$item_id == "w_oa_allow_sdt"]),
                                                       width = "100%",
                                                       inline = FALSE),
                                          
                                          # Conditional: Allow S&DT
                                          conditionalPanel("input.w_oa_allow_sdt == 'YES'",
                                                           
                                                           # Input - allow S&DT for LDCs
                                                           radioButtons("w_oa_sdt_ldc",
                                                                        label = tags$b(text$item_label[text$item_id == "w_oa_sdt_ldc"]),
                                                                        choices = unlist(wid$choices[wid$item_id == "w_oa_sdt_ldc"]),
                                                                        selected = unlist(wid$selected[wid$item_id == "w_oa_sdt_ldc"]),
                                                                        width = "100%",
                                                                        inline = FALSE),
                                                           
                                                           # Input - allow S&DT for developing
                                                           radioButtons("w_oa_sdt_developing",
                                                                        label = tags$b(text$item_label[text$item_id == "w_oa_sdt_developing"]),
                                                                        choices = unlist(wid$choices[wid$item_id == "w_oa_sdt_developing"]),
                                                                        selected = unlist(wid$selected[wid$item_id == "w_oa_sdt_developing"]),
                                                                        width = "100%",
                                                                        inline = FALSE),
                                                           
                                                           # Input - allow S&DT for SVEs
                                                           radioButtons("w_oa_sdt_sve",
                                                                        label = tags$b(text$item_label[text$item_id == "w_oa_sdt_sve"]),
                                                                        choices = unlist(wid$choices[wid$item_id == "w_oa_sdt_sve"]),
                                                                        selected = unlist(wid$selected[wid$item_id == "w_oa_sdt_sve"]),
                                                                        width = "100%",
                                                                        inline = FALSE)
                                                           
                                          ) # /conditional - allow S&DT 
                                   ), # /column 4
                                   
                                   # Second column
                                   column(6, style = "padding-left: 10px;",
                                          
                                          # Conditional - Allow S&DT for LDCS
                                          conditionalPanel("(input.w_oa_allow_sdt == 'YES' & input.w_oa_sdt_ldc == 'YES')",
                                                           
                                                           # Input - LDC S&DT
                                                           checkboxGroupInput("w_oa_sdt_what_ldc",
                                                                              label = tags$b(text$item_label[text$item_id == "w_oa_sdt_what_ldc"]),
                                                                              choices = unlist(wid$choices[wid$item_id == "w_oa_sdt_what_ldc"]),
                                                                              selected = unlist(wid$selected[wid$item_id == "w_oa_sdt_what_ldc"]),
                                                                              width = "100%",
                                                                              inline = FALSE),
                                                           
                                                           # Conditional panel - High seas exception for LDCs
                                                           conditionalPanel("input.w_oa_sdt_what_ldc.includes('HS')",
                                                                            
                                                                            # Input - High seas cutoff for LDCs
                                                                            sliderInput("w_oa_sdt_hs_cutoff_ldc",
                                                                                        label = tags$b(text$item_label[text$item_id == "w_oa_sdt_hs_cutoff_ldc"]),
                                                                                        min = wid$min[wid$item_id == "w_oa_sdt_hs_cutoff_ldc"],
                                                                                        max = wid$max[wid$item_id == "w_oa_sdt_hs_cutoff_ldc"],
                                                                                        value = wid$value[wid$item_id == "w_oa_sdt_hs_cutoff_ldc"],
                                                                                        width = "100%")
                                                                            
                                                           ), # /conditionalpanel - High seas exception for LDCs
                                                           
                                                           # Conditional panel - Time delay for LDCs allowed
                                                           conditionalPanel("input.w_oa_sdt_what_ldc.includes('TIME')",
                                                                            
                                                                            # Input - Time delay for LDCs
                                                                            sliderInput("w_oa_sdt_time_delay_ldc",
                                                                                        label = tags$b(text$item_label[text$item_id == "w_oa_sdt_time_delay_ldc"]),
                                                                                        min = wid$min[wid$item_id == "w_oa_sdt_time_delay_ldc"],
                                                                                        max = wid$max[wid$item_id == "w_oa_sdt_time_delay_ldc"],
                                                                                        value = wid$value[wid$item_id == "w_oa_sdt_time_delay_ldc"],
                                                                                        width = "100%")
                                                                            
                                                           ) # /conditionalpanel - Time delay for LDCs allowed
                                          ), # /conditionalPanel - S&DT should be allowed for LDCs
                                          
                                          conditionalPanel("(input.w_oa_allow_sdt == 'YES' & input.w_oa_sdt_developing == 'YES')",
                                                           
                                                           # Input - developing S&DT
                                                           checkboxGroupInput("w_oa_sdt_what_developing",
                                                                              label = tags$b(text$item_label[text$item_id == "w_oa_sdt_what_developing"]),
                                                                              choices = unlist(wid$choices[wid$item_id == "w_oa_sdt_what_developing"]),
                                                                              selected = unlist(wid$selected[wid$item_id == "w_oa_sdt_what_developing"]),
                                                                              width = "100%",
                                                                              inline = FALSE),
                                                           
                                                           # Conditional panel - High seas exception for developing
                                                           conditionalPanel("input.w_oa_sdt_what_developing.includes('HS')",
                                                                            
                                                                            # Input - High seas cutoff for developing
                                                                            sliderInput("w_oa_sdt_hs_cutoff_developing",
                                                                                        label = tags$b(text$item_label[text$item_id == "w_oa_sdt_hs_cutoff_developing"]),
                                                                                        min = wid$min[wid$item_id == "w_oa_sdt_hs_cutoff_developing"],
                                                                                        max = wid$max[wid$item_id == "w_oa_sdt_hs_cutoff_developing"],
                                                                                        value = wid$value[wid$item_id == "w_oa_sdt_hs_cutoff_developing"],
                                                                                        width = "100%")
                                                                            
                                                           ), # /conditionalpanel - High seas exception for developing
                                                           
                                                           # Conditional panel - Time delay for developing allowed
                                                           conditionalPanel("input.w_oa_sdt_what_developing.includes('TIME')",
                                                                            
                                                                            # Input - Time delay for developing
                                                                            sliderInput("w_oa_sdt_time_delay_developing",
                                                                                        label = tags$b(text$item_label[text$item_id == "w_oa_sdt_time_delay_developing"]),
                                                                                        min = wid$min[wid$item_id == "w_oa_sdt_time_delay_developing"],
                                                                                        max = wid$max[wid$item_id == "w_oa_sdt_time_delay_developing"],
                                                                                        value = wid$value[wid$item_id == "w_oa_sdt_time_delay_developing"],
                                                                                        width = "100%")
                                                                            
                                                                            
                                                           ) # /conditionalpanel - Time delay for developing allowed
                                          ), # /conditionalPanel - S&DT should be allowed for developing
                                          
                                          # Conditional panel - S&DT should be allowed for SVEs
                                          conditionalPanel("(input.w_oa_allow_sdt == 'YES' & input.w_oa_sdt_sve == 'YES')",
                                                           
                                                           # Input - SVE S&DT
                                                           checkboxGroupInput("w_oa_sdt_what_sve",
                                                                              label = tags$b(text$item_label[text$item_id == "w_oa_sdt_what_sve"]),
                                                                              choices = unlist(wid$choices[wid$item_id == "w_oa_sdt_what_sve"]),
                                                                              selected = unlist(wid$selected[wid$item_id == "w_oa_sdt_what_sve"]),
                                                                              width = "100%",
                                                                              inline = FALSE),
                                                           
                                                           # Conditional panel - High seas exception for sve
                                                           conditionalPanel("input.w_oa_sdt_what_sve.includes('HS')",
                                                                            
                                                                            # Input - High seas cutoff for sve
                                                                            sliderInput("w_oa_sdt_hs_cutoff_sve",
                                                                                        label = tags$b(text$item_label[text$item_id == "w_oa_sdt_hs_cutoff_sve"]),
                                                                                        min = wid$min[wid$item_id == "w_oa_sdt_hs_cutoff_sve"],
                                                                                        max = wid$max[wid$item_id == "w_oa_sdt_hs_cutoff_sve"],
                                                                                        value = wid$value[wid$item_id == "w_oa_sdt_hs_cutoff_sve"],
                                                                                        width = "100%")
                                                                            
                                                           ), # /conditionalpanel - High seas exception for sve
                                                           
                                                           # Conditional panel - Time delay for SVE allowed
                                                           conditionalPanel("input.w_oa_sdt_what_sve.includes('TIME')",
                                                                            
                                                                            # Input - Time delay for SVE
                                                                            sliderInput("w_oa_sdt_time_delay_sve",
                                                                                        label = tags$b(text$item_label[text$item_id == "w_oa_sdt_time_delay_sve"]),
                                                                                        min = wid$min[wid$item_id == "w_oa_sdt_time_delay_sve"],
                                                                                        max = wid$max[wid$item_id == "w_oa_sdt_time_delay_sve"],
                                                                                        value = wid$value[wid$item_id == "w_oa_sdt_time_delay_sve"],
                                                                                        width = "100%")
                                                                            
                                                                            
                                                           ) # /conditionalpanel - Time delay for SVE allowed
                                          ) # /conditionalPanel - S&DT should be allowed for SVE
                                   ) # /column 8
                                   
                  ) # / conditional - length of definitions > 0
                  
                ) # /fluidRow  
                
         ), # /column 12 - Manual OA discipline selection
         
         # Bottom navigation buttons
         fluidRow(
           
           # Previous tab
           column(3, id = "spaced-div",
                  
                  tags$button(id = "ab_edit_policies_tabs_oa_to_iuu",
                              class = "btn action-button rounded-button-grey",
                              text$item_label[text$item_id == "ab_edit_policies_tabs_oa_to_iuu"]
                  )
                  
           ),
           
           # Next tab
           column(3, offset = 6, id = "spaced-div",
                  
                  tags$button(id = "ab_edit_policies_tabs_oa_to_overcap",
                              class = "btn action-button rounded-button-grey",
                              text$item_label[text$item_id == "ab_edit_policies_tabs_oa_to_overcap"] 
                  )
                  
           )
           
         ) # /fluidRow - Bottom navigation buttons
         
  ) # /column - container for OA tab box
