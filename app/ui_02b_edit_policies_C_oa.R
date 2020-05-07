### -------------------------------------------------------------------
### SubsidyExplorer
### An interactive toolkit to explore the tradeoffs across proposed fisheries subsidies disciplines
### 
### This script contains the content for the "oa" box on the edit-policies" tab
### --------------------------------------------------------------------

OA = function(wto_members_and_observers) 
  
  # Column container for tab panel        
  column(12, style = "border-style: solid; border-width: 2px 3px 3px 3px; border-color: #28292C;",
                  
         # Introductory text about the OA disciplines
         column(12, style = "padding: 15px 25px 15px;",
                
                includeHTML("./text/02b_edit_policies_oa_intro.html")
                
         ),
         
         # Manual OA discipline selection
         column(12, style = "padding: 0px 25px 15px;", 
                
                fluidRow( 
                  
                  ### ------------------------
                  ### Left column: Definitions
                  ### ------------------------
                  column(5, style = "padding: 0 10px;",
                         
                         # Input - Select OA discipline(s)
                         checkboxGroupInput("w_oa_definitions", 
                                            
                                            label = tagList(
                                              tags$b(text$item_label[text$item_id == "w_oa_definitions"]),
                                              # OA definitions info button
                                              tags$button(id = "info_oa",
                                                          class = "btn action-button info-button",
                                                          icon("info"))
                                              ), 
                                            choices = unlist(wid$choices[wid$item_id == "w_oa_definitions"]),
                                            selected = unlist(wid$selected[wid$item_id == "w_oa_definitions"]),
                                            width = "100%",
                                            inline = FALSE)

                  ), # /column 5 - Left column 
                  
                  ### -------------------------------
                  ### Middle column: scope/allow S&DT
                  ### -------------------------------
                  column(4, style = "padding: 0 10px;",
                         
                         # Conditional panel - At least one OA discipline(s) selected
                         conditionalPanel('input.w_oa_definitions.length > 0',
                                          
                                          # Input - Set OA scope
                                          radioButtons("w_oa_scope",
                                                       label = tags$b(text$item_label[text$item_id == "w_oa_scope"]),
                                                       choices = unlist(wid$choices[wid$item_id == "w_oa_scope"]),
                                                       selected = unlist(wid$selected[wid$item_id == "w_oa_scope"]),
                                                       width = "100%",
                                                       inline = FALSE),
                                          
                                          # Conditional panel - Manually select members
                                          conditionalPanel("input.w_oa_scope == 'SELECT'",
                                                           
                                                           # Input: Manual selection of Members
                                                           selectizeInput("w_oa_scope_manual",
                                                                          label = tags$b(text$item_label[text$item_id == "w_oa_scope_manual"]),
                                                                          choices = wto_members_and_observers,
                                                                          selected = "",
                                                                          width = "100%",
                                                                          options = list(placeholder = 'Select...'),
                                                                          multiple = T)
                                                           
                                          ), # /conditionalPanel - Manually select members
                                          
                                          # Conditional panel - High seas scope
                                          conditionalPanel("input.w_oa_scope == 'HS' || input.w_oa_scope == 'OUT'",
                                                           
                                                           # Input: High seas cutoff
                                                           sliderInput("w_oa_hs_cutoff",
                                                                       label = tagList(
                                                                         tags$b(text$item_label[text$item_id == "w_oa_hs_cutoff"]),
                                                                         # Info button
                                                                         tags$button(id = "info_oa_hs",
                                                                                     class = "btn action-button info-button",
                                                                                     icon("info"))),
                                                                       min = wid$min[wid$item_id == "w_oa_hs_cutoff"],
                                                                       max = wid$max[wid$item_id == "w_oa_hs_cutoff"],
                                                                       value = wid$value[wid$item_id == "w_oa_hs_cutoff"],
                                                                       width = "100%")
                                                           
                                          ), # /conditionalPanel - High seas scope
                                          
                                          # Conditional panel - Length cutoff
                                          conditionalPanel("input.w_oa_scope == 'LENGTH' || input.w_oa_scope == 'LTE'",
                                                           
                                                           # Input: High seas cutoff
                                                           sliderInput("w_oa_length_cutoff",
                                                                       label = tags$b(text$item_label[text$item_id == "w_oa_length_cutoff"]),
                                                                       min = wid$min[wid$item_id == "w_oa_length_cutoff"],
                                                                       max = wid$max[wid$item_id == "w_oa_length_cutoff"],
                                                                       value = wid$value[wid$item_id == "w_oa_length_cutoff"],
                                                                       width = "100%")
                                                           
                                          ), # /conditionalPanel - Length cutoff
                                          
                                          # Conditional panel - tonnage cutoff
                                          conditionalPanel("input.w_oa_scope == 'TONNAGE' || input.w_oa_scope == 'LTE'",
                                                           
                                                           # Input: High seas cutoff
                                                           sliderInput("w_oa_tonnage_cutoff",
                                                                       label = tags$b(text$item_label[text$item_id == "w_oa_tonnage_cutoff"]),
                                                                       min = wid$min[wid$item_id == "w_oa_tonnage_cutoff"],
                                                                       max = wid$max[wid$item_id == "w_oa_tonnage_cutoff"],
                                                                       value = wid$value[wid$item_id == "w_oa_tonnage_cutoff"],
                                                                       width = "100%")
                                                           
                                          ), # /conditionalPanel - tonnage cutoff
                                          
                                          # Conditional panel - engine power cutoff
                                          conditionalPanel("input.w_oa_scope == 'ENGINE' || input.w_oa_scope == 'LTE'",
                                                           
                                                           # Input: High seas cutoff
                                                           sliderInput("w_oa_engine_cutoff",
                                                                       label = tags$b(text$item_label[text$item_id == "w_oa_engine_cutoff"]),
                                                                       min = wid$min[wid$item_id == "w_oa_engine_cutoff"],
                                                                       max = wid$max[wid$item_id == "w_oa_engine_cutoff"],
                                                                       value = wid$value[wid$item_id == "w_oa_engine_cutoff"],
                                                                       width = "100%")
                                                           
                                          ), # /conditionalPanel - engine power cutoff
                                          
                                          
                                          tags$hr(),
                                          
                                          # Input: Allow S&DT
                                          radioButtons("w_oa_allow_sdt",
                                                       label = tagList(
                                                         tags$b(text$item_label[text$item_id == "w_oa_allow_sdt"]),
                                                         
                                                         # Info button
                                                         tags$button(id = "info_oa_sdt",
                                                                     class = "btn action-button info-button",
                                                                     icon("info"))
                                                       ),
                                                       choices = unlist(wid$choices[wid$item_id == "w_oa_allow_sdt"]),
                                                       selected = unlist(wid$selected[wid$item_id == "w_oa_allow_sdt"]),
                                                       width = "100%",
                                                       inline = FALSE)
                                          
                         ) # /conditionalPanel - At least one OA discipline(s) selected
                  ), # # /column 4 - Middle column 
                  
                  ### ----------------------
                  ### Right column: set S&DT
                  ### ----------------------
                  column(3, style = "padding: 0 10px;",
                         
                         # Conditional panel - S&DT should be allowed
                         conditionalPanel("(input.w_oa_allow_sdt == 'Yes' && input.w_oa_definitions.length > 0)",
                                          
                                          # Input - allow S&DT for LDCs
                                          radioButtons("w_oa_sdt_ldc",
                                                       label = tags$b(text$item_label[text$item_id == "w_oa_sdt_ldc"]),
                                                       choices = unlist(wid$choices[wid$item_id == "w_oa_sdt_ldc"]),
                                                       selected = unlist(wid$selected[wid$item_id == "w_oa_sdt_ldc"]),
                                                       width = "100%",
                                                       inline = FALSE),
                                          
                                          # Conditional panel - S&DT should be allowed for LDCs
                                          conditionalPanel("input.w_oa_sdt_ldc == 'Yes'",
                                                           
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
                                          
                                          tags$hr(),
                                          
                                          # Input - allow S&DT for developing
                                          radioButtons("w_oa_sdt_developing",
                                                       label = tags$b(text$item_label[text$item_id == "w_oa_sdt_developing"]),
                                                       choices = unlist(wid$choices[wid$item_id == "w_oa_sdt_developing"]),
                                                       selected = unlist(wid$selected[wid$item_id == "w_oa_sdt_developing"]),
                                                       width = "100%",
                                                       inline = FALSE),
                                          
                                          # Conditional panel - S&DT should be allowed for developing
                                          conditionalPanel("input.w_oa_sdt_developing == 'Yes'",
                                                           
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
                                          
                                          tags$hr(),
                                          
                                          # Input - allow S&DT for SVEs
                                          radioButtons("w_oa_sdt_sve",
                                                       label = tags$b(text$item_label[text$item_id == "w_oa_sdt_sve"]),
                                                       choices = unlist(wid$choices[wid$item_id == "w_oa_sdt_sve"]),
                                                       selected = unlist(wid$selected[wid$item_id == "w_oa_sdt_sve"]),
                                                       width = "100%",
                                                       inline = FALSE),
                                          
                                          # Conditional panel - S&DT should be allowed for SVEs
                                          conditionalPanel("input.w_oa_sdt_sve == 'Yes'",
                                                           
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
                                          
                         ) # /conditionalPanel - S&DT should be allowed
     
                  ) # /column 3 - Right column 
                  
                ) # /fluidRow
                
         ), # /column 12 - Manual OA discipline selection
         
         # Bottom navigation buttons
         fluidRow(
           
           # Previous tab
           column(3, style = "padding: 5px;",
                  
                  tags$button(id = "ab_edit_policies_tabs_oa_to_iuu",
                              class = "btn action-button nav-button-white-l",
                              icon("chevron-left"), text$item_label[text$item_id == "ab_edit_policies_tabs_oa_to_iuu"]
                  )
                  
           ),
           
           # Next tab
           column(3, offset = 6, style = "padding: 5px;",
                  
                  tags$button(id = "ab_edit_policies_tabs_oa_to_overcap",
                              class = "btn action-button nav-button-white-r",
                              text$item_label[text$item_id == "ab_edit_policies_tabs_oa_to_overcap"], icon("chevron-right") 
                  )
                  
           )
           
         ) # /fluidRow - Bottom navigation buttons
         
  ) # /column - container for OA tab box
