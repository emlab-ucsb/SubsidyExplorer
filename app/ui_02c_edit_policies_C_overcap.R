### -------------------------------------------------------------------
### SubsidyExplorer
### An interactive toolkit to explore the tradeoffs across proposed fisheries subsidies disciplines
### 
### This script contains the content for the "overcap" box on the edit-policies" tab
### --------------------------------------------------------------------

Overcap = function(wto_members_and_observers, subsidy_types_sorted_sumaila) 
  
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
                         
                         # Input - Select OCOF discipline(s)
                         checkboxGroupInput("w_overcap_definitions", 
                                            
                                            label = tagList(
                                              tags$b(text$item_label[text$item_id == "w_overcap_definitions"]),
                                              # OA definitions info button
                                              tags$button(id = "info_overcap",
                                                          class = "btn action-button info-button",
                                                          icon("info"))
                                            ), 
                                            choices = subsidy_types_sorted_sumaila[4:13],
                                            selected = NULL,
                                            width = "100%",
                                            inline = FALSE)
                         
                         
                  ) # /column 12
                ), #/fluidRow - top Row
                
                ### ------------------------
                ### Middle Row: Scope
                ### ------------------------
                fluidRow(
                  
                  # Conditional panel - At least one OCOF discipline(s) selected
                  conditionalPanel('input.w_overcap_definitions.length > 0',
                                   
                                   tags$hr(),
                                   tags$h4("SCOPE: "),
                                   
                                   ### Left column
                                   column(6, style = "padding-right: 10px;",
                                          # Input - Should these prohibitions apply to all, or only to selected states/fishing activity
                                          radioButtons("w_overcap_scope",
                                                       label = tags$b(text$item_label[text$item_id == "w_overcap_scope"]),
                                                       choices = unlist(wid$choices[wid$item_id == "w_overcap_scope"]),
                                                       selected = unlist(wid$selected[wid$item_id == "w_overcap_scope"]),
                                                       width = "100%",
                                                       inline = FALSE)
                                          
                                   ),
                                   
                                   ### Right column
                                   column(6, style = "padding-left: 10px;",
                                          
                                          # Conditional panel - Only certain states and/or vessel characteristics
                                          conditionalPanel("input.w_overcap_scope == 'SELECT'",
                                                           
                                                           # Input - Set OCOF scope (only certain states and/or vessel characteristics)
                                                           checkboxGroupInput("w_overcap_scope_select",
                                                                              label = tags$b(text$item_label[text$item_id == "w_overcap_scope_select"]),
                                                                              choices = unlist(wid$choices[wid$item_id == "w_overcap_scope_select"]),
                                                                              selected = unlist(wid$selected[wid$item_id == "w_overcap_scope_select"]),
                                                                              width = "100%",
                                                                              inline = FALSE),
                                                           
                                                           # Conditional panel - Manually select members
                                                           conditionalPanel("input.w_overcap_scope_select.includes('MANUAL')",
                                                                            
                                                                            # Input: Manual selection of Members
                                                                            selectizeInput("w_overcap_scope_manual",
                                                                                           label = tags$b(text$item_label[text$item_id == "w_overcap_scope_manual"]),
                                                                                           choices = wto_members_and_observers,
                                                                                           selected = "",
                                                                                           width = "100%",
                                                                                           options = list(placeholder = 'Select...'),
                                                                                           multiple = T)
                                                                            
                                                                            
                                                           ), # /conditionalPanel - Manually select members
                                                           
                                                           # Conditional panel - High seas scope
                                                           conditionalPanel("input.w_overcap_scope_select.includes('HS') || input.w_overcap_scope_select.includes('OUT')",
                                                                            
                                                                            # Input: High seas cutoff
                                                                            sliderInput("w_overcap_hs_cutoff",
                                                                                        label = tagList(
                                                                                          tags$b(text$item_label[text$item_id == "w_overcap_hs_cutoff"]),
                                                                                          # Info button
                                                                                          tags$button(id = "info_overcap_hs",
                                                                                                      class = "btn action-button info-button",
                                                                                                      icon("info"))
                                                                                        ),
                                                                                        min = wid$min[wid$item_id == "w_overcap_hs_cutoff"],
                                                                                        max = wid$max[wid$item_id == "w_overcap_hs_cutoff"],
                                                                                        value = wid$value[wid$item_id == "w_overcap_hs_cutoff"],
                                                                                        width = "100%")
                                                                            
                                                                            
                                                           ), # /conditionalPanel - High seas scope
                                                           
                                                           # Conditional panel - Length cutoff
                                                           conditionalPanel("input.w_overcap_scope_select.includes('LENGTH')",
                                                                            
                                                                            # Input: High seas cutoff
                                                                            sliderInput("w_overcap_length_cutoff",
                                                                                        label = tags$b(text$item_label[text$item_id == "w_overcap_length_cutoff"]),
                                                                                        min = wid$min[wid$item_id == "w_overcap_length_cutoff"],
                                                                                        max = wid$max[wid$item_id == "w_overcap_length_cutoff"],
                                                                                        value = wid$value[wid$item_id == "w_overcap_length_cutoff"],
                                                                                        width = "100%")
                                                                            
                                                                            
                                                           ), # /conditionalPanel - Length cutoff
                                                           
                                                           # Conditional panel - tonnage cutoff
                                                           conditionalPanel("input.w_overcap_scope_select.includes('TONNAGE')",
                                                                            
                                                                            # Input: High seas cutoff
                                                                            sliderInput("w_overcap_tonnage_cutoff",
                                                                                        label = tags$b(text$item_label[text$item_id == "w_overcap_tonnage_cutoff"]),
                                                                                        min = wid$min[wid$item_id == "w_overcap_tonnage_cutoff"],
                                                                                        max = wid$max[wid$item_id == "w_overcap_tonnage_cutoff"],
                                                                                        value = wid$value[wid$item_id == "w_overcap_tonnage_cutoff"],
                                                                                        width = "100%")
                                                                            
                                                                            
                                                           ), # /conditionalPanel - tonnage cutoff
                                                           
                                                           # Conditional panel - engine power cutoff
                                                           conditionalPanel("input.w_overcap_scope_select.includes('ENGINE')",
                                                                            
                                                                            # Input: High seas cutoff
                                                                            sliderInput("w_overcap_engine_cutoff",
                                                                                        label = tags$b(text$item_label[text$item_id == "w_overcap_engine_cutoff"]),
                                                                                        min = wid$min[wid$item_id == "w_overcap_engine_cutoff"],
                                                                                        max = wid$max[wid$item_id == "w_overcap_engine_cutoff"],
                                                                                        value = wid$value[wid$item_id == "w_overcap_engine_cutoff"],
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
                  
                  # Conditional panel - At least one OCOF discipline(s) selected
                  conditionalPanel("input.w_overcap_definitions.length > 0",
                                   
                                   tags$hr(),
                                   tags$h4("S&DT: "),
                                   
                                   # First column
                                   column(6, style = "padding-right: 10px;",
                                          
                                          # Input: Allow S&DT
                                          radioButtons("w_overcap_allow_sdt",
                                                       label = tagList(
                                                         tags$b(text$item_label[text$item_id == "w_overcap_allow_sdt"]),
                                                         # Info button
                                                         tags$button(id = "info_overcap_sdt",
                                                                     class = "btn action-button info-button",
                                                                     icon("info"))
                                                       ),
                                                       choices = unlist(wid$choices[wid$item_id == "w_overcap_allow_sdt"]),
                                                       selected = unlist(wid$selected[wid$item_id == "w_overcap_allow_sdt"]),
                                                       width = "100%",
                                                       inline = FALSE),
                                          
                                          # Conditional: Allow S&DT
                                          conditionalPanel("input.w_overcap_allow_sdt == 'YES'",
                                                           
                                                           # Input - allow S&DT for LDCs
                                                           radioButtons("w_overcap_sdt_ldc",
                                                                        label = tags$b(text$item_label[text$item_id == "w_overcap_sdt_ldc"]),
                                                                        choices = unlist(wid$choices[wid$item_id == "w_overcap_sdt_ldc"]),
                                                                        selected = unlist(wid$selected[wid$item_id == "w_overcap_sdt_ldc"]),
                                                                        width = "100%",
                                                                        inline = FALSE),
                                                           
                                                           # Input - allow S&DT for developing
                                                           radioButtons("w_overcap_sdt_developing",
                                                                        label = tags$b(text$item_label[text$item_id == "w_overcap_sdt_developing"]),
                                                                        choices = unlist(wid$choices[wid$item_id == "w_overcap_sdt_developing"]),
                                                                        selected = unlist(wid$selected[wid$item_id == "w_overcap_sdt_developing"]),
                                                                        width = "100%",
                                                                        inline = FALSE),
                                                           
                                                           # Input - allow S&DT for SVEs
                                                           radioButtons("w_overcap_sdt_sve",
                                                                        label = tags$b(text$item_label[text$item_id == "w_overcap_sdt_sve"]),
                                                                        choices = unlist(wid$choices[wid$item_id == "w_overcap_sdt_sve"]),
                                                                        selected = unlist(wid$selected[wid$item_id == "w_overcap_sdt_sve"]),
                                                                        width = "100%",
                                                                        inline = FALSE)
                                                           
                                          ) # /conditional - allow S&DT 
                                   ), # /column 4
                                   
                                   # Second column
                                   column(6, style = "padding-left: 10px;",
                                          
                                          # Conditional - Allow S&DT for LDCS
                                          conditionalPanel("(input.w_overcap_allow_sdt == 'YES' & input.w_overcap_sdt_ldc == 'YES')",
                                                           
                                                           # Input - LDC S&DT
                                                           checkboxGroupInput("w_overcap_sdt_what_ldc",
                                                                              label = tags$b(text$item_label[text$item_id == "w_overcap_sdt_what_ldc"]),
                                                                              choices = unlist(wid$choices[wid$item_id == "w_overcap_sdt_what_ldc"]),
                                                                              selected = unlist(wid$selected[wid$item_id == "w_overcap_sdt_what_ldc"]),
                                                                              width = "100%",
                                                                              inline = FALSE),
                                                           
                                                           # Conditional panel - High seas exception for LDCs
                                                           conditionalPanel("input.w_overcap_sdt_what_ldc.includes('HS')",
                                                                            
                                                                            # Input - High seas cutoff for LDCs
                                                                            sliderInput("w_overcap_sdt_hs_cutoff_ldc",
                                                                                        label = tags$b(text$item_label[text$item_id == "w_overcap_sdt_hs_cutoff_ldc"]),
                                                                                        min = wid$min[wid$item_id == "w_overcap_sdt_hs_cutoff_ldc"],
                                                                                        max = wid$max[wid$item_id == "w_overcap_sdt_hs_cutoff_ldc"],
                                                                                        value = wid$value[wid$item_id == "w_overcap_sdt_hs_cutoff_ldc"],
                                                                                        width = "100%")
                                                                            
                                                           ), # /conditionalpanel - High seas exception for LDCs
                                                           
                                                           # Conditional panel - Time delay for LDCs allowed
                                                           conditionalPanel("input.w_overcap_sdt_what_ldc.includes('TIME')",
                                                                            
                                                                            # Input - Time delay for LDCs
                                                                            sliderInput("w_overcap_sdt_time_delay_ldc",
                                                                                        label = tags$b(text$item_label[text$item_id == "w_overcap_sdt_time_delay_ldc"]),
                                                                                        min = wid$min[wid$item_id == "w_overcap_sdt_time_delay_ldc"],
                                                                                        max = wid$max[wid$item_id == "w_overcap_sdt_time_delay_ldc"],
                                                                                        value = wid$value[wid$item_id == "w_overcap_sdt_time_delay_ldc"],
                                                                                        width = "100%")
                                                                            
                                                           ) # /conditionalpanel - Time delay for LDCs allowed
                                          ), # /conditionalPanel - S&DT should be allowed for LDCs
                                          
                                          conditionalPanel("(input.w_overcap_allow_sdt == 'YES' & input.w_overcap_sdt_developing == 'YES')",
                                                           
                                                           # Input - developing S&DT
                                                           checkboxGroupInput("w_overcap_sdt_what_developing",
                                                                              label = tags$b(text$item_label[text$item_id == "w_overcap_sdt_what_developing"]),
                                                                              choices = unlist(wid$choices[wid$item_id == "w_overcap_sdt_what_developing"]),
                                                                              selected = unlist(wid$selected[wid$item_id == "w_overcap_sdt_what_developing"]),
                                                                              width = "100%",
                                                                              inline = FALSE),
                                                           
                                                           # Conditional panel - High seas exception for developing
                                                           conditionalPanel("input.w_overcap_sdt_what_developing.includes('HS')",
                                                                            
                                                                            # Input - High seas cutoff for developing
                                                                            sliderInput("w_overcap_sdt_hs_cutoff_developing",
                                                                                        label = tags$b(text$item_label[text$item_id == "w_overcap_sdt_hs_cutoff_developing"]),
                                                                                        min = wid$min[wid$item_id == "w_overcap_sdt_hs_cutoff_developing"],
                                                                                        max = wid$max[wid$item_id == "w_overcap_sdt_hs_cutoff_developing"],
                                                                                        value = wid$value[wid$item_id == "w_overcap_sdt_hs_cutoff_developing"],
                                                                                        width = "100%")
                                                                            
                                                           ), # /conditionalpanel - High seas exception for developing
                                                           
                                                           # Conditional panel - Time delay for developing allowed
                                                           conditionalPanel("input.w_overcap_sdt_what_developing.includes('TIME')",
                                                                            
                                                                            # Input - Time delay for developing
                                                                            sliderInput("w_overcap_sdt_time_delay_developing",
                                                                                        label = tags$b(text$item_label[text$item_id == "w_overcap_sdt_time_delay_developing"]),
                                                                                        min = wid$min[wid$item_id == "w_overcap_sdt_time_delay_developing"],
                                                                                        max = wid$max[wid$item_id == "w_overcap_sdt_time_delay_developing"],
                                                                                        value = wid$value[wid$item_id == "w_overcap_sdt_time_delay_developing"],
                                                                                        width = "100%")
                                                                            
                                                                            
                                                           ) # /conditionalpanel - Time delay for developing allowed
                                          ), # /conditionalPanel - S&DT should be allowed for developing
                                          
                                          # Conditional panel - S&DT should be allowed for SVEs
                                          conditionalPanel("(input.w_overcap_allow_sdt == 'YES' & input.w_overcap_sdt_sve == 'YES')",
                                                           
                                                           # Input - SVE S&DT
                                                           checkboxGroupInput("w_overcap_sdt_what_sve",
                                                                              label = tags$b(text$item_label[text$item_id == "w_overcap_sdt_what_sve"]),
                                                                              choices = unlist(wid$choices[wid$item_id == "w_overcap_sdt_what_sve"]),
                                                                              selected = unlist(wid$selected[wid$item_id == "w_overcap_sdt_what_sve"]),
                                                                              width = "100%",
                                                                              inline = FALSE),
                                                           
                                                           # Conditional panel - High seas exception for sve
                                                           conditionalPanel("input.w_overcap_sdt_what_sve.includes('HS')",
                                                                            
                                                                            # Input - High seas cutoff for sve
                                                                            sliderInput("w_overcap_sdt_hs_cutoff_sve",
                                                                                        label = tags$b(text$item_label[text$item_id == "w_overcap_sdt_hs_cutoff_sve"]),
                                                                                        min = wid$min[wid$item_id == "w_overcap_sdt_hs_cutoff_sve"],
                                                                                        max = wid$max[wid$item_id == "w_overcap_sdt_hs_cutoff_sve"],
                                                                                        value = wid$value[wid$item_id == "w_overcap_sdt_hs_cutoff_sve"],
                                                                                        width = "100%")
                                                                            
                                                           ), # /conditionalpanel - High seas exception for sve
                                                           
                                                           # Conditional panel - Time delay for SVE allowed
                                                           conditionalPanel("input.w_overcap_sdt_what_sve.includes('TIME')",
                                                                            
                                                                            # Input - Time delay for SVE
                                                                            sliderInput("w_overcap_sdt_time_delay_sve",
                                                                                        label = tags$b(text$item_label[text$item_id == "w_overcap_sdt_time_delay_sve"]),
                                                                                        min = wid$min[wid$item_id == "w_overcap_sdt_time_delay_sve"],
                                                                                        max = wid$max[wid$item_id == "w_overcap_sdt_time_delay_sve"],
                                                                                        value = wid$value[wid$item_id == "w_overcap_sdt_time_delay_sve"],
                                                                                        width = "100%")
                                                                            
                                                                            
                                                           ) # /conditionalpanel - Time delay for SVE allowed
                                          ) # /conditionalPanel - S&DT should be allowed for SVE
                                   ) # /column 8
                                   
                  ) # / conditional - length of definitions > 0
                  
                ), # /fluidRow  
                
                ### ------------------------
                ### Forth Row: Cap/Tier
                ### ------------------------
                fluidRow(
                  
                  tags$hr(),
                  tags$h4("CAP & TIER: "),
                
                  column(12, align = "center",
                       
                       # Input - cap/tier on/off
                       radioButtons("w_cap_on_off",
                                    label = tags$b(text$item_label[text$item_id == "w_cap_on_off"]),
                                    choices = unlist(wid$choices[wid$item_id == "w_cap_on_off"]),
                                    selected = unlist(wid$selected[wid$item_id == "w_cap_on_off"]),
                                    width = "100%",
                                    inline = FALSE),
                       
                       # Conditional Panel - Cap is turned on
                       conditionalPanel("input.w_cap_on_off == 'YES'",
                                        
                                        # Input - subsidy types to include in cap
                                        checkboxGroupInput("w_cap_subsidy_types",
                                                           label = tags$b(text$item_label[text$item_id == "w_cap_subsidy_types"]),
                                                           choices = subsidy_types_sorted_sumaila[4:13],
                                                           selected = subsidy_types_sorted_sumaila[4:13],
                                                           width = "100%",
                                                           inline = TRUE)
                                        
                       ) # /conditionalPanel - Cap is turned on
                       
                  ),
                
                  # Conditional Panel - Cap is turned on and at least one subsidy type selected
                  conditionalPanel("input.w_cap_on_off == 'YES' && input.w_cap_subsidy_types.length > 0",
                                 
                                   ### Step 1: Tier structure ------------------
                                   column(12, id = "section-title-div-underline",
                                          
                                          # Section Title
                                          tags$h5(text$item_label[text$item_id == "tier-structure"])
                                          
                                   ),
                                   
                                   # Number of tiers
                                   column(12, id = "tb-spaced-div",
                                          
                                          radioButtons("w_cap_tier_number",
                                                       label = tags$b(text$item_label[text$item_id == "w_cap_tier_number"]),
                                                       choices = unlist(wid$choices[wid$item_id == "w_cap_tier_number"]),
                                                       selected = unlist(wid$selected[wid$item_id == "w_cap_tier_number"]),
                                                       width = "100%",
                                                       inline = FALSE)
                                          
                                   ), # /column 12
                                   
                                   conditionalPanel('input.w_cap_tier_number != "ONE"',
                                                    
                                                    column(12, id = "div-topline",
                                                           
                                                           fluidRow(
                                                             
                                                             # Left Column
                                                             column(6, style = "padding-right: 10px;",
                                                                    
                                                                    # Input - select system for tiering
                                                                    radioButtons("w_tier_system",
                                                                                 label = tags$b(text$item_label[text$item_id == "w_tier_system"]),
                                                                                 choices = unlist(wid$choices[wid$item_id == "w_tier_system"]),
                                                                                 selected = unlist(wid$selected[wid$item_id == "w_tier_system"]),
                                                                                 width = "100%",
                                                                                 inline = FALSE)
                                                                    
                                                             ),
                                                             
                                                             # Right column
                                                             column(6, style = "padding-left: 10px;",
                                                                    
                                                                    # Conditional Panel - Two tiers
                                                                    conditionalPanel('input.w_cap_tier_number == "TWO" && input.w_tier_system != "DEVELOPMENT"',
                                                                                     # Input - cutoff for top tier                 
                                                                                     sliderInput("w_two_tier_cutoff",
                                                                                                 label = tags$b(text$item_label[text$item_id == "w_two_tier_cutoff"]),
                                                                                                 min = wid$min[wid$item_id == "w_two_tier_cutoff"],
                                                                                                 max = wid$max[wid$item_id == "w_two_tier_cutoff"],
                                                                                                 step = wid$step[wid$item_id == "w_two_tier_cutoff"],
                                                                                                 value = wid$value[wid$item_id == "w_two_tier_cutoff"])
                                                                                     
                                                                    ), # /conditionalPanel - Two tiers
                                                                    
                                                                    # Conditional Panel - Three tiers
                                                                    conditionalPanel('input.w_cap_tier_number == "THREE" & input.w_tier_system != "DEVELOPMENT"',
                                                                                     
                                                                                     # Input - cutoffs for top and bottom tiers
                                                                                     sliderInput("w_three_tier_cutoff",
                                                                                                 
                                                                                                 label = tags$b(text$item_label[text$item_id == "w_three_tier_cutoff"]),
                                                                                                 min = wid$min[wid$item_id == "w_three_tier_cutoff"],
                                                                                                 max = wid$max[wid$item_id == "w_three_tier_cutoff"],
                                                                                                 step = wid$step[wid$item_id == "w_three_tier_cutoff"],
                                                                                                 value = c(wid$min[wid$item_id == "w_three_tier_cutoff"]/10, 
                                                                                                           wid$value[wid$item_id == "w_three_tier_cutoff"]))
                                                                                     
                                                                    ) # /conditionalPanel - Three tiers
                                                             ) # /column 6 - right column
                                                           ) # /fluidRow
                                                    ) # /column 12 - div-topline
                                   ), # /conditional panel - more than one tier
                                          
                                 ### Step 2: Subsidy caps
                                 column(12, id = "section-title-div-underline",
                                        
                                        # Section Title
                                        tags$h5(text$item_label[text$item_id == "caps"])
                                        
                                 ),
                                 
                                 column(12, id = "tb-spaced-div",
                                                         
                                        # Row 1 - Tier 1
                                        fluidRow(
                                                    
                                                 # Left column              
                                                 column(6, style = "padding-right: 10px;",
                                                                         
                                                        # Input - Tier 1 cap rule
                                                        radioButtons("w_tier1_cap_rule",
                                                                     label = tags$b(text$item_label[text$item_id == "w_tier1_cap_rule"]),
                                                                     choices = unlist(wid$choices[wid$item_id == "w_tier1_cap_rule"]),
                                                                     selected = unlist(wid$selected[wid$item_id == "w_tier1_cap_rule"]),
                                                                     width = "100%",
                                                                     inline = FALSE)
                                                                         
                                                 ), # /column 6 - left column
                                                       
                                                 # Right column           
                                                 column(6, style = "padding-left: 10px;",
                                                                         
                                                        # Conditional Panel - Tier 1 cap is set by value
                                                        conditionalPanel(condition = "input.w_tier1_cap_rule == 'VALUE'",
                                                                                          
                                                                         # Input - Set Tier 1 cap value
                                                                         sliderInput("w_tier1_cap_value",
                                                                                     
                                                                                     label = tags$b(text$item_label[text$item_id == "w_tier1_cap_value"]),
                                                                                     min = wid$min[wid$item_id == "w_tier1_cap_value"],
                                                                                     max = wid$max[wid$item_id == "w_tier1_cap_value"],
                                                                                     value = wid$value[wid$item_id == "w_tier1_cap_value"],
                                                                                     width = "100%")
                                                                         
                                                        ), # /conditionalPanel - Tier 1 cap is set by value
                                                                         
                                                        # # Conditional Panel - Tier 1 cap is set by per fisher value
                                                        # conditionalPanel(condition = "input.w_tier1_cap_rule == 'FISHERS'",
                                                        #                                   
                                                        #                  # Input - Set Tier 1 per fisher amount
                                                        #                  sliderInput("w_tier1_cap_fishers",
                                                        #                              label = tags$b(text$item_label[text$item_id == "w_tier1_cap_fishers"]),
                                                        #                              min = wid$min[wid$item_id == "w_tier1_cap_fishers"],
                                                        #                              max = wid$max[wid$item_id == "w_tier1_cap_fishers"],
                                                        #                              value = wid$value[wid$item_id == "w_tier1_cap_fishers"],
                                                        #                              width = "100%")
                                                        #                  
                                                        # ), # /conditionalPanel - Tier 1 cap is set by value
                                                        
                                                        # Conditional Panel - Tier 1 cap is set by BEST (percent subs, percent landed value, percent fishers calculation)
                                                        conditionalPanel(condition = "input.w_tier1_cap_rule == 'BEST'",
                                                                         
                                                                         # Input - Set Tier 1 percentage for best option - subsidies
                                                                         sliderInput("w_tier1_cap_best_percent_subs",
                                                                                     label = tags$b(text$item_label[text$item_id == "w_tier1_cap_best_percent_subs"]),
                                                                                     min = wid$min[wid$item_id == "w_tier1_cap_best_percent_subs"],
                                                                                     max = wid$max[wid$item_id == "w_tier1_cap_best_percent_subs"],
                                                                                     value = wid$value[wid$item_id == "w_tier1_cap_best_percent_subs"],
                                                                                     width = "100%"),
                                                                         
                                                                         # Input - Set Tier 1 percentage for best option - landed value
                                                                         sliderInput("w_tier1_cap_best_percent_landed_value",
                                                                                     label = tags$b(text$item_label[text$item_id == "w_tier1_cap_best_percent_landed_value"]),
                                                                                     min = wid$min[wid$item_id == "w_tier1_cap_best_percent_landed_value"],
                                                                                     max = wid$max[wid$item_id == "w_tier1_cap_best_percent_landed_value"],
                                                                                     value = wid$value[wid$item_id == "w_tier1_cap_best_percent_landed_value"],
                                                                                     width = "100%"),
                                                                         
                                                                         # Input - Set Tier 1 percentage for best option - fishers
                                                                         sliderInput("w_tier1_cap_best_percent_fishers",
                                                                                     label = tags$b(text$item_label[text$item_id == "w_tier1_cap_best_percent_fishers"]),
                                                                                     min = wid$min[wid$item_id == "w_tier1_cap_best_percent_fishers"],
                                                                                     max = wid$max[wid$item_id == "w_tier1_cap_best_percent_fishers"],
                                                                                     value = wid$value[wid$item_id == "w_tier1_cap_best_percent_fishers"],
                                                                                     width = "100%")
                                                                         
                                                        ), # /conditionalPanel - Tier 1 cap is set by best option
                                                                         
                                                        # Conditional Panel - Tier 1 cap is set by percentage
                                                        conditionalPanel(condition = "input.w_tier1_cap_rule != 'VALUE' && input.w_tier1_cap_rule != 'BEST'",
                                                                                          
                                                                         # Input - Set Tier 1 percentage
                                                                         sliderInput("w_tier1_cap_percent",
                                                                                     label = tags$b(text$item_label[text$item_id == "w_tier1_cap_percent"]),
                                                                                     min = wid$min[wid$item_id == "w_tier1_cap_percent"],
                                                                                     max = wid$max[wid$item_id == "w_tier1_cap_percent"],
                                                                                     value = wid$value[wid$item_id == "w_tier1_cap_percent"],
                                                                                     width = "100%")
                                                                         
                                                        ) # /conditionalPanel - Tier 1 cap is set by percentage
                                                        
                                                 ) # /column 6 - Right column
                                                 
                                        ), # /fluidRow - Tier 1

                                        # Conditional Panel - More than one tier
                                        conditionalPanel("input.w_cap_tier_number !== 'ONE'",
                                                         
                                                         column(12, id = "div-topline",
                                                         
                                                         # Row 1 - Tier 2
                                                         fluidRow(
                                                                  
                                                                  # Left column              
                                                                  column(6, style = "padding-right: 10px;",
                                                                         
                                                                         # Input - Tier 2 cap rule
                                                                         radioButtons("w_tier2_cap_rule",
                                                                                      label = tags$b(text$item_label[text$item_id == "w_tier2_cap_rule"]),
                                                                                      choices = unlist(wid$choices[wid$item_id == "w_tier2_cap_rule"]),
                                                                                      selected = unlist(wid$selected[wid$item_id == "w_tier2_cap_rule"]),
                                                                                      width = "100%",
                                                                                      inline = FALSE)
                                                                         
                                                                  ), # /column 6 - left column
                                                                  
                                                                  # Right column           
                                                                  column(6, style = "padding-left: 10px;",
                                                                         
                                                                         # Conditional Panel - Tier 2 cap is set by value
                                                                         conditionalPanel(condition = "input.w_tier2_cap_rule == 'VALUE'",
                                                                                          
                                                                                          # Input - Set Tier 2 cap value
                                                                                          sliderInput("w_tier2_cap_value",
                                                                                                      
                                                                                                      label = tags$b(text$item_label[text$item_id == "w_tier2_cap_value"]),
                                                                                                      min = wid$min[wid$item_id == "w_tier2_cap_value"],
                                                                                                      max = wid$max[wid$item_id == "w_tier2_cap_value"],
                                                                                                      value = wid$value[wid$item_id == "w_tier2_cap_value"],
                                                                                                      width = "100%")
                                                                                          
                                                                         ), # /conditionalPanel - Tier 2 cap is set by value
                                                                         
                                                                         # # Conditional Panel - Tier 2 cap is set by per fisher value
                                                                         # conditionalPanel(condition = "input.w_tier2_cap_rule == 'FISHERS'",
                                                                         #                  
                                                                         #                  # Input - Set Tier 2 per fisher amount
                                                                         #                  sliderInput("w_tier2_cap_fishers",
                                                                         #                              label = tags$b(text$item_label[text$item_id == "w_tier2_cap_fishers"]),
                                                                         #                              min = wid$min[wid$item_id == "w_tier2_cap_fishers"],
                                                                         #                              max = wid$max[wid$item_id == "w_tier2_cap_fishers"],
                                                                         #                              value = wid$value[wid$item_id == "w_tier2_cap_fishers"],
                                                                         #                              width = "100%")
                                                                         #                  
                                                                         # ), # /conditionalPanel - Tier 2 cap is set by value
                                                                         
                                                                         # Conditional Panel - Tier 2 cap is set by BEST (percent subs, percent landed value, percent fishers calculation)
                                                                         conditionalPanel(condition = "input.w_tier2_cap_rule == 'BEST'",
                                                                                          
                                                                                          # Input - Set Tier 2 percentage for best option - subsidies
                                                                                          sliderInput("w_tier2_cap_best_percent_subs",
                                                                                                      label = tags$b(text$item_label[text$item_id == "w_tier2_cap_best_percent_subs"]),
                                                                                                      min = wid$min[wid$item_id == "w_tier2_cap_best_percent_subs"],
                                                                                                      max = wid$max[wid$item_id == "w_tier2_cap_best_percent_subs"],
                                                                                                      value = wid$value[wid$item_id == "w_tier2_cap_best_percent_subs"],
                                                                                                      width = "100%"),
                                                                                          
                                                                                          # Input - Set Tier 2 percentage for best option - landed value
                                                                                          sliderInput("w_tier2_cap_best_percent_landed_value",
                                                                                                      label = tags$b(text$item_label[text$item_id == "w_tier2_cap_best_percent_landed_value"]),
                                                                                                      min = wid$min[wid$item_id == "w_tier2_cap_best_percent_landed_value"],
                                                                                                      max = wid$max[wid$item_id == "w_tier2_cap_best_percent_landed_value"],
                                                                                                      value = wid$value[wid$item_id == "w_tier2_cap_best_percent_landed_value"],
                                                                                                      width = "100%"),
                                                                                          
                                                                                          # Input - Set Tier 2 percentage for best option - fishers
                                                                                          sliderInput("w_tier2_cap_best_percent_fishers",
                                                                                                      label = tags$b(text$item_label[text$item_id == "w_tier2_cap_best_percent_fishers"]),
                                                                                                      min = wid$min[wid$item_id == "w_tier2_cap_best_percent_fishers"],
                                                                                                      max = wid$max[wid$item_id == "w_tier2_cap_best_percent_fishers"],
                                                                                                      value = wid$value[wid$item_id == "w_tier2_cap_best_percent_fishers"],
                                                                                                      width = "100%")
                                                                                          
                                                                         ), # /conditionalPanel - Tier 2 cap is set by best option
                                                                         
                                                                         # Conditional Panel - Tier 2 cap is set by percentage
                                                                         conditionalPanel(condition = "(input.w_tier2_cap_rule != 'VALUE') && (input.w_tier2_cap_rule != 'BEST') && (input.w_tier2_cap_rule != 'NONE')",
                                                                                          
                                                                                          # Input - Set Tier 2 percentage
                                                                                          sliderInput("w_tier2_cap_percent",
                                                                                                      label = tags$b(text$item_label[text$item_id == "w_tier2_cap_percent"]),
                                                                                                      min = wid$min[wid$item_id == "w_tier2_cap_percent"],
                                                                                                      max = wid$max[wid$item_id == "w_tier2_cap_percent"],
                                                                                                      value = wid$value[wid$item_id == "w_tier2_cap_percent"],
                                                                                                      width = "100%")
                                                                                          
                                                                         ) # /conditionalPanel - Tier 2 cap is set by percent
                                                                         
                                                                  ) # /column 6 - Right column
                                                                  
                                                         ) # /fluidRow - Tier 2
                                                         
                                                         ) # /column 12 - div-topline
                                                         
                                        ), # close tier 2 conditional
                                                         
                                        # Conditional Panel - Three tiers
                                        conditionalPanel("input.w_cap_tier_number == 'THREE'",
                                                         
                                                         column(12, id = "div-topline",
                                                                          
                                                         # Row 3 - Tier 3
                                                         fluidRow(
                                                                  
                                                                  # Left column              
                                                                  column(6, style = "padding-right: 10px;",
                                                                         
                                                                         # Input - Tier 3 cap rule
                                                                         radioButtons("w_tier3_cap_rule",
                                                                                      label = tags$b(text$item_label[text$item_id == "w_tier3_cap_rule"]),
                                                                                      choices = unlist(wid$choices[wid$item_id == "w_tier3_cap_rule"]),
                                                                                      selected = unlist(wid$selected[wid$item_id == "w_tier3_cap_rule"]),
                                                                                      width = "100%",
                                                                                      inline = FALSE)
                                                                         
                                                                  ), # /column 6 - left column
                                                                  
                                                                  # Right column           
                                                                  column(6, style = "padding-left: 10px;",
                                                                         
                                                                         # Conditional Panel - Tier 3 cap is set by value
                                                                         conditionalPanel(condition = "input.w_tier3_cap_rule == 'VALUE'",
                                                                                          
                                                                                          # Input - Set Tier 3 cap value
                                                                                          sliderInput("w_tier3_cap_value",
                                                                                                      
                                                                                                      label = tags$b(text$item_label[text$item_id == "w_tier3_cap_value"]),
                                                                                                      min = wid$min[wid$item_id == "w_tier3_cap_value"],
                                                                                                      max = wid$max[wid$item_id == "w_tier3_cap_value"],
                                                                                                      value = wid$value[wid$item_id == "w_tier3_cap_value"],
                                                                                                      width = "100%")
                                                                                          
                                                                         ), # /conditionalPanel - Tier 3 cap is set by value
                                                                         
                                                                         # # Conditional Panel - Tier 3 cap is set by per fisher value
                                                                         # conditionalPanel(condition = "input.w_tier3_cap_rule == 'FISHERS'",
                                                                         #                  
                                                                         #                  # Input - Set Tier 3 per fisher amount
                                                                         #                  sliderInput("w_tier3_cap_fishers",
                                                                         #                              label = tags$b(text$item_label[text$item_id == "w_tier3_cap_fishers"]),
                                                                         #                              min = wid$min[wid$item_id == "w_tier3_cap_fishers"],
                                                                         #                              max = wid$max[wid$item_id == "w_tier3_cap_fishers"],
                                                                         #                              value = wid$value[wid$item_id == "w_tier3_cap_fishers"],
                                                                         #                              width = "100%")
                                                                         #                  
                                                                         # ), # /conditionalPanel - Tier 3 cap is set by value
                                                                         
                                                                         # Conditional Panel - Tier 3 cap is set by BEST (percent subs, percent landed value, percent fishers calculation)
                                                                         conditionalPanel(condition = "input.w_tier3_cap_rule == 'BEST'",
                                                                                          
                                                                                          # Input - Set Tier 3 percentage for best option - subsidies
                                                                                          sliderInput("w_tier3_cap_best_percent_subs",
                                                                                                      label = tags$b(text$item_label[text$item_id == "w_tier3_cap_best_percent_subs"]),
                                                                                                      min = wid$min[wid$item_id == "w_tier3_cap_best_percent_subs"],
                                                                                                      max = wid$max[wid$item_id == "w_tier3_cap_best_percent_subs"],
                                                                                                      value = wid$value[wid$item_id == "w_tier3_cap_best_percent_subs"],
                                                                                                      width = "100%"),
                                                                                          
                                                                                          # Input - Set Tier 3 percentage for best option - landed value
                                                                                          sliderInput("w_tier3_cap_best_percent_landed_value",
                                                                                                      label = tags$b(text$item_label[text$item_id == "w_tier3_cap_best_percent_landed_value"]),
                                                                                                      min = wid$min[wid$item_id == "w_tier3_cap_best_percent_landed_value"],
                                                                                                      max = wid$max[wid$item_id == "w_tier3_cap_best_percent_landed_value"],
                                                                                                      value = wid$value[wid$item_id == "w_tier3_cap_best_percent_landed_value"],
                                                                                                      width = "100%"),
                                                                                          
                                                                                          # Input - Set Tier 3 percentage for best option - fishers
                                                                                          sliderInput("w_tier3_cap_best_percent_fishers",
                                                                                                      label = tags$b(text$item_label[text$item_id == "w_tier3_cap_best_percent_fishers"]),
                                                                                                      min = wid$min[wid$item_id == "w_tier3_cap_best_percent_fishers"],
                                                                                                      max = wid$max[wid$item_id == "w_tier3_cap_best_percent_fishers"],
                                                                                                      value = wid$value[wid$item_id == "w_tier3_cap_best_percent_fishers"],
                                                                                                      width = "100%")
                                                                                          
                                                                         ), # /conditionalPanel - Tier 3 cap is set by best option
                                                                         
                                                                         # Conditional Panel - Tier 3 cap is set by percentage
                                                                         conditionalPanel(condition = "(input.w_tier3_cap_rule != 'VALUE') && (input.w_tier3_cap_rule != 'BEST') && (input.w_tier3_cap_rule != 'NONE')",
                                                                                          
                                                                                          # Input - Set Tier 3 percentage
                                                                                          sliderInput("w_tier3_cap_percent",
                                                                                                      label = tags$b(text$item_label[text$item_id == "w_tier3_cap_percent"]),
                                                                                                      min = wid$min[wid$item_id == "w_tier3_cap_percent"],
                                                                                                      max = wid$max[wid$item_id == "w_tier3_cap_percent"],
                                                                                                      value = wid$value[wid$item_id == "w_tier3_cap_percent"],
                                                                                                      width = "100%")
                                                                                          
                                                                         ) # /conditionalPanel - Tier 3 cap is set by percent
                                                                         
                                                                  ) # /column 6 - Right column
                                                                  
                                                         ) # /fluidRow - Tier 3
                                                         ) # /column 12 - div-topline
                                                         
                                        ) # close tier 3 conditional
                                                         
                                 ) # /column 12 - Subsidy caps
                                                  
                  ) # /conditionalPanel - Cap is on and at leaast one subsidy type selected
                
                ) # /fluidRow - forth row
                                 
         ), # /column 12 - Manual Overcapacity discipline selection
         
         # Bottom navigation buttons
         fluidRow(
           
           # Previous tab
           column(3, id = "spaced-div",
                  
                  tags$button(id = "ab_edit_policies_tabs_overcap_to_oa",
                              class = "btn action-button rounded-button-grey",
                              text$item_label[text$item_id == "ab_edit_policies_tabs_overcap_to_oa"]
                  )
                  
           )
           
         ) # /fluidRow - Bottom navigation buttons
         
  ) # /column - container for Overcapacity tab box
