### -------------------------------------------------------------------
### SubsidyExplorer
### An interactive toolkit to explore the tradeoffs across proposed fisheries subsidies disciplines
### 
### This script contains the content for the "edit-policies" tab
### --------------------------------------------------------------------

EditPolicies = function() 
  fluidPage(
    
    # Page style
    style = "background-color: #ffffff; color: #000000;",
    
    ### Custom formatting for tabs
    tags$style(
      "
      .nav-tabs {
      background: #323337;
      }
      
      .nav-tabs-custom .nav-tabs {
      border-bottom-color: transparent;
      }
      
      .nav-tabs-custom .nav-tabs li a {
      color: #ffffff;
      }
      
      .nav-tabs-custom .nav-tabs li:hover {
      background: #28292C;
      color: #ffffff;
      }
      
      .nav-tabs-custom .nav-tabs li.active:hover a {
      color: #ffffff;
      background: #28292C;
      }
      
      .nav-tabs-custom .nav-tabs li.active {
      border-top-color: #3c8dbc;
      }
      
      .nav-tabs-custom .nav-tabs li.active a {
      background: #28292C;
      color: #ffffff;
      border-left-color: transparent;
      border-right-color: transparent;
      }"

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
    
  ####-----------------------------------------------------------------------------------------
  #### Left column - tabBox with select policies
  #### ----------------------------------------------------------------------------------------
column(12,
       
  column(9,
    
    ### Title and introductory text 
    column(12, style = "padding: 25px 25px;",
           
           # Title
           tags$h3(style = "text-align: left; padding: 0; margin: 0 0 10px;", text$item_label[text$item_id == "edit-policies"])
           
    ),
    
    ### Content
    column(12, style = "padding: 0 0 25px;",

           column(12, style = "padding: 0 10px;",
                  
                  tabBox(width = 12, id = "policy_tabs", 
                         
                         ### --------------------------
                         ### Tab # 0  - Introduction
                         ### --------------------------
                         
                         tabPanel(text$item_label[text$item_id == "instructions"],
                                  value = "instructions",
                                  
                                  # Column container for tab panel        
                                  column(12, style = "border-style: solid;
                                         border-width: 2px 1px 1px 1px;
                                         border-color: #28292C;",
                                         
                                         # Intro text and policy name
                                         column(12, style = "padding: 15px 25px 15px;",
                                                
                                                # Introductory text
                                                includeHTML("./text/02b_edit_policies_intro.html"),
                                                
                                                # Provide policy description
                                                textInput("w_run_name",
                                                          label = text$item_label[text$item_id == "w_run_name"],
                                                          value = text$value[text$item_id == "w_run_name"]),
                                         ),
                                         
                                         # Next tab button
                                         column(3, offset = 9, style = "padding: 5px;",
                                                
                                                tags$button(id = "ab_edit_policies_tabs_instructions_to_iuu",
                                                            class = "btn action-button nav-button-white-r",
                                                            text$item_label[text$item_id == "ab_edit_policies_tabs_instructions_to_iuu"], icon("chevron-right")
                                                )
                                         )
                                         
                                  ) # /column 12 - column container for instructions tabPanel
                                  
                         ), # /tabPanel - instructions
                        
                         
                         ### --------------------------
                         ### Tab # 1  - IUU 
                         ### --------------------------
                         
                         tabPanel(text$item_label[text$item_id == "iuu"], 
                                  value = "iuu",
                                  
                                  # Column container for tab panel        
                                  column(12, style = "border-style: solid;
                                         border-width: 2px 1px 1px 1px;
                                         border-color: #28292C;",
                                         
                                         # IUU discipline text
                                         column(12, style = "padding: 15px 25px 15px;",
                                                
                                                includeHTML("./text/02b_edit_policies_iuu_intro.html")
                                                
                                         ),
                                         
                                         # # IUU proposal dropdown
                                         # column(12, style = "padding: 0px 25px 15px;",
                                         #        
                                         #        # Dropdown widget
                                         #        column(12, style = "padding: 0px 10px;",
                                         #               
                                         #               selectInput("w_iuu_proposal_selection", 
                                         #                           label = tags$b(widget_text$label[widget_text$id == "w_iuu_proposal_selection"]),
                                         #                           choices = c("Default", "A", "B"),
                                         #                           selected = "Default",
                                         #                           width = "100%")
                                         #        )
                                         #        
                                         # ), # /column - IUU proposal dropdown
                                         
                                         # Manual IUU discipline selection
                                         column(12, style = "padding: 0px 25px 15px;", 
                                                
                                                    fluidRow( 
                                                      
                                                      ### Left column: Definitions
                                                      column(5, style = "padding: 0 10px;",
                                                         
                                                             # Input: IUU definitions
                                                             checkboxGroupInput("w_iuu_definitions", 
                                                                                
                                                                                label = tagList(tags$b(text$item_label[text$item_id == "w_iuu_definitions"]),
                                                                                                # IUU definitions info button
                                                                                                tags$button(id = "info_iuu",
                                                                                                            class = "btn action-button info-button",
                                                                                                            icon("info"))), 
                                                                            
                                                                                choices = c("iuu1", "iuu2", "iuu3", "iuu4"),
                                                                                selected = c(""),
                                                                                width = "100%",
                                                                                inline = FALSE), 
                                                             
                                                             # IUU data warning
                                                             tags$i(textOutput("iuu_warning")),
                                                         
                                                         br(),
                                                         
                                                         # Conditional panel: IUU assumption
                                                         conditionalPanel(condition = "input.w_iuu_definitions.includes('iuu2') | input.w_iuu_definitions.includes('iuu3') | input.w_iuu_definitions.includes('iuu4')",
                                                                          
                                                                          # Input: Make IUU assumption
                                                                          radioButtons("w_iuu_assumption",
                                                                                       label = tagList(tags$b(text$item_label[text$item_id == "w_iuu_assumption"]),
                                                                                                       # IUU assumption info button
                                                                                                       tags$button(id = "info_iuu_assumption",
                                                                                                                   class = "btn action-button info-button",
                                                                                                                   icon("info"))), 
                                                                                       choices = c("Yes", "No"),
                                                                                       selected = "No",
                                                                                       width = "100%",
                                                                                       inline = FALSE), 
                                                                          
                                                                          
                                                                          # Conditional panel: IUU assumption value
                                                                          conditionalPanel(condition = "input.w_iuu_assumption == 'Yes'",
                                                                                           
                                                                                           # Input: Assumed level of IUU fishing
                                                                                           sliderInput("w_iuu_percent", 
                                                                                                       label = tags$b(text$item_label[text$item_id == "w_iuu_percent"]),
                                                                                                       min = text$min[text$item_id == "w_iuu_percent"],
                                                                                                       max = text$max[text$item_id == "w_iuu_percent"],
                                                                                                       value = text$value[text$item_id == "w_iuu_percent"],
                                                                                                       width = "100%")
                                                                                           
                                                                          ) # /conditionalPanel: IUU assumption value
                                                         ) # /conditionalPanel: IUU assumption
                                                         
                                                  ), # /column: Left column 
                                                  
                                                  
                                                  ### Middle column: scope
                                                  column(4, style = "padding: 0 10px;",
                                                         
                                                         conditionalPanel('input.w_iuu_definitions.length > 0',
                                                                          
                                                                          radioButtons("w_iuu_scope",
                                                                                       label = tags$b(text$item_label[text$item_id == "w_iuu_scope"]),
                                                                                       choices = c("all", "other"),
                                                                                       selected = "all",
                                                                                       width = "100%",
                                                                                       inline = FALSE),
                                                                          
                                                                          # Manual selection of countries
                                                                          conditionalPanel("input.w_iuu_scope == 'select'",
                                                                                           
                                                                                           selectizeInput("w_iuu_scope_manual",
                                                                                                          label = tags$b(text$item_label[text$item_id == "w_iuu_scope_manual"]),
                                                                                                          choices = c("A", "B", "C"),
                                                                                                          selected = NULL,
                                                                                                          width = "100%",
                                                                                                          options = list(placeholder = 'Select...'),
                                                                                                          multiple = T)
                                                                                           
                                                                          ) # close iuu scope manual conditional
                                                         ) # close iuu scope conditional
                                                  ), # close iuu scope column
                                                  
                                                  ### S&DT: right column
                                                  column(3, style = "padding: 0 10px;",
                                                         conditionalPanel('input.w_iuu_definitions.length > 0',
                                                                          
                                                                          radioButtons("w_iuu_allow_sdt",
                                                                                       label = tags$b(text$item_label[text$item_id == "w_iuu_allow_sdt"]),
                                                                                       choices = c("Yes", "No"),
                                                                                       selected = "No",
                                                                                       width = "100%",
                                                                                       inline = FALSE)
                                                                          
                                                         ), # /conditionalPanel - input.w_iuu_definitions.length > 0
                                                         
                                                         conditionalPanel("(input.w_iuu_allow_sdt == 'Yes' && input.w_iuu_definitions.length > 0)",
                                                                          
                                                                          # S&DT - Who?
                                                                          radioButtons("w_iuu_sdt_who",
                                                                                       label = tagList(tags$b(text$item_label[text$item_id == "w_iuu_sdt_who"]),
                                                                                                       # Info button
                                                                                                       tags$button(id = "info_iuu_sdt",
                                                                                                                   class = "btn action-button info-button",
                                                                                                                   icon("info"))),
                                                                                       choices = c("A", "B", "ldc"),
                                                                                       selected = "ldc",
                                                                                       width = "100%",
                                                                                       inline = FALSE),
                                                                          
                                                                          # S&DT - What? 
                                                                          checkboxGroupInput("w_iuu_sdt_what",
                                                                                             label = tags$b(text$item_label[text$item_id == "w_iuu_sdt_what"]),
                                                                                             choices = c("all", "domestic", "time"),
                                                                                             selected = "",
                                                                                             width = "100%",
                                                                                             inline = FALSE),
                                                                          
                                                                          # S&DT - Time delay if relevant
                                                                          conditionalPanel("input.w_iuu_sdt_what.includes('time')",

                                                                                           sliderInput("w_iuu_sdt_time_delay",
                                                                                                       label = tags$b(text$item_label[text$item_id == "w_iuu_sdt_time_delay"]),
                                                                                                       min = 0,
                                                                                                       max = 5,
                                                                                                       value = 1,
                                                                                                       width = "100%")

                                                                          ), # /conditionalpanel - input.w_iuu_sdt_what.includes('time')
                                                                          
                                                                          tags$hr(),
                                                                          
                                                                          # S&DT - Specify second S&DT?
                                                                          radioButtons("w_iuu_allow_sdt_second",
                                                                                       label = tags$b(text$item_label[text$item_id == "w_iuu_allow_sdt_second"]),
                                                                                       choices = c("Yes", "No"),
                                                                                       selected = "No",
                                                                                       width = "100%",
                                                                                       inline = FALSE),
                                                                          
                                                                          conditionalPanel("(input.w_iuu_allow_sdt == 'Yes' && input.w_iuu_definitions.length > 0 && input.w_iuu_allow_sdt_second == 'Yes')",
                                                                                           # S&DT - Who for second?
                                                                                           radioButtons("w_iuu_sdt_who_second",
                                                                                                        label = tags$b(text$item_label[text$item_id == "w_iuu_sdt_who_second"]),
                                                                                                        choices = c("A", "B", "ldc"),
                                                                                                        selected = "ldc",
                                                                                                        width = "100%",
                                                                                                        inline = FALSE),
                                                                                           
                                                                                           # S&DT - What for second?
                                                                                           checkboxGroupInput("w_iuu_sdt_what_second",
                                                                                                              label = tags$b(text$item_label[text$item_id == "w_iuu_sdt_what_second"]),
                                                                                                              choices = c("all", "domestic", "time"),
                                                                                                              selected = "",
                                                                                                              width = "100%",
                                                                                                              inline = FALSE),
                                                                                           
                                                                                           # S&DT - Time delay for second if relevant
                                                                                           conditionalPanel("input.w_iuu_sdt_what_second.includes('time')",

                                                                                                            sliderInput("w_iuu_sdt_time_delay_second",
                                                                                                                        label = tags$b(text$item_label[text$item_id == "w_iuu_sdt_time_delay_second"]),
                                                                                                                        min = 0,
                                                                                                                        max = 5,
                                                                                                                        value = 1,
                                                                                                                        width = "100%")

                                                                                           ) # /conditionalpanel - input.w_iuu_sdt_what_second.includes('time')
                                                                                           
                                                                          ) # /conditionalPanel - (input.w_iuu_allow_sdt == 'Yes' && input.w_iuu_definitions.length > 0 && input.w_iuu_allow_sdt_second == 'Yes')
                                                                          
                                                         ) # /conditionalpanel - input.w_iuu_allow_sdt == 'Yes' && input.w_iuu_definitions.length > 0
                                                  ) # /column - IUU S&DT
                                                  
                                                ) # /fluidRow - IUU
                                                
                                         ), # /column 12 - IUU manual options
                                         
                                         # # Collapsable box with more info
                                         # column(12, style = "padding: 0px 25px 15px; align: 'center';",
                                         #        
                                         #        box(title = "More information on IUU disciplines...",
                                         #            collapsible = TRUE,
                                         #            width = "100%",
                                         #            collapsed = T,
                                         #            
                                         #            # Text with links
                                         #            column(12, style = "padding: 0px 10px;",
                                         #                   
                                         #                   includeHTML("./text/06_iuu_more_info.html")
                                         #            )
                                         #        )
                                         # )
                                         
                                         # Previous and next tab buttons
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
                                                              text$item_label[text$item_id == "ab_edit_policies_tabs_iuu_to_oa"], icon("chevron-right"), 
                                                  )
                                                  
                                           )
                                           
                                         ) # /fluidRow
                                         
                                  ) # /column - container for IUU tab box
                                  
                         ), # /tabPanel #0
                         
                         ### --------------------------
                         ### Tab # 1  - Overfished stock disciplines
                         ### --------------------------
                         
                         tabPanel(text$item_label[text$item_id == "oa"], 
                                  value = "oa",
                                  
                                  # Column container for tab panel        
                                  column(12, style = "border-style: solid;
                                         border-width: 2px 1px 1px 1px;
                                         border-color: #28292C;",
                                         
                                         # OA discipline text
                                         column(12, style = "padding: 15px 25px 15px;",
                                                
                                                includeHTML("./text/02b_edit_policies_oa_intro.html")
                                                
                                         ),
                                         
                                         # # OA proposal dropdown
                                         # column(12, style = "padding: 0px 25px 15px;",
                                         #        
                                         #        # Section header
                                         #        h4("Prefill disciplines from a proposal"),
                                         #        
                                         #        # Dropdown widget
                                         #        column(12, style = "padding: 0px 10px;",
                                         #               
                                         #               selectInput("w_oa_proposal_selection", 
                                         #                           label = tagList(tags$b("Select a proposal...")),
                                         #                           choices = c("A", "B", "C", "Default"),
                                         #                           selected = "Default",
                                         #                           width = "100%")
                                         #               
                                         #        )
                                         #        
                                         # ),
                                         
                                         # Manual OA discipline selection
                                         column(12, style = "padding: 0px 25px 15px;", 
                                                
                                                fluidRow(
                                                  ### OA definitions: left column
                                                  column(5, style = "padding: 0 10px;",
                                                         
                                                         checkboxGroupInput("w_oa_definitions",
                                                                            label = tagList(tags$b(text$item_label[text$item_id == "w_oa_definitions"]),
                                                                                            # Info button
                                                                                            tags$button(id = "info_oa",
                                                                                                        class = "btn action-button info-button",
                                                                                                        icon("info"))),
                                                                            choices = c("oa1", "oa2"),
                                                                            selected = c(""),
                                                                            width = "100%",
                                                                            inline = FALSE)
                                                         
                                                  ), # /column - OA definitions
                                                  
                                                  ### OA scope: middle column
                                                  column(4, style = "padding: 0 10px;",
                                                         
                                                         conditionalPanel('input.w_oa_definitions.length > 0',
                                                                          
                                                                          radioButtons("w_oa_scope",
                                                                                       label = tags$b(text$item_label[text$item_id == "w_oa_scope"]),
                                                                                       choices = c("all", "select", "HS", "DW", "OUT", "LENGTH", "TONNAGE", "ENGINE", "LTE"),
                                                                                       selected = "all",
                                                                                       width = "100%",
                                                                                       inline = FALSE), 
                                                                          
                                                                          conditionalPanel("input.w_oa_scope == 'select'",
                                                                                           
                                                                                           selectizeInput("w_oa_scope_manual",
                                                                                                          label = tags$b(text$item_label[text$item_id == "w_oa_scope_manual"]),
                                                                                                          choices = c("A", "B", "C"),
                                                                                                          selected = NULL,
                                                                                                          width = "100%",
                                                                                                          options = list(placeholder = 'Select...'), 
                                                                                                          multiple = T)
                                                                                           
                                                                          ) # /conditionalPanel - input.w_oa_scope == 'select'
                                                         ), # /conditionalPanel - input.w_oa_definitions.length > 0
                                                         
                                                         conditionalPanel('(input.w_oa_definitions.length > 0 && (input.w_oa_scope == "HS" || input.w_oa_scope == "OUT"))',
                                                                          
                                                                          sliderInput("w_oa_hs_cutoff",
                                                                                      label = tagList(tags$b(text$item_label[text$item_id == "w_oa_hs_cutoff"]),
                                                                                                      # Info button
                                                                                                      tags$button(id = "info_oa_hs",
                                                                                                                  class = "btn action-button info-button",
                                                                                                                  icon("info"))),
                                                                                      min = 1,
                                                                                      max = 100,
                                                                                      value = 5,
                                                                                      width = "100%")
                                                                          
                                                         ), # /conditionalPanel - input.w_oa_definitions.length > 0 && input.w_oa_scope == "HS"
                                                         
                                                         conditionalPanel('(input.w_oa_definitions.length > 0 && (input.w_oa_scope == "LENGTH" || input.w_oa_scope == "LTE"))',
                                                                          
                                                                          sliderInput("w_oa_length_cutoff",
                                                                                      label = tags$b(text$item_label[text$item_id == "w_oa_length_cutoff"]),
                                                                                      min = 10,
                                                                                      max = 100,
                                                                                      value = 24,
                                                                                      width = "100%")
                                                                          
                                                         ), # /conditionalPanel - input.w_oa_definitions.length > 0 && input.w_oa_scope == "LENGTH"
                                                         
                                                         conditionalPanel('(input.w_oa_definitions.length > 0 && (input.w_oa_scope == "TONNAGE" || input.w_oa_scope == "LTE"))',
                                                                          
                                                                          sliderInput("w_oa_tonnage_cutoff",
                                                                                      label = tags$b(text$item_label[text$item_id == "w_oa_tonnage_cutoff"]),
                                                                                      min = 10,
                                                                                      max = 100,
                                                                                      value = 24,
                                                                                      width = "100%")
                                                                          
                                                         ), # /conditionalPanel - input.w_oa_definitions.length > 0 && input.w_oa_scope == "TONNAGE"
                                                         
                                                         conditionalPanel('(input.w_oa_definitions.length > 0 && (input.w_oa_scope == "ENGINE" || input.w_oa_scope == "LTE"))',
                                                                          
                                                                          sliderInput("w_oa_engine_cutoff",
                                                                                      label = tags$b(text$item_label[text$item_id == "w_oa_engine_cutoff"]),
                                                                                      min = 10,
                                                                                      max = 100,
                                                                                      value = 24,
                                                                                      width = "100%")
                                                                          
                                                         ) # /conditionalPanel - input.w_oa_definitions.length > 0 && input.w_oa_scope == "ENGINE"
                                                         
                                                  ), # /column - OA scope
                                                  
                                                  ### OA S&DT: right column
                                                  column(3, style = "padding: 0 10px;",
                                                         
                                                         # S&DT - Allow?
                                                         conditionalPanel('input.w_oa_definitions.length > 0',
                                                                          
                                                                          radioButtons("w_oa_allow_sdt",
                                                                                       label = tags$b(text$item_label[text$item_id == "w_oa_allow_sdt"]),
                                                                                       choices = c("Yes", "No"),
                                                                                       selected = "No",
                                                                                       width = "100%",
                                                                                       inline = FALSE)
                                                                          
                                                         ), # /conditionalPanel - input.w_oa_definitions.length > 0
                                                         
                                                         conditionalPanel("(input.w_oa_allow_sdt == 'Yes' && input.w_oa_definitions.length > 0)",
                                                                          
                                                                          # S&DT - Who recieves it
                                                                          radioButtons("w_oa_sdt_who",
                                                                                       label = tagList(tags$b(text$item_label[text$item_id == "w_oa_sdt_who"]),
                                                                                                       # Info button
                                                                                                       tags$button(id = "info_oa_sdt",
                                                                                                                   class = "btn action-button info-button",
                                                                                                                   icon("info"))), 
                                                                                       choices = c("ldc", "B"),
                                                                                       selected = "ldc",
                                                                                       width = "100%",
                                                                                       inline = FALSE), 
                                                                          
                                                                          # S&DT - What is it
                                                                          checkboxGroupInput("w_oa_sdt_what",
                                                                                             label = tags$b(text$item_label[text$item_id == "w_oa_sdt_what"]),
                                                                                             choices = c("all", "domestic", "HS", "time"),
                                                                                             selected = "all",
                                                                                             width = "100%",
                                                                                             inline = FALSE), 
                                                                          
                                                                          # S&DT - Define "high seas" fishing if relevant
                                                                          conditionalPanel('input.w_oa_sdt_what.includes("HS")',
                                                                                           
                                                                                           sliderInput("w_oa_sdt_hs_cutoff",
                                                                                                       label = tags$b(text$item_label[text$item_id == "w_oa_sdt_hs_cutoff"]),
                                                                                                       min = 1,
                                                                                                       max = 100,
                                                                                                       value = 5,
                                                                                                       width = "100%")
                                                                                           
                                                                          ), # /conditionalPanel - input.oa_sdt_what.includes("HS")
                                                                          
                                                                          # S&DT - Time delay if relevant
                                                                          conditionalPanel("input.oa_sdt_what.includes('time')",

                                                                                           sliderInput("w_oa_sdt_time_delay",

                                                                                                       label = tags$b(text$item_label[text$item_id == "w_oa_sdt_time_delay"]),
                                                                                                       min = 0,
                                                                                                       max = 5,
                                                                                                       value = 1,
                                                                                                       width = "100%")

                                                                          ), # /conditionalPanel - input.oa_sdt_what.includes('time')
                                                                          
                                                                          tags$hr(),
                                                                          
                                                                          # S&DT - Specify second S&DT?
                                                                          radioButtons("w_oa_allow_sdt_second",
                                                                                       label = tags$b(text$item_label[text$item_id == "w_oa_allow_sdt_second"]),
                                                                                       choices = c("Yes", "No"),
                                                                                       selected = "No",
                                                                                       width = "100%",
                                                                                       inline = FALSE),
                                                                          
                                                                          conditionalPanel("(input.w_oa_allow_sdt == 'Yes' && input.w_oa_definitions.length > 0 && input.w_oa_allow_sdt_second == 'Yes')",
                                                                                           # S&DT - Who for second?
                                                                                           radioButtons("w_oa_sdt_who_second",
                                                                                                        label = tags$b(text$item_label[text$item_id == "w_oa_sdt_who_second"]),
                                                                                                        choices = c("A", "B", "ldc"),
                                                                                                        selected = "ldc",
                                                                                                        width = "100%",
                                                                                                        inline = FALSE),
                                                                                           
                                                                                           # S&DT - What for second?
                                                                                           checkboxGroupInput("w_oa_sdt_what_second",
                                                                                                              label = tags$b(text$item_label[text$item_id == "w_oa_sdt_what_second"]),
                                                                                                              choices = c("all", "domestic", "HS", "time"),
                                                                                                              selected = "",
                                                                                                              width = "100%",
                                                                                                              inline = FALSE),
                                                                                           
                                                                                           # S&DT - High seas cutoff for second if relevant
                                                                                           conditionalPanel('input.w_oa_sdt_what_second.includes("HS")',
                                                                                                            
                                                                                                            sliderInput("w_oa_sdt_hs_cutoff_second",
                                                                                                                        label = tags$b(text$item_label[text$item_id == "w_oa_sdt_hs_cutoff_second"]),
                                                                                                                        min = 1,
                                                                                                                        max = 100,
                                                                                                                        value = 5,
                                                                                                                        width = "100%")
                                                                                                            
                                                                                           ), # /conditionalPanel - input.oa_sdt_what.includes("HS")
                                                                                           
                                                                                           # S&DT - Time delay for second if relevant
                                                                                           conditionalPanel("input.w_oa_sdt_what_second.includes('time')",
                                                                                                            
                                                                                                            sliderInput("w_oa_sdt_time_delay_second",
                                                                                                                        label = tags$b(text$item_label[text$item_id == "w_oa_sdt_time_delay_second"]),
                                                                                                                        min = 0,
                                                                                                                        max = 5,
                                                                                                                        value = 1,
                                                                                                                        width = "100%")
                                                                                                            
                                                                                           ) # /conditionalpanel - input.w_iuu_sdt_what_second.includes('time')
                                                                                           
                                                                          ) # /conditionalPanel - (input.w_iuu_allow_sdt == 'Yes' && input.w_iuu_definitions.length > 0 && input.w_iuu_allow_sdt_second == 'Yes')
                                                                          
                                                         ) # /conditionalpanel - input.oa_allow_sdt == 'Yes' && input.oa_definitions.length > 0
                                                  ) # /column - OA S&DT
                                                  
                                                ) # /fluidRow - OA
                                                
                                         ) #/column - OA manual selection
                                         
                                         # # Collapsable box with more info
                                         # column(12, style = "padding: 0px 25px 15px; align: 'center';",
                                         #        
                                         #        box(title = "More information on overfished stock disciplines...",
                                         #            collapsible = TRUE,
                                         #            width = "100%",
                                         #            collapsed = T,
                                         #            
                                         #            # Text with links
                                         #            column(12, style = "padding: 0px 10px;",
                                         #                   
                                         #                   includeHTML("./text/06_iuu_more_info.html")
                                         #            )
                                         #            
                                         #        ) # /box - OA more information
                                         # ) # /column - OA more information container
                                         
                                  ) #/coumn - OA tab panel container
                                  
                         ), #/tabPanel 1
                         
                         ### ------------------------------------------
                         ### Tab # 2  - Overcapacity and overfishing disciplines
                         ### ------------------------------------------
                         
                         tabPanel(text$item_label[text$item_id == "overcap"], 
                                  value = "overcap",
                                  
                                  # Column container for tab panel        
                                  column(12, style = "border-style: solid;
                                         border-width: 2px 1px 1px 1px;
                                         border-color: #28292C;",
                                         
                                         # Overcap discipline text
                                         column(12, style = "padding: 15px 25px 15px;",
                                                
                                                includeHTML("./text/02b_edit_policies_overcap_intro.html")
                                                
                                         ),
                                         
                                         # # Overcap proposal dropdown
                                         # column(12, style = "padding: 0px 25px 15px;",
                                         #        
                                         #        # Section header
                                         #        h4("Prefill disciplines from a proposal"),
                                         #        
                                         #        # Dropdown widget
                                         #        column(12, style = "padding: 0px 10px;",
                                         #               
                                         #               selectInput("w_overcap_proposal_selection", 
                                         #                           label = tagList(tags$b("Select a proposal...")),
                                         #                           choices = c("oc1", "oc2"),
                                         #                           selected = "Default",
                                         #                           width = "100%")
                                         #               
                                         #        )
                                         #        
                                         # ),
                                         
                                         # Manual Overcap discipline selection
                                         column(12, style = "padding: 0px 25px 15px;", 
                                                
                                                h4("Manually select overcapacity and overfishing disciplines"),
                                                
                                                fluidRow(
                                                  ### Overcapacity definitions: left column
                                                  column(6, style = "padding: 0 10px;",
                                                         
                                                         checkboxGroupInput("w_overcap_definitions",
                                                                            label = tagList(tags$b("The following types of subsidies are considered to \ncontribute to overcapacity and overfishing and are prohibited..."),
                                                                                            # Info button
                                                                                            tags$button(id = "info_overcap",
                                                                                                        class = "btn action-button info-button",
                                                                                                        icon("info"))), 
                                                                            choices = c("B1", "B2", "B3"),
                                                                            selected = c(""),
                                                                            width = "100%",
                                                                            inline = FALSE)
                                                         
                                                  ), # /column - Overcapcity definitions
                                                  
                                                  ### Overcapacity scope: middle column
                                                  column(3, style = "padding: 0 10px;",
                                                         
                                                         conditionalPanel('input.w_overcap_definitions.length > 0',
                                                                          
                                                                          radioButtons("w_overcap_scope",
                                                                                       label = tags$b("Discipline(s) apply to:"),
                                                                                       choices = c("all", "select"),
                                                                                       selected = "all",
                                                                                       width = "100%",
                                                                                       inline = FALSE), 
                                                                          
                                                                          conditionalPanel("input.w_overcap_scope == 'select'",
                                                                                           
                                                                                           selectizeInput("w_overcap_scope_manual",
                                                                                                          
                                                                                                          label = tags$b("Select Members:"),
                                                                                                          choices = c("A", "B", "D"),
                                                                                                          selected = NULL,
                                                                                                          width = "100%",
                                                                                                          options = list(placeholder = 'Select...'),
                                                                                                          multiple = T)
                                                                                           
                                                                          ) # /conditionalPanel - input.overcap_scope == 'select'
                                                                          
                                                         ), # close overcap scope conditional
                                                         
                                                         conditionalPanel('(input.w_overcap_definitions.length > 0 && (input.w_overcap_scope == "HS" || input.w_overcap_scope == "OUT"))',
                                                                          
                                                                          sliderInput("w_overcap_hs_cutoff",
                                                                                      label = tagList(tags$b("What constitutes a high seas vessel?\n (min % of total fishing \n effort on the high seas)"),
                                                                                                      # Info button
                                                                                                      tags$button(id = "info_overcap_hs",
                                                                                                                  class = "btn action-button info-button",
                                                                                                                  icon("info"))), 
                                                                                      min = 1,
                                                                                      max = 100,
                                                                                      value = 5,
                                                                                      width = "100%")
                                                                          
                                                         ), # close overcap hs cutoff conditional
                                                         
                                                         conditionalPanel('(input.w_overcap_definitions.length > 0 && input.w_overcap_scope == "LENGTH")',
                                                                          
                                                                          sliderInput("w_overcap_length_cutoff",
                                                                                      label = tags$b("Minimum total vessel length (m)"),
                                                                                      min = 10,
                                                                                      max = 100,
                                                                                      value = 24,
                                                                                      width = "100%")
                                                                          
                                                         ) # /conditionalPanel - input.overcap_definitions.length > 0 && input.overcap_scope == "LENGTH"
                                                  ), # close middle column
                                                  
                                                  ### Overcapacity S&DT: right column
                                                  column(3, style = "padding: 0 10px;",
                                                         
                                                         # Overcapacity S&DT - Allow?
                                                         conditionalPanel(
                                                           'input.w_overcap_definitions.length > 0',
                                                           
                                                           radioButtons(
                                                             "w_overcap_allow_sdt",
                                                             label = tags$b("Allow S&DT?"),
                                                             choices = c("Yes", "No"),
                                                             selected = "No",
                                                             width = "100%",
                                                             inline = FALSE
                                                           )
                                                           
                                                         ), # close overcap sdt conditional
                                                         
                                                         conditionalPanel("(input.w_overcap_allow_sdt == 'Yes' && input.w_overcap_definitions.length > 0)",
                                                                          
                                                                          # Overcapacity S&DT - Who does it apply to?
                                                                          radioButtons("w_overcap_sdt_who",
                                                                                       label = tagList(tags$b("S&DT applies to:"),
                                                                                                       # Info button
                                                                                                       tags$button(id = "info_overcap_sdt",
                                                                                                                   class = "btn action-button info-button",
                                                                                                                   icon("info"))), 
                                                                                       choices = c("ldc", "B"),
                                                                                       selected = "ldc",
                                                                                       width = "100%",
                                                                                       inline = FALSE), 
                                                                          
                                                                          
                                                                          # Overcapacity S7DT - What is it?
                                                                          checkboxGroupInput("w_overcap_sdt_what",
                                                                                             
                                                                                             label = tags$b("S&DT:"),
                                                                                             choices = c("A", "B"),
                                                                                             selected = "",
                                                                                             width = "100%",
                                                                                             inline = FALSE), 
                                                                          
                                                                          # Overcapacity S&DT - Define "high seas" fishing (if necessary)
                                                                          conditionalPanel('input.w_overcap_sdt_what.includes("HS")',
                                                                                           
                                                                                           sliderInput("w_overcap_hs_cutoff_sdt",
                                                                                                       label = tags$b("What constitutes a high seas vessel?\n (min % of total fishing effort \n on the high seas)"),
                                                                                                       min = 1,
                                                                                                       max = 100,
                                                                                                       value = 5,
                                                                                                       width = "100%")
                                                                                           
                                                                          ) # /conditionalPanel - input.overcap_sdt_what.includes("HS")
                                                                          
                                                                          # conditionalPanel("input.overcap_sdt_what.includes('time')",
                                                                          #                  sliderInput("overcap_sdt_time_delay",
                                                                          #                              
                                                                          #label = tags$b("Time delay (years):"),
                                                                          #                              min = 0,
                                                                          #                              max = 5,
                                                                          #                              value = 1,
                                                                          #                              width = "100%")
                                                                          #
                                                                          # ) # close overcap sdt time conditional
                                                                          
                                                         ) # close overcap sdt conditional
                                                         
                                                  ) # close right column
                                                  
                                                ), # close overcap fluid row
                                                
                                                ### Cap/Tier here
                                                
                                                column(12,
                                                       
                                                       tags$p("A cap and tier system is an additional constraint imposed on top of any selected prohibitions to further restrict fisheries subsidies. A subsidy cap sets a specific monetary limit on the total amount of fisheries subsidies each member is allowed to provide. Subsidies that exceed this cap would be prohibited. A tiered approach that accommodates the differential circumstances of members can be used. Under this approach, member groups are given different caps based on rule that separates them into tiers.")
                                                       
                                                ),
                                                
                                                # On/off buttons
                                                column(12, align = "center",
                                                       radioButtons("w_cap_on_off",
                                                                    label = tags$i("Apply subsidy caps?"),
                                                                    selected = "No",
                                                                    choices = c("Yes", "No"),
                                                                    width = "100%",
                                                                    inline = FALSE)
                                                ),
                                                
                                                # Cap is turned on
                                                conditionalPanel("input.w_cap_on_off == 'Yes'",
                                                                 
                                                                 # Subsidy types to include in cap
                                                                 column(12, align = "center",
                                                                        checkboxGroupInput("w_cap_subsidy_types",
                                                                                           label = tags$i("Which subsidy type(s) are included in the cap?"),
                                                                                           choices = c("B1", "B2", "B3"),
                                                                                           selected = "B1",
                                                                                           width = "100%",
                                                                                           inline = TRUE)
                                                                 ),
                                                                 
                                                                 # Length
                                                                 conditionalPanel("input.w_cap_subsidy_types.length > 0",
                                                                                  
                                                                                  ### Step 1: Tier structure -------------------
                                                                                  column(12, style = "padding: 5px;",
                                                                                         
                                                                                         h4(class = "header-line-light", "Tier structure"),
                                                                                         
                                                                                         # Row 1: Number of tiers
                                                                                         fluidRow(
                                                                                           column(12, style = "padding: 0 10px;",
                                                                                                  
                                                                                                  radioButtons("w_cap_tier_number",
                                                                                                               label = tags$i("How many tiers?"),
                                                                                                               choices = c("One (cap applies equally to all Members)" = "One",
                                                                                                                           "Two" = "Two",
                                                                                                                           "Three" = "Three"),
                                                                                                               selected = "Three",
                                                                                                               width = "100%",
                                                                                                               inline = FALSE)
                                                                                                  
                                                                                           ) # close left column
                                                                                         ),
                                                                                         
                                                                                         # Row 2: How should Members be sorted into tiers?
                                                                                         fluidRow(
                                                                                           
                                                                                           # Left Column
                                                                                           column(6, style = "padding: 0 10px;",
                                                                                                  
                                                                                                  # Only one tier
                                                                                                  conditionalPanel('input.w_cap_tier_number == "One"',
                                                                                                                   ""
                                                                                                  ),
                                                                                                  
                                                                                                  # Two or three tiers
                                                                                                  conditionalPanel('input.w_cap_tier_number != "One"',
                                                                                                                   radioButtons("w_tier_system",
                                                                                                                                label = tags$i("How should Members be grouped into tiers?"),
                                                                                                                                choices = c("capture", "other"),
                                                                                                                                selected = "capture",
                                                                                                                                width = "100%",
                                                                                                                                inline = FALSE)
                                                                                                  ) # close multiple tier conditional
                                                                                                  
                                                                                           ), # close left column
                                                                                           
                                                                                           # Right Column
                                                                                           column(6, style = "padding: 0 10px;",
                                                                                                  
                                                                                                  # Only one tier
                                                                                                  conditionalPanel('input.w_cap_tier_number == "One"',
                                                                                                                   ""
                                                                                                  ),
                                                                                                  
                                                                                                  # Two tiers
                                                                                                  conditionalPanel('input.w_cap_tier_number == "Two" & input.w_tier_system != "development"',
                                                                                                                   
                                                                                                                   sliderInput("w_two_tier_cutoff",
                                                                                                                               label = tags$i("Threshold for top tier (%)"),
                                                                                                                               min = 0.01,
                                                                                                                               max = 2,
                                                                                                                               step = 0.001,
                                                                                                                               value = 0.7)
                                                                                                                   
                                                                                                  ), # close two tier cutoff conditional
                                                                                                  
                                                                                                  # Three tiers
                                                                                                  conditionalPanel('input.w_cap_tier_number == "Three" & input.w_tier_system != "development"',
                                                                                                                   
                                                                                                                   sliderInput("w_three_tier_cutoff",
                                                                                                                               label = tags$i("Thresholds for bottom (left) and top (right) tiers (%)"),
                                                                                                                               min = 0.01,
                                                                                                                               max = 2,
                                                                                                                               step = 0.01,
                                                                                                                               value = c(0.07,0.7))
                                                                                                                   
                                                                                                  ) # close two tier cutoff conditional
                                                                                           ) # close right column
                                                                                         ) # close tier row
                                                                                  ), # close tier column
                                                                                  
                                                                                  ### Step 2: Subsidy caps
                                                                                  
                                                                                  column(12, style = "padding: 5px;",
                                                                                         
                                                                                         h4(class = "header-line-light", "Set subsidy caps"),
                                                                                         
                                                                                         # Tier 1
                                                                                         fluidRow(style = "background-color: #f7f7f7; padding: 10px;",
                                                                                                  
                                                                                                  column(6, style = "padding: 0 10px;",
                                                                                                         
                                                                                                         # Tier 1 cap:
                                                                                                         radioButtons("w_tier1_cap_rule",
                                                                                                                      label = tags$i("Tier 1: How should the cap be set?"),
                                                                                                                      choices = c("percent_subs", "other"),
                                                                                                                      selected = "percent_subs",
                                                                                                                      width = "100%",
                                                                                                                      inline = FALSE)
                                                                                                         
                                                                                                  ), # close column
                                                                                                  
                                                                                                  column(6, style = "padding: 0 10px;",
                                                                                                         
                                                                                                         # Tier 1 cap:
                                                                                                         # Slider for absolute value
                                                                                                         conditionalPanel(condition = "input.w_tier1_cap_rule == 'value'",
                                                                                                                          
                                                                                                                          sliderInput("w_tier1_cap_value",
                                                                                                                                      label = tags$i("Tier 1: Subsidy cap ($USD, millions)"),
                                                                                                                                      min = 0,
                                                                                                                                      max = 2000,
                                                                                                                                      value = 5,
                                                                                                                                      width = "100%")
                                                                                                         ),
                                                                                                         
                                                                                                         # Slider for per fisher value
                                                                                                         conditionalPanel(condition = "input.w_tier1_cap_rule == 'fishers'",
                                                                                                                          
                                                                                                                          sliderInput("w_tier1_cap_fishers",
                                                                                                                                      label = tags$i("Tier 1: Subsidies per fisher (US$)"),
                                                                                                                                      min = 0,
                                                                                                                                      max = 5000,
                                                                                                                                      value = 800,
                                                                                                                                      width = "100%")
                                                                                                         ),
                                                                                                         
                                                                                                         # Slider for percentage value
                                                                                                         conditionalPanel(condition = "input.w_tier1_cap_rule != 'value' && input.w_tier1_cap_rule != 'fishers'",
                                                                                                                          
                                                                                                                          sliderInput("w_tier1_cap_percent",
                                                                                                                                      label = tags$i("Tier 1: Percent for cap rule"),
                                                                                                                                      min = 0,
                                                                                                                                      max = 100,
                                                                                                                                      value = 5,
                                                                                                                                      width = "100%")
                                                                                                         )
                                                                                                  )
                                                                                         ), # close fluid row
                                                                                         
                                                                                         conditionalPanel('input.w_cap_tier_number != "One"',
                                                                                                          
                                                                                                          # Tier 2
                                                                                                          fluidRow(style = "background-color: #d6d6d6; padding: 10px;",
                                                                                                                   
                                                                                                                   # Left column
                                                                                                                   column(6, style = "padding: 0 10px;",
                                                                                                                          
                                                                                                                          # Tier 2 cap:
                                                                                                                          radioButtons("w_tier2_cap_rule",
                                                                                                                                       label = tags$i("Tier 2: How should the cap be set?"),
                                                                                                                                       choices = c("Value" = "value", "No cap" = "none"),
                                                                                                                                       selected = "value",
                                                                                                                                       width = "100%",
                                                                                                                                       inline = FALSE)
                                                                                                                          
                                                                                                                   ), # close column
                                                                                                                   
                                                                                                                   # Middle column
                                                                                                                   column(6, style = "padding: 0 10px;",
                                                                                                                          
                                                                                                                          # Tier 1 cap:
                                                                                                                          # Slider for absolute value
                                                                                                                          conditionalPanel(condition = "input.w_tier2_cap_rule == 'value'",
                                                                                                                                           sliderInput("w_tier2_cap_value",
                                                                                                                                                       label = tags$i("Tier 2: Subsidy cap ($USD, millions)"),
                                                                                                                                                       min = 0,
                                                                                                                                                       max = 500,
                                                                                                                                                       value = 5,
                                                                                                                                                       width = "100%")
                                                                                                                          ),
                                                                                                                          
                                                                                                                          # Slider for per fisher value
                                                                                                                          conditionalPanel(condition = "input.w_tier2_cap_rule == 'fishers'",
                                                                                                                                           sliderInput("w_tier2_cap_fishers",
                                                                                                                                                       label = tags$i("Tier 2: Subsidies per fisher (US$)"),
                                                                                                                                                       min = 0,
                                                                                                                                                       max = 5000,
                                                                                                                                                       value = 800,
                                                                                                                                                       width = "100%")
                                                                                                                          ),
                                                                                                                          
                                                                                                                          # Slider for percentage value
                                                                                                                          conditionalPanel(condition = "input.w_tier2_cap_rule != 'value' && input.w_tier2_cap_rule != 'none' && input.w_tier2_cap_rule != 'fishers'",
                                                                                                                                           sliderInput("w_tier2_cap_percent",
                                                                                                                                                       label = tags$i("Tier 2: Percent for cap rule"),
                                                                                                                                                       min = 0,
                                                                                                                                                       max = 100,
                                                                                                                                                       value = 5,
                                                                                                                                                       width = "100%")
                                                                                                                          )
                                                                                                                   )
                                                                                                          ) # close fluid row
                                                                                         ), # close tier 2 conditional
                                                                                         
                                                                                         conditionalPanel('input.w_cap_tier_number == "Three"',
                                                                                                          
                                                                                                          # Tier 3
                                                                                                          fluidRow(style = "background-color: #f7f7f7; padding: 10px;",
                                                                                                                   
                                                                                                                   column(6, style = "padding: 0 10px;",
                                                                                                                          
                                                                                                                          # Tier 1 cap:
                                                                                                                          radioButtons("w_tier3_cap_rule",
                                                                                                                                       label = tags$i("Tier 3: How should the cap be set?"),
                                                                                                                                       choices = c("Value" = "value",
                                                                                                                                                   "No cap" = "none"),
                                                                                                                                       selected = "none",
                                                                                                                                       width = "100%",
                                                                                                                                       inline = FALSE)
                                                                                                                          
                                                                                                                   ), # close column
                                                                                                                   
                                                                                                                   column(6, style = "padding: 0 10px;",
                                                                                                                          
                                                                                                                          # Tier 1 cap:
                                                                                                                          # Slider for absolute value
                                                                                                                          conditionalPanel(condition = "input.w_tier3_cap_rule == 'value'",
                                                                                                                                           sliderInput("w_tier3_cap_value",
                                                                                                                                                       label = tags$i("Tier 3: Subsidy cap ($USD, millions)"),
                                                                                                                                                       min = 0,
                                                                                                                                                       max = 500,
                                                                                                                                                       value = 5,
                                                                                                                                                       width = "100%")
                                                                                                                          ),
                                                                                                                          
                                                                                                                          # Slider for per fisher value
                                                                                                                          conditionalPanel(condition = "input.w_tier3_cap_rule == 'fishers'",
                                                                                                                                           sliderInput("w_tier3_cap_fishers",
                                                                                                                                                       label = tags$i("Tier 3: Subsidies per fisher (US$)"),
                                                                                                                                                       min = 0,
                                                                                                                                                       max = 5000,
                                                                                                                                                       value = 800,
                                                                                                                                                       width = "100%")
                                                                                                                          ),
                                                                                                                          
                                                                                                                          # Slider for percentage value
                                                                                                                          conditionalPanel(condition = "input.w_tier3_cap_rule != 'value' && input.w_tier3_cap_rule != 'none' && input.w_tier3_cap_rule != 'fishers'",
                                                                                                                                           sliderInput("w_tier3_cap_percent",
                                                                                                                                                       label = tags$i("Tier 3: Percent for cap rule"),
                                                                                                                                                       min = 0,
                                                                                                                                                       max = 100,
                                                                                                                                                       value = 5,
                                                                                                                                                       width = "100%")
                                                                                                                          )
                                                                                                                   )
                                                                                                          ) # close fluid row
                                                                                         ) # close tier 3 conditional
                                                                                         
                                                                                  ) # close cap selection
                                                                                  
                                                                 ) # close subsidy types conditional
                                                                 
                                                ) # close cap on/off conditional 
                                                
                                         ) # close overcap manual selection
                                         
                                         # # Collapsable box with more info
                                         # column(12, style = "padding: 0px 25px 15px; align: 'center';",
                                         #        
                                         #        box(title = "More information on overcapacity and overfishing disciplines...",
                                         #            collapsible = TRUE,
                                         #            width = "100%",
                                         #            collapsed = T,
                                         #            
                                         #            # Text with links
                                         #            column(12, style = "padding: 0px 10px;",
                                         #                   
                                         #                   includeHTML("./text/06_iuu_more_info.html")
                                         #            )
                                         #            
                                         #        ) # /box - OCOF more information
                                         # ) # /column - OCOF more information container
                                         
                                  ) # close overcap container column
                                  
                         ) # /tabPanel #2  
                         
                  ) # /tabBox
                  
           ) # /column 12 (tabBox container)
           
    ) # /column 12 (content)
    
  ), # /column 8 - left column
           
 ####-----------------------------------------------------------------------------------------
 #### Right column - Menu of selected policies
 #### ----------------------------------------------------------------------------------------
           
 column(3,
        style = "position: absolute; 
        background-color: #286182; 
        color: #ffffff; 
        padding: 0 10px;
        top:0;
        bottom:0;
        right:0;",
                  
        tags$h4(text$item_label[text$item_id == "selected-policy"]),
        tags$p("Text here")
        
 )
 
), # close column 12
                  
  ### Bottom navigation buttons
  column(12,
       fluidRow(style = "padding: 5px 5px; background-color: #3c8dbc;",
                
                # Back to compare fishery stats
                column(3,
                       tags$button(id = "ab_edit_policies_to_selected_results",
                                   class = "btn action-button nav-button-l",
                                   icon("chevron-left"), button_text$text[button_text$id == "ab_edit_policies_to_selected_results"]
                       )
                )
                
       )
  )
    
  ) # /fluidPage
  
  
