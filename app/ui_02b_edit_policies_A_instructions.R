### -------------------------------------------------------------------
### SubsidyExplorer
### An interactive toolkit to explore the tradeoffs across proposed fisheries subsidies disciplines
### 
### This script contains the content for the "instructions" box on the edit-policies" tab
### --------------------------------------------------------------------

Instructions = function() 
  # Column container for tab panel        
  column(12, style = "border-style: solid; border-width: 2px 1px 1px 1px; border-color: #28292C;",
                  
         # Intro text and policy name
         column(12, style = "padding: 15px 25px 15px;",
                         
                # Introductory text
                includeHTML("./text/02b_edit_policies_intro.html"),
                         
                # Provide policy description
                textInput("w_run_name",
                          label = text$item_label[text$item_id == "w_run_name"],
                          value = text$value[text$item_id == "w_run_name"])
                
         ), # /column 12
                  
         # Next tab button
         column(3, offset = 9, style = "padding: 5px;",
                         
                tags$button(id = "ab_edit_policies_tabs_instructions_to_iuu",
                            class = "btn action-button nav-button-white-r",
                            text$item_label[text$item_id == "ab_edit_policies_tabs_instructions_to_iuu"], icon("chevron-right"))
                
         ) # /column 3
                  
  ) # /column 12 - column container for instructions tabPanel
