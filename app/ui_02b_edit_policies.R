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
    
    ### Content
    column(12,
    
           ####-----------------------------------------------------------------------------------------
           #### Left column - tabBox with manual policy selection
           #### ----------------------------------------------------------------------------------------
           column(9,
    
                  ### Title and introductory text
                  column(12, style = "padding: 25px 25px;",
           
                         # Title
                         tags$h3(style = "text-align: left; padding: 0; margin: 0 0 10px;", text$item_label[text$item_id == "edit-policies"])
                         
                  ),
    
                  ### tabBox container
                  column(12, style = "padding: 10px 0 25px;",

                         # tabBox
                         tabBox(width = 12, id = "policy_tabs", 
                         
                                ### --------------------------
                                ### Tab # 0  - Instructions
                                ### --------------------------
                                
                                tabPanel(text$item_label[text$item_id == "instructions"],
                                         value = "instructions",
                                         
                                         Instructions()
                                  
                                ), # /tabPanel - instructions
                         
                                ### --------------------------
                                ### Tab # 1  - IUU
                                ### --------------------------
                                
                                tabPanel(text$item_label[text$item_id == "iuu"], 
                                         value = "iuu",
                                         
                                         IUU(wto_members_and_observers)
                                         
                                ), # /tabPanel #0
                         
                                ### --------------------------
                                ### Tab # 2  - Overfished stock disciplines
                                ### --------------------------
                                
                                tabPanel(text$item_label[text$item_id == "oa"], 
                                         value = "oa",
                                         
                                         OA(wto_members_and_observers)
                                         
                                ), #/tabPanel 1
                         
                                ### ------------------------------------------
                                ### Tab # 3  - Overcapacity and overfishing disciplines
                                ### ------------------------------------------
                                
                                tabPanel(text$item_label[text$item_id == "overcap"], 
                                         value = "overcap",
                                         
                                         Overcap(wto_members_and_observers, subsidy_types_sorted_sumaila)
                                         
                                ) # /tabPanel #3  
                         
                         ) # /tabBox
                  
                  ) # /column 12 - tabBox container
           
           ), # /column 9 - Left column
    
           
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
                  
           ) # /column 3 - Right column
 
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
  
  
