### -------------------------------------------------------------------
### SubsidyExplorer
### An interactive toolkit to explore the tradeoffs across proposed fisheries subsidies disciplines
### 
### This script contains the content for the global-subsidies tab
### --------------------------------------------------------------------

GlobalSubsidies = function(subsidy_types_sorted_sumaila) 
  fluidPage(
    
    # Page style
    style = "background-color: #ffffff;",
    
    # ### Select data types
    # column(12, id = "tb-spaced-div",
    #        
    #        fluidRow(
    #          
    #          # Select category of subsidies to plot
    #          column(6, style = "padding: 0 12.5px 0 0;",
    #                 
    #                 selectInput("w_global_subsidies_category",
    #                             label = tags$b(text$item_label[text$item_id == "w_global_subsidies_category"]),
    #                             choices = unlist(wid$choices[wid$item_id == "w_global_subsidies_category"]),
    #                             selected = unlist(wid$selected[wid$item_id == "w_global_subsidies_category"]),
    #                             width = "100%")
    #          ),
    #          
    #          # Select subsidy type(s) within selected category to plot
    #          column(6, style = "padding: 0 0 0 12.5px;",
    #                 
    #                 #conditionalPanel("input.w_global_subsidies_category != 'All'",
    #                                  
    #                
    #                 
    #                 #) # /conditionalPanel
    #          )
    #        )
    #        
    # ),
    
    ### Global subsidies map
    column(12,
           
           
           # Leaflet map
           leafletOutput('global_subsidies_map', width = "100%", height = "90vh"),
           
           # Widgets
           absolutePanel(id = "subsidy_types_control_panel",
                         left = 0, top = 50, bottom = 0, width = "20vw",
                         
                         selectInput("w_global_subsidies_category",
                                     label = tags$b(text$item_label[text$item_id == "w_global_subsidies_category"]),
                                     choices = unlist(wid$choices[wid$item_id == "w_global_subsidies_category"]),
                                     selected = unlist(wid$selected[wid$item_id == "w_global_subsidies_category"]),
                                     width = "100%"),
                         
                         checkboxGroupInput("w_global_subsidies_types",
                                            label = tagList(tags$b(text$item_label[text$item_id == "w_global_subsidies_types"]),
                                                        # Info button
                                                        tags$button(id = "info_subsidy_type_to_plot",
                                                                    class = "btn action-button info-button",
                                                                    icon("info"))),
                                        
                                            choices = subsidy_types_sorted_sumaila,
                                            selected = subsidy_types_sorted_sumaila,
                                            width = "100%")
                         
           ),
           
           # Info button 
           absolutePanel(id = "subsidy_types_info_panel",
                         top = 25, right = 25, width = "100px",
                         
                         tags$button("button", 'test-button', class = "rounded-button")
           ),
           
           # Map disclaimer
           absolutePanel(id = "map_disclaimer+panel",
                         bottom = 25, left = "30vw", right = "30vw",
                         
                         tags$small(text$item_label[text$item_id == "map_disclaimer"])
                         
           )
        
           
    )
    
    # ### Map disclaimer
    # column(12, id = "spaced-div", align = "center",
    #        
    #        tags$small(text$item_label[text$item_id == "map_disclaimer"])
    #        
    # )
            
  ) # /fluidPage
  
  
