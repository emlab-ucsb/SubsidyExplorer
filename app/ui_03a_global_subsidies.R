### -------------------------------------------------------------------
### SubsidyExplorer
### An interactive toolkit to explore the tradeoffs across proposed fisheries subsidies disciplines
### 
### This script contains the content for the global-subsidies tab
### --------------------------------------------------------------------

GlobalSubsidies = function(subsidy_types_sorted_sumaila) 
  fluidPage(
    
    # Page style
    style = "background-color: #ffffff; color: #000000;",
    
    ### Global subsidies map
    column(12,
           
           # Leaflet map
           leafletOutput('global_subsidies_map', width = "auto", height = "89vh"),
           
           # Widgets
           absolutePanel(id = "global_subsidies_map_control_panel",
                         
                         column(12, id = "lr-spaced-div",
                                
                                # Title 
                                tagList(
                                  tags$b(text$item_label[text$item_id == "subsidy-types-to-include"]),
                                  # Info button
                                  tags$button(id = "info_global_subsidies_subsidy_types",
                                              class = "btn action-button info-button",
                                              icon("info"))),
                                
                                # Buttons
                                column(12, id = "tb-spaced-div",
                                       
                                       fluidRow(
                                         
                                         column(6, style = "padding-right: 5px;",
                                                
                                                # Button to select all
                                                tags$button(id = "ab_global_subsidies_select_all",
                                                            
                                                            text$item_label[text$item_id == "ab_global_subsidies_select_all"],
                                                            class = "btn action-button rounded-button-grey")
                                                
                                                
                                         ),
                                         
                                         column(6, style = "padding-left: 5px;",
                                                
                                                # Button to clear selection
                                                tags$button(id = "ab_global_subsidies_clear_all",
                                                            text$item_label[text$item_id == "ab_global_subsidies_clear_all"],
                                                            class = "btn action-button rounded-button-grey")
                                                
                                         )
                                         
                                       )
                                ),
                                
                                # Subsidy Type Widgets
                                checkboxGroupInput("w_global_subsidies_good_types",
                                                   label =  tags$b(text$item_label[text$item_id == "w_global_subsidies_good_types"], 
                                                                   style = paste0("color:", goodColors[1], ";")),
                                                   choices = subsidy_types_sorted_sumaila[1:3],
                                                   selected = subsidy_types_sorted_sumaila[1:3],
                                                   width = "100%"),
                                
                                checkboxGroupInput("w_global_subsidies_ugly_types",
                                                   label =  tags$b(text$item_label[text$item_id == "w_global_subsidies_ugly_types"], 
                                                                   style = paste0("color:", ambigColors[1], ";")),
                                                   choices = subsidy_types_sorted_sumaila[11:13],
                                                   selected = subsidy_types_sorted_sumaila[11:13],
                                                   width = "100%"),
                                
                                checkboxGroupInput("w_global_subsidies_bad_types",
                                                   label =  tags$b(text$item_label[text$item_id == "w_global_subsidies_bad_types"], 
                                                                   style = paste0("color:", badColors[1], ";")),
                                                   choices = subsidy_types_sorted_sumaila[4:10],
                                                   selected = subsidy_types_sorted_sumaila[4:10],
                                                   width = "100%")
                                
                         )

           ),
           
           # Info button 
           absolutePanel(id = "global_subsidies_map_info_panel", 
                         
                         column(12, id = "tb-spaced-div", align = "center",
                         
                                
                                tagList(tags$b(text$item_label[text$item_id == "global-subsidies"]),
                                        # Info button
                                        tags$button(id = "info_global_subsidies_map",
                                                    class = "btn action-button info-button",
                                                    icon("info")))
                         )
           ),
           
           # Map disclaimer
           absolutePanel(id = "global_subsidies_map_disclaimer_panel",
                         
                         column(12, id = "tb-spaced-div", align = "center",
                                
                                text$item_label[text$item_id == "map_disclaimer"] %>% lapply(htmltools::HTML)
                                
                         )
                         
           )
        
           
    )
            
  ) # /fluidPage
  
  
