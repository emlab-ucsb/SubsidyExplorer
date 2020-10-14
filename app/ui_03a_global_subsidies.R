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
           leafletOutput('global_subsidies_map', width = "auto", height = "80vh"),
           
           # Button to hide left panel 
           shinyjs::hidden(absolutePanel(id = "global_subsidies_map_hide_arrow_panel",
                         
                         tags$button(id = "ab_global_subsidies_hide_panel",
                                     class = "btn action-button",
                                     icon("caret-left"))
           )),
           
           # Button to open left panel
           absolutePanel(id = "global_subsidies_map_expand_arrow_panel",
                         
                         tags$button(id = "ab_global_subsidies_expand_panel",
                                     class = "btn action-button",
                                     icon("caret-right"))
           ),

           # Widgets
           shinyjs::hidden(absolutePanel(id = "global_subsidies_map_control_panel",
                        
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
                                
                                # Good Subsidy Types Header
                                tags$table(id = "global-subsidies-control-table",
                                  tags$tr(id = "global-subsidies-control-table-row",
                                    # Title
                                    tags$td(id = "global-subsidies-control-table-cell-l",
                                      tags$b(text$item_label[text$item_id == "w_global_subsidies_good_types"], 
                                             style = paste0("color:", goodColors[1], ";"))
                                      
                                    ),
                                    tags$td(id = "global-subsidies-control-table-cell-r",
                                            # Select all button
                                            tags$button(id = "ab_global_subsidies_select_all_good",
                                                        text$item_label[text$item_id == "ab_global_subsidies_select_all"],
                                                        class = "btn action-button")
                                    )
                                  
                                  )
                                ),
                                
                                # Good Subsidy Types Widget
                                checkboxGroupInput("w_global_subsidies_good_types",
                                                   label = NULL,
                                                   choices = subsidy_types_sorted_sumaila[1:3],
                                                   selected = subsidy_types_sorted_sumaila[1:3],
                                                   width = "100%"),
                                
                                # Ugly Subsidy Types Header
                                tags$table(id = "global-subsidies-control-table",
                                           tags$tr(id = "global-subsidies-control-table-row",
                                                   # Title
                                                   tags$td(id = "global-subsidies-control-table-cell-l",
                                                           tags$b(text$item_label[text$item_id == "w_global_subsidies_ugly_types"], 
                                                                  style = paste0("color:", ambigColors[1], ";"))
                                                           
                                                   ),
                                                   tags$td(id = "global-subsidies-control-table-cell-r",
                                                           # Select all button
                                                           tags$button(id = "ab_global_subsidies_select_all_ugly",
                                                                       text$item_label[text$item_id == "ab_global_subsidies_select_all"],
                                                                       class = "btn action-button")
                                                   )
                                                   
                                           )
                                ),
                                
                                # Ugly Subsidy Types Widget
                                checkboxGroupInput("w_global_subsidies_ugly_types",
                                                   label =  NULL,
                                                   choices = subsidy_types_sorted_sumaila[11:13],
                                                   selected = subsidy_types_sorted_sumaila[11:13],
                                                   width = "100%"),
                                
                                # Ugly Subsidy Types Header
                                tags$table(id = "global-subsidies-control-table",
                                           tags$tr(id = "global-subsidies-control-table-row",
                                                   # Title
                                                   tags$td(id = "global-subsidies-control-table-cell-l",
                                                           tags$b(text$item_label[text$item_id == "w_global_subsidies_bad_types"], 
                                                                  style = paste0("color:", badColors[1], ";"))
                                                           
                                                   ),
                                                   tags$td(id = "global-subsidies-control-table-cell-r",
                                                           # Select all button
                                                           tags$button(id = "ab_global_subsidies_select_all_bad",
                                                                       text$item_label[text$item_id == "ab_global_subsidies_select_all"],
                                                                       class = "btn action-button")
                                                   )
                                                   
                                           )
                                ),
                                
                                # Bad Subsidy Types Widget
                                checkboxGroupInput("w_global_subsidies_bad_types",
                                                   label =  NULL,
                                                   choices = subsidy_types_sorted_sumaila[4:10],
                                                   selected = subsidy_types_sorted_sumaila[4:10],
                                                   width = "100%")
                                
                         )

           )),
           
           # Info button 
           absolutePanel(id = "global_subsidies_map_info_panel", 
                         
                         column(12, id = "t-spaced-div", align = "center",
                         
                                
                                tagList(tags$b(text$item_label[text$item_id == "global-subsidies"]),
                                        # Info button
                                        tags$button(id = "info_global_subsidies_map",
                                                    class = "btn action-button info-button",
                                                    icon("info")))
                         ),
                         column(12, id = "tb-spaced-div", align = "center",
                                
                                tags$table(id = "global-subsidies-table",
                                  
                                  tags$tr(id = "global-subsidies-table-row",
                                    
                                    tags$td(id = "global-subsidies-table-cell-1",
                                            
                                            downloadButton("db_global_subsidies_download_data",
                                                           text$item_label[text$item_id == "db_global_subsidies_download_data"])
                                    ),
                                    
                                    tags$td(id = "global-subsidies-table-cell-2",
                                            
                                            downloadButton("db_global_subsidies_download_figure",
                                                           text$item_label[text$item_id == "db_global_subsidies_download_figure"])
                                      
                                    )
                                  )
                                )
                         )
           ),
           
           # Map disclaimer
           # Button to hide disclaimer 
           shinyjs::hidden(absolutePanel(id = "global_subsidies_map_hide_arrow_disclaimer",
                                         
                                         tags$button(id = "ab_global_subsidies_hide_disclaimer",
                                                     class = "btn action-button",
                                                     icon("caret-down"))
           )),
           
           # Button to open left panel
           absolutePanel(id = "global_subsidies_map_expand_arrow_disclaimer",
                         
                         tags$button(id = "ab_global_subsidies_expand_disclaimer",
                                     class = "btn action-button",
                                     icon("caret-up"))
           ),
           
           shinyjs::hidden(absolutePanel(id = "global_subsidies_map_disclaimer_panel",
                         
                         column(12, id = "tb-spaced-div", align = "center",
                                
                                text$item_label[text$item_id == "map_disclaimer"] %>% lapply(htmltools::HTML)
                                
                         )
                         
           ))

    )
            
  ) # /fluidPage
  
  
