### -------------------------------------------------------------------
### SubsidyExplorer
### An interactive toolkit to explore the tradeoffs across proposed fisheries subsidies disciplines
### 
### This script contains the content for the global-fishing-footprint tab
### --------------------------------------------------------------------

GlobalFishingFootprint = function() 
  fluidPage(
    
    # Page style
    style = "background-color: #ffffff; color: #000000;",
    
    ### Global subsidies map
    column(12,
           
           # Leaflet map
           leafletOutput('global_fishing_footprint_map', width = "auto", height = "80vh"),
           
           # Info button 
           absolutePanel(id = "global_fishing_footprint_map_info_panel", 
                         
                         column(12, id = "t-spaced-div", align = "center",
                                
                                
                                tagList(tags$b(text$item_label[text$item_id == "global-fishing-footprint"]),
                                        # Info button
                                        tags$button(id = "info_global_fishing_footprint_map",
                                                    class = "btn action-button info-button",
                                                    icon("info")))
                         ),
                         column(12, id = "tb-spaced-div", align = "center",
                                
                                downloadButton("db_global_fishing_footprint_download_figure",
                                               text$item_label[text$item_id == "db_global_fishing_footprint_download_figure"]),
                                
                                # tags$button(id = "db_global_fishing_footprint_download_figure",
                                #             class = "btn action-button rounded-button-grey download-button",
                                #             tags$b(icon("download"),
                                #                    text$item_label[text$item_id == "db_global_fishing_footprint_download_figure"]))
                                
                         )
           ),
           
           # Map disclaimer
           # Button to hide disclaimer 
           shinyjs::hidden(absolutePanel(id = "global_fishing_footprint_map_hide_arrow_disclaimer",
                                         
                                         tags$button(id = "ab_global_fishing_footprint_hide_disclaimer",
                                                     class = "btn action-button",
                                                     icon("caret-down"))
           )),
           
           # Button to open left panel
           absolutePanel(id = "global_fishing_footprint_map_expand_arrow_disclaimer",
                         
                         tags$button(id = "ab_global_fishing_footprint_expand_disclaimer",
                                     class = "btn action-button",
                                     icon("caret-up"))
           ),
           
           shinyjs::hidden(absolutePanel(id = "global_fishing_footprint_map_disclaimer_panel",
                         
                         column(12, id = "tb-spaced-div", align = "center",
                                
                                text$item_label[text$item_id == "map_disclaimer"] %>% lapply(htmltools::HTML)
                                
                         )
                         
           ))
           
    )
            
  ) # /fluidPage
  
  
