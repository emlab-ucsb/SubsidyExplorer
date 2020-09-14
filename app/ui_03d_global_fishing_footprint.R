### -------------------------------------------------------------------
### SubsidyExplorer
### An interactive toolkit to explore the tradeoffs across proposed fisheries subsidies disciplines
### 
### This script contains the content for the global-fishing-footprint tab
### --------------------------------------------------------------------

GlobalFishingFootprint = function() 
  fluidPage(
    
    # Page style
    style = "background-color: #262626; color: #ffffff;",
    
    ### Title and introductory text 
    column(12, style = "padding: 25px 25px;",
           
           # Title
           tags$h3(style = "text-align: left; padding: 0; margin: 0 0 10px; color: #ffffff;", text$item_label[text$item_id == "global-fishing-footprint"], tags$button(id = "info_global_fishing_footprint",
                                                                                                                                 class = "btn action-button info-button",
                                                                                                                                 icon("info"))),
           
           # Text
           includeHTML("./text/04d_global_fishing_footprint_intro.html")
           
    ),
    
    ### Global fishing effort map
    column(12, style = "padding: 10px 0;",
           
           
           # Leaflet map
           leafletOutput('global_fishing_footprint_map', width = "auto", height = "70vh")
           
           
    ),
    
    ### Map disclaimer
    column(12, style = "padding: 25px 25px; color: #ffffff;",
           
           tags$i(text$item_label[text$item_id == "map_disclaimer"])
    )
            
  ) # /fluidPage
  
  
