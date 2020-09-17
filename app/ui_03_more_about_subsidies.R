### -------------------------------------------------------------------
### SubsidyExplorer
### An interactive toolkit to explore the tradeoffs across proposed fisheries subsidies disciplines
### 
### This script contains the content for the global-subsidies tab
### --------------------------------------------------------------------

MoreAboutSubsidies = function(subsidy_types_sorted_sumaila, wto_members_and_observers) 
  fluidPage(
    
    # Page style
    style = "background-color: #ffffff;",
    
    ### tabBox container
    column(12,
           
           # tabBox
           tabBox(width = 12, id = "subsidy-data-tabs", 
                  
                  ### --------------------------
                  ### Tab # 1  - Global fisheries subsidies
                  ### --------------------------
                  
                  tabPanel(value = "global-subsidies-tab",
                    
                           # Title
                           tags$h4(text$item_label[text$item_id == "global-subsidies"]),
                           
                           # Content
                           GlobalSubsidies(subsidy_types_sorted_sumaila)
                  ),
                  
                  ### --------------------------
                  ### Tab # 2  - Fishery stats by state
                  ### --------------------------
                  
                  tabPanel(value = "country-fishery-stats-tab",
                           
                           # Title
                           tags$h4(text$item_label[text$item_id == "country-fishery-stats"]),
                           
                           # Content
                           CountryFisheryStats(wto_members_and_observers)
                  ),
                  
                  ### --------------------------
                  ### Tab # 3  - Compare fishery stats
                  ### --------------------------
                  
                  tabPanel(value = "compare-fishery-stats-tab",
                           
                           # Title
                           tags$h4(text$item_label[text$item_id == "compare-fishery-stats"]),
                           
                           # Content
                           CompareFisheryStats(wto_members_and_observers, subsidy_types_sorted_sumaila)
                  ),
                  
                  ### --------------------------
                  ### Tab # 4  - Global fishing footprint
                  ### --------------------------
                  
                  tabPanel(value = "global-fishing-footprint-tab",
                           
                           # Title
                           tags$h4(text$item_label[text$item_id == "global-fishing-footprint"]),
                           
                           # Content
                           GlobalFishingFootprint()
                  )
                  
           )
    )
                  
  ) # /fluidPage
  
  
