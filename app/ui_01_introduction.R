### -------------------------------------------------------------------
### SubsidyExplorer
### An interactive toolkit to explore the tradeoffs across proposed fisheries subsidies disciplines
### 
### This script contains the content for the "Introduction" tab
### --------------------------------------------------------------------

Introduction = function() 
  fluidPage(
    
    # page style
    style = "color: #ffffff; padding-bottom: 40px;",
            
    # open links in new window
    tags$head(tags$base(target = "_blank")),
    
    ### Image with text -------------------

    # Parent container: landing page header (photo + overlay)
    tags$div(class = "intro-div",
             
             # Child element 1: background image
             tags$div(class = "intro-picture-div",

                      # tags$iframe(src = "https://www.youtube.com/embed/xYl4m0xFcCU?controls=0&amp;start=2",
                      #             frameborder="0",
                      #             allow="autoplay; encrypted-media; gyroscope; picture-in-picture", allowfullscreen=NA,
                      #             style='width:100vw;height:90vh;')
                      
                      tags$video(src = "fish_swimming.mp4",
                                 type = "video/mp4",
                                 autoplay = "autoplay",
                                 style = "min-width: 100vw; min-height: 90vh; max-width: 200%; max-height: 200%;")

                      # tags$img(src = "fish-background.jpeg")

             ),
                      
             # Child element 2: overlay
             tags$div(class = "intro-overlay-div",
                      
                      # Main text 
                      column(12, id = "spaced-div",
                             
                             tags$table(id = "introduction-table",
                                        
                                        tags$tr(id = "introduction-table-table-row",
                                                
                                                tags$td(id = "introduction-table-cell-l",
                                                        
                                                        tags$h3("Subsidy reform represents one of the most beneficial actions we can take to restore our world's oceans"),
                                                        tags$p("SubsidyExplorer is an interactive toolkit that allows users to learn more about fisheries subsidies and to explore potential biological and economic tradeoffs associated with subsidy reform. This tool supports the negotiations currently underway at the World Trade Organization (WTO).")
                                                        
                                                ),
                                                
                                                tags$td(id = "introduction-table-cell-r",
                                                        
                                                        tags$h4("An ambitious agreement on fisheries subsidies reform could result in increases of up to"),
                                                        tags$h3(paste0("+ ", biomass_end_percent, "% in global fish biomass"), id = "color-text-top"),
                                                        tags$h3(paste0("+ ", catch_end_percent, "% in global fish catch"), id = "color-text-bottom"),
                                                        tags$h4(paste0("Resulting in ", biomass_end_value, " million more tons of fish in the water, and ", catch_end_value, " million more tons of fish being caught."))
                                                )
                                        )
                             )
                      ),
                      
                      ### Button 1
                      column(12, id = "tb-spaced-div",
                             
                             actionButton("ab_introduction_to_explore_results",
                                          tags$h3(text$item_label[text$item_id == "ab_introduction_to_explore_results"], icon("caret-right")))
                      ),
                      
                      ### Buttons 2-3
                      column(12, id = "tb-spaced-div", align = "center",
                             
                             tags$table(id = "introduction-table", style = "text-align: center;",
                                        
                                        tags$tr(id = "introduction-table-table-row",
                                                
                                                tags$td(id = "introduction-table-cell-l", style = "padding: 0;",
                                                        
                                                        actionButton("ab_introduction_to_global_subsidies",
                                                                     tags$h4(text$item_label[text$item_id == "ab_introduction_to_global_subsidies"], icon("caret-right")))
                                                ),
                                                
                                                tags$td(id = "introduction-table-cell-r", style = "padding: 0;",
                                                        
                                                        actionButton("ab_introduction_to_methods_process",
                                                                     tags$h4(text$item_label[text$item_id == "ab_introduction_to_methods_process"], "   ", icon("caret-right")))
                                                )
                                        )
                             )

                      )
                      
             ) # /div picture-overlay-black
             
    ) # /div landing-wrapper
    
  ) # /fluidPage
  