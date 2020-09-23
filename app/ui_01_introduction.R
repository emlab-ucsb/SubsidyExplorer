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

                      tags$video(src = "fishschool.mp4",
                                 type = "video/mp4",
                                 autoplay = NA,
                                 style = "min-width: 100vw; min-height: 90vh; max-width: 200%; max-height: 200%;")

             ),
                      
             # Child element 2: overlay
             tags$div(class = "intro-overlay-div",
                      
                      # Main text 
                      column(12, id = "spaced-div",
                             
                             tags$table(id = "introduction-table",
                                        
                                        tags$tr(id = "introduction-table-table-row",
                                                
                                                tags$td(id = "introduction-table-cell-l",
                                                        
                                                        tags$h2(text$item_label[text$item_id == "left-top"]),
                                                        tags$p(text$item_label[text$item_id == "left-bottom"])

                                                ),
                                                
                                                tags$td(id = "introduction-table-cell-r",
                                                        
                                                        tags$h4(text$item_label[text$item_id == "right-top"], style = "font-weight: italic; color: var(--accent-color);"),
                                                        tags$h2(paste0("+ ", biomass_end_percent), tags$sup("%"), " in global fish biomass", style = "color: var(--accent-color-2); text-align: center;"),
                                                        tags$h2(paste0("+ ", catch_end_percent), tags$sup("%"), " in global fish catch per year", style = "color: var(--accent-color-2); text-align: center;"),
                                                        tags$h4(paste0("This would mean ", biomass_end_value, " million more tons of fish in the water, and ", catch_end_value, " million more tons of fish being caught every year!"), style = "color: var(--accent-color);")
                                                )
                                        )
                             )
                      ),
                      
                      ### Button 1
                      column(12, id = "tb-spaced-div",
                             
                             actionButton("ab_introduction_to_explore_results",
                                          tags$h3(text$item_label[text$item_id == "ab_introduction_to_explore_results"], icon("caret-right"), style = "color: white;"))
                      ),
                      
                      ### Buttons 2-3
                      column(12, id = "tb-spaced-div", align = "center",
                             
                             tags$table(id = "introduction-table", style = "text-align: center;",
                                        
                                        tags$tr(id = "introduction-table-table-row",
                                                
                                                tags$td(id = "introduction-table-cell-l", style = "padding: 0;",
                                                        
                                                        actionButton("ab_introduction_to_global_subsidies",
                                                                     tags$h4(text$item_label[text$item_id == "ab_introduction_to_global_subsidies"], icon("caret-right"), style = "color: black; font-weight: bold;"))
                                                ),
                                                
                                                tags$td(id = "introduction-table-cell-r", style = "padding: 0;",
                                                        
                                                        actionButton("ab_introduction_to_methods_process",
                                                                     tags$h4(text$item_label[text$item_id == "ab_introduction_to_methods_process"], icon("caret-right"), style = "color: black; font-weight: bold;"))
                                                )
                                        )
                             )

                      )
                      
             ) # /div picture-overlay-black
             
    ) # /div landing-wrapper
    
  ) # /fluidPage
  