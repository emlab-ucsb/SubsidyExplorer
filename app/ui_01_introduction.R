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
                      
                      tags$img(src = "fish-background.jpeg")
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
                                                        tags$h3(paste0("+ ", biomass_end_percent, "% in global fish biomass")),
                                                        tags$h3(paste0("+ ", catch_end_percent, "% in global fish catch")),
                                                        tags$h4(paste0("Resulting in ", biomass_end_value, " million more tons of fish in the water, and ", catch_end_value, " million more tons of fish being caught."))
                                                )
                                        )
                             )
                      ),
                      
                      ### Button 1
                      column(12, id = "tb-spaced-div",
                             
                             actionButton("ab_introduction_to_selected_results",
                                          tags$h3(text$item_label[text$item_id == "ab_introduction_to_selected_results"], icon("caret-right")))
                      ),
                      
                      ### Buttons 2-3
                      column(12, id = "tb-spaced-div", align = "center",
                             
                             tags$table(id = "introduction-table", style = "text-align: center;",
                                        
                                        tags$tr(id = "introduction-table-table-row",
                                                
                                                tags$td(id = "introduction-table-cell-l",
                                                        
                                                        actionButton("ab_introduction_to_global_subsidies",
                                                                     tags$h4(text$item_label[text$item_id == "ab_introduction_to_global_subsidies"], icon("caret-right")))
                                                ),
                                                
                                                tags$td(id = "introduction-table-cell-r",
                                                        
                                                        actionButton("ab_introduction_to_methods_process",
                                                                     tags$h4(text$item_label[text$item_id == "ab_introduction_to_methods_process"], "   ", icon("caret-right")))
                                                )
                                        )
                             )

                      )
                      
                      # # Footer
                      # tags$div(class = "picture-overlay-footer-row",
                      # 
                      #          tags$div(class = "picture-overlay-footer-wrapper",
                      # 
                      #                   actionLink("al_introduction_to_need_help",
                      #                              tags$h4(text$item_label[text$item_id == "al_introduction_to_need_help"]))
                      # 
                      #          )
                      # 
                      # ) # /div picture-overlay-row
                      
             ) # /div picture-overlay-black
             
    ) # /div landing-wrapper
    
  ) # /fluidPage
  