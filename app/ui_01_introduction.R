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
    tags$div(class = "landing-wrapper",
             
             # Child element 1: background image
             tags$div(class = "picture-wrapper",
                      
                      tags$img(src = "intro-background.jpg")
             ),
                      
             # Child element 2: overlay
             tags$div(class = "picture-overlay-black",
                      
                      # Main Text
                      tags$div(class = "picture-overlay-main-text",
                               
                               includeHTML("./text/01_introduction_intro.html")
                               
                      ),
                      
                      # Buttons
                      
                      tags$div(class = "picture-overlay-button-row",
                             
                             tags$div(class = "picture-overlay-button-wrapper-l",
                                      
                                      actionButton("ab_introduction_to_selected_results",
                                                   tags$h4(text$item_label[text$item_id == "ab_introduction_to_selected_results"]))

                             ),
                             
                             tags$div(class = "picture-overlay-button-wrapper-c",
                                      
                                      actionButton("ab_introduction_to_methods_process",
                                                   tags$h4(text$item_label[text$item_id == "ab_introduction_to_methods_process"]))
                                      
                             ),
                             
                             tags$div(class = "picture-overlay-button-wrapper-r",
                                      
                                      actionButton("ab_introduction_to_global_subsidies",
                                                   tags$h4(text$item_label[text$item_id == "ab_introduction_to_global_subsidies"]))
                                      
                             )

                      ),
                      
                      # Footer
                      tags$div(class = "picture-overlay-footer-row",

                               tags$div(class = "picture-overlay-footer-wrapper",

                                        actionLink("al_introduction_to_need_help",
                                                   tags$h4(text$item_label[text$item_id == "al_introduction_to_need_help"]))

                               )

                      ) # /div picture-overlay-row
                      
             ) # /div picture-overlay-black
             
    ) # /div landing-wrapper
    
  ) # /fluidPage
  