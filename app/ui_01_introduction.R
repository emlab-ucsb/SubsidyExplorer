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
                               
                               tags$h3("An ambitious agreement on fisheries subsidy reform at the WTO could result in increases of up to"),
                               tags$h1("XX% in global fish biomass"),
                               tags$h4("and"),
                               tags$h1("XX% in global fish catch"),
                               
                               tags$h3("Subsidy reform represents one of the most beneficial actions we can take to restore our world’s oceans.")
                               
                               
                      ),
                      
                      # Buttons
                      
                      tags$div(class = "picture-overlay-button-row",
                             
                             tags$div(class = "picture-overlay-button-wrapper-l",
                                      
                                      actionButton("ab_selected_results",
                                                   tags$h4("Explore the Results"))

                             ),
                             
                             tags$div(class = "picture-overlay-button-wrapper-c",
                                      
                                      actionButton("ab_methods_process",
                                                   tags$h4("Learn about the Methods and Process"))
                                      
                             ),
                             
                             tags$div(class = "picture-overlay-button-wrapper-r",
                                      
                                      actionButton("ab_global_subsidies",
                                                   tags$h4("More About Fisheries Subsidies"))
                                      
                             )

                      ),
                      
                      # Footer
                      tags$div(class = "picture-overlay-footer-row",

                               tags$div(class = "picture-overlay-footer-wrapper",

                                        actionLink("al_need_help",
                                                   tags$h4("Need Help?"))

                               )

                      ) # /div picture-overlay-row
                      
             ) # /div picture-overlay-black
             
    ) # /div landing-wrapper
    
  ) # /fluidPage
  
  
  #column(12, style = "color: white; text-align: center; display: flex; justify-content: center; align-items: center; width: 100%;",
  # 
  # tags$div(class = "picture-overlay-sub-text-wrapper",
  #          
  #          fluidRow(
  #            
  #            # column(5, style = "padding: 5%; margin: 4%; background-color: rgba(50,51,55,0.8);",
  #            #      
  #            #      tags$h4("I am already familiar with fisheries subsidies"),
  #            #      
  #            #      fluidRow(
  #            
  #            # First button
  #            column(4, style = "padding: 20px 15px;",
  #                   
  #                   actionButton("ab-selected-results",
  #                                "Explore the results in more detail",
  #                                style = "color: #fff; background-color: #3c8dbc; border-color: #2e6da4; padding: 15px 10px; width:100%; white-space: normal;")
  #                   
  #            ),
  #            
  #            # Second button
  #            column(4, style = "padding: 20px 15px;",
  #                   
  #                   actionButton("ab-more-information",
  #                                "Learn about the methods and process",
  #                                style = "color: #fff; background-color: #3c8dbc; border-color: #2e6da4; padding: 15px 10px; width:100%; white-space: normal;")
  #                   
  #            ),
  #            
  #            #        )
  #            #          
  #            # ),
  #            
  #            # column(5, style = "padding: 5%; margin:4%; background-color: rgba(50,51,55,0.8);",
  #            #        
  #            #        tags$h4("I would like to learn more about fisheries subsidies"),
  #            #        
  #            #        fluidRow(
  #            
  #            # Third button
  #            column(4, style = "padding: 20px 15px;",
  #                   
  #                   actionButton("ab-global-subsidies",
  #                                "Learn about fisheries subsidies",
  #                                style = "color: #fff; background-color: #3c8dbc; border-color: #2e6da4; padding: 15px 10px; width:100%; white-space: normal;")
  #                   
  #            )
  #            
  #            #   # Second Yes button
  #            #   column(6, style = "padding: 20px 15px;",
  #            #          
  #            #          actionButton("ab-get-help",
  #            #                       "Get help on how to use this tool",
  #            #                       style = "color: #fff; background-color: #3c8dbc; border-color: #2e6da4; padding: 15px 10px; width:100%; white-space: normal;")
  #            #          
  #            #   )
  #            # )
  #            
  #            
  #            #)
  #            
  #          )
  # )
             
