### -------------------------------------------------------------------
### SubsidyExplorer
### An interactive toolkit to explore the tradeoffs across proposed fisheries subsidies disciplines
### 
### This script contains the content for the "Introduction" tab
### --------------------------------------------------------------------

introduction = function() 
  fluidPage(
    
    # page style
    style = "color: #ffffff; border-bottom: 10px solid #3c8dbc;",
            
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
                      
                      # Sub Text with buttons
                      column(12, style = "color: white; text-align: center; display: flex; justify-content: center; align-items: center; width: 100%;",
                             
                             fluidRow(
                               
                                 column(5, style = "padding: 5%; margin: 4%; background-color: rgba(50,51,55,0.8);",
                                      
                                      tags$h4("I am already familiar with fisheries subsidies"),
                                      
                                      fluidRow(
                                        
                                        # First Yes button
                                        column(6, style = "padding: 20px 15px;",
                                               
                                               actionButton("ab-selected-results",
                                                            "Explore the results in more detail",
                                                            style = "color: #fff; background-color: #3c8dbc; border-color: #2e6da4; padding: 15px 10px; width:100%; white-space: normal;")
                                               
                                        ),
                                    
                                        # Second Yes button
                                        column(6, style = "padding: 20px 15px;",
                                               
                                               actionButton("ab-more-information",
                                                            "Learn about the methods and process",
                                                            style = "color: #fff; background-color: #3c8dbc; border-color: #2e6da4; padding: 15px 10px; width:100%; white-space: normal;")
                                               
                                        )
                                        
                                      )
                                        
                               ),
                               
                               column(5, style = "padding: 5%; margin:4%; background-color: rgba(50,51,55,0.8);",
                                      
                                      tags$h4("I would like to learn more about fisheries subsidies"),
                                      
                                      fluidRow(
                                        # First Yes button
                                        column(6, style = "padding: 20px 15px;",
                                               
                                               actionButton("ab-global-subsidies",
                                                            "Learn about fisheries subsidies",
                                                            style = "color: #fff; background-color: #3c8dbc; border-color: #2e6da4; padding: 15px 10px; width:100%; white-space: normal;")
                                               
                                        ),
                                      
                                        # Second Yes button
                                        column(6, style = "padding: 20px 15px;",
                                               
                                               actionButton("ab-get-help",
                                                            "Get help on how to use this tool",
                                                            style = "color: #fff; background-color: #3c8dbc; border-color: #2e6da4; padding: 15px 10px; width:100%; white-space: normal;")
                                               
                                        )
                                      )
                                        
                                      
                               )
                               
                             )
                      )
                                      
                                      
                                      
                                      
                      #                 )
                      # tags$div(class = "picture-overlay-sub-text-wrapper",
                      #   
                      #   # Left box
                      #   tags$div(class = "picture-overlay-sub-text",
                      #            
                      #            tags$h4("I'm already familiar with fisheries subsidies"),
                      #            
                      #            fluidRow(
                      #              # First Yes button
                      #              column(6,
                      #                     
                      #                     actionButton("ab-selected-results",
                      #                                  "Explore the results in more detail",
                      #                                  style = "color: #fff; background-color: #3c8dbc; border-color: #2e6da4; padding: 15px 10px; width:100%; height:100px;")
                      #                     
                      #              ),
                      #              # Second Yes button
                      #              column(6,
                      #                     
                      #                     actionButton("ab-more-information",
                      #                                  "Learn about the methods and process",
                      #                                  style = "color: #fff; background-color: #3c8dbc; border-color: #2e6da4; padding: 15px 10px; width:100%; height:100px;")
                      #                     
                      #              )
                      #              
                      #            ) # /fluidRow
                      #            
                      #   ),
                      #   
                      #   # Right box
                      #   tags$div(class = "picture-overlay-sub-text",
                      #            
                      #            tags$h4("I would like to learn more about fisheries subsidies"),
                      #            
                      #            fluidRow(
                      #              # First No button
                      #              column(6,
                      #                     
                      #                     actionButton("ab-selected-results2",
                      #                                  "Explore the results in more detail",
                      #                                  style = "color: #fff; background-color: #3c8dbc; border-color: #2e6da4; padding: 15px 10px; width:auto; height:100px;")
                      #                     
                      #              ),
                      #              # Second No button
                      #              column(6,
                      #                     
                      #                     actionButton("ab-more-information2",
                      #                                  "Learn more about the methods and process",
                      #                                  style = "color: #fff; background-color: #3c8dbc; border-color: #2e6da4; padding: 15px 10px; width:auto; height:100px;")
                      #                     
                      #              )
                      #              
                      #            ) # /fluidRow
                      #            
                      #   )
                      #   
                      # ) # /div picture-overlay-sub-text-wrapper
                                 
             ) # /div picture-overlay-black
             
    ) # /div landing-wrapper
    
    ### Buttons -------------------
    # fluidRow(
    # 
    #     column(12,
    #            align = "center",
    # 
    #            tags$h4("Are you familiar with fisheries subsidies?")
    # 
    #     ),
    # 
    #     column(12,
    #            align = "center",
    # 
    #            # Yes column
    #            column(6,
    #                   tags$h4("Yes. I want to..."),
    # 
    #                   
    # 
    
    # 
    #            ), # /column
    # 
    #            # No column
    #            column(6,
    #                   tags$h4("No. Take me to..."),
    # 
    #                   column(6,
    #                          actionButton("ab-global-subsidies",
    #                                       "Learn about fisheries subsidies",
    #                                       style = "color: #fff; background-color: #3c8dbc; border-color: #2e6da4; padding: 15px 10px; width:80%;")
    #                   ),
    # 
    #                   column(6,
    #                          actionButton("ab-need-help",
    #                                       "Get help",
    #                                       style="color: #fff; background-color: #3c8dbc; border-color: #2e6da4; padding: 15px 10px; width:80%;")
    # 
    #                   )
    # 
    #            ) # /column
    # 
    #     ) # /column
    # 
    #   ) # /fluidRow
    
  ) # /fluidPage
             
