### -------------------------------------------------------------------
### SubsidyExplorer
### An interactive toolkit to explore the tradeoffs across proposed fisheries subsidies disciplines
### 
### This script contains the content for the "Introduction" tab
### --------------------------------------------------------------------

introduction = function() 
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
                      
                      # Sub Text (if needed) with buttons
                      
                      tags$div(class = "picture-overlay-sub-text-wrapper",
                             
                             tags$div(class = "picture-overlay-button-wrapper-l",
                                      
                                      actionButton("ab-selected-results",
                                                   tags$h4("Explore the Results"))

                             ),
                             
                             tags$div(class = "picture-overlay-button-wrapper-c",
                                      
                                      actionButton("ab-more-information",
                                                   tags$h4("Learn about the Methods and Process"))
                                      
                             ),
                             
                             tags$div(class = "picture-overlay-button-wrapper-r",
                                      
                                      actionButton("ab-global-subsidies",
                                                   tags$h4("More About Fisheries Subsidies"))
                                      
                             )

                      )
             )
    ),
    
    ### Footer 
    
    column(12, align = "center", style = "padding: 40px;",
           
           actionLink("al-need-help",
                      tags$h4("Need help?"),
                      style = "text-size: 200%;")
           
           )
     
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
             
