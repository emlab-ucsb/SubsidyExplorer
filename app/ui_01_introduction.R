### -------------------------------------------------------------------
### SubsidyExplorer
### An interactive toolkit to explore the tradeoffs across proposed fisheries subsidies disciplines
### 
### This script contains the content for the "Introduction" tab
### --------------------------------------------------------------------

introduction = function() 
  fluidPage(
    
    # page style
    style = "color: #ffffff; padding-bottom: 40px; border-bottom: 10px solid #3c8dbc;",
            
    # open links in new window
    tags$head(tags$base(target = "_blank")),
    
    ### Image with text -------------------

    # Parent container: landing page header (photo + overlay)
    tags$div(class = "landing-wrapper", style = "border-bottom: 4px solid #3c8dbc;",
             
             # Child element 1: background image
             tags$div(class = "landing-block background-content",
                      
                      tags$img(src = "intro-background.jpg"),
                      
             ), 
             
             # Child element 2: overlay
             tags$div(class = "landing-block foreground-content",
                      
                      # Text
                      tags$div(class = "foreground-text",
                               
                               tags$h3("An ambitious agreement on fisheries subsidy reform at the WTO could result in increases of up to"),
                               tags$h1("XX% in global fish biomass"),
                               tags$h4("and"),
                               tags$h1("XX% in global fish catch"),
                               
                               tags$h3("Subsidy reform represents one of the most beneficial actions we can take to restore our world’s oceans.")
                               
                      )
             ) # /div landing-block foreground-content
             
    ), # /div landing-wrapper
    
    ### Buttons -------------------
    fluidRow(

        column(12,
               align = "center",

               tags$h4("Are you familiar with fisheries subsidies?")

        ),

        column(12,
               align = "center",

               # Yes column
               column(6,
                      tags$h4("Yes. I want to..."),

                      column(5, offset = 1,
                             actionButton("ab-selected-results",
                                          "Explore the results in more detail",
                                          style = "color: #fff; background-color: #3c8dbc; border-color: #2e6da4; padding: 15px 10px; width:80%;")
                      ),

                      column(5,
                             actionButton("ab-more-information",
                                          "Learn more about the methods and process",
                                          style="color: #fff; background-color: #3c8dbc; border-color: #2e6da4; padding: 15px 10px; width:80%;")

                      )

               ), # /column

               # No column
               column(6,
                      tags$h4("No. Take me to..."),

                      column(6,
                             actionButton("ab-global-subsidies",
                                          "Learn about fisheries subsidies",
                                          style = "color: #fff; background-color: #3c8dbc; border-color: #2e6da4; padding: 15px 10px; width:80%;")
                      ),

                      column(6,
                             actionButton("ab-need-help",
                                          "Get help",
                                          style="color: #fff; background-color: #3c8dbc; border-color: #2e6da4; padding: 15px 10px; width:80%;")

                      )

               ) # /column

        ) # /column

      ) # /fluidRow
    
  ) # /fluidPage
             
