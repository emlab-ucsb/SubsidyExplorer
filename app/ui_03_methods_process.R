### -------------------------------------------------------------------
### SubsidyExplorer
### An interactive toolkit to explore the tradeoffs across proposed fisheries subsidies disciplines
### 
### This script contains the content for the methods-process tab
### --------------------------------------------------------------------

MethodsProcess = function() 
  fluidPage(
    
    # Page style
    style = "background-color: #262626; color: #ffffff; padding-bottom: 40px; border-bottom: 10px solid #3c8dbc;",
    
    ### Navigation buttons
    column(12,
           fluidRow(style = "padding: 5px 5px; background-color: #3c8dbc;",
                    
                    # Back to main menu button
                    column(3,
                           tags$button(id = "ab_methods_process_to_introduction",
                                       class = "btn action-button nav-button-l",
                                       icon("chevron-left"), button_text$text[button_text$id == "ab_methods_process_to_introduction"]
                           )
                    )
                    
           )
    ),
    
    ### Title and introductory text 
    column(12, style = "padding: 25px 25px;",
           
           # Title
           tags$h3(style = "padding: 0; margin: 0 0 10px;", tab_text$tab[tab_text$id == "methods-process"]),
           
           # Text
           includeHTML("./text/03_methods_process_intro.html"),
           
           # Download button for methods
           column(12, style = "text-align: center;",
                  
                  downloadButton("db_download_methods", 
                                 style = "padding: 10px; width: auto;", 
                                 button_text$text[button_text$id == "db_download_methods"])
                  
           )
           
    )
            
    
    
  ) # /fluidPage
  
  
