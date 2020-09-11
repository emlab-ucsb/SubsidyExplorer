### -------------------------------------------------------------------
### SubsidyExplorer
### An interactive toolkit to explore the tradeoffs across proposed fisheries subsidies disciplines
### 
### This script contains the content for the methods-process tab
### --------------------------------------------------------------------

MethodsProcess = function() 
  fluidPage(
    
    # Page style
    style = "background-color: #262626; color: #ffffff;",
    
    ### Title and introductory text 
    column(12, style = "padding: 25px 25px;",
           
           # Title
           tags$h3(style = "text-align: left; padding: 0; margin: 0 0 10px; color: #ffffff;", text$item_label[text$item_id == "methods-process"]),
           
           # Text
           includeHTML("./text/03_methods_process_intro.html"),
           
           # Download button for methods
           column(12, style = "text-align: center;",
                  
                  downloadButton("db_download_methods", 
                                 style = "padding: 10px; width: auto;", 
                                 text$item_label[text$item_id == "db_download_methods"])
                  
           )
           
    )
    
  ) # /fluidPage
  
  
