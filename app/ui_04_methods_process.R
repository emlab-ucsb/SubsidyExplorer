### -------------------------------------------------------------------
### SubsidyExplorer
### An interactive toolkit to explore the tradeoffs across proposed fisheries subsidies disciplines
### 
### This script contains the content for the methods-process tab
### --------------------------------------------------------------------

MethodsProcess = function() 
  fluidPage(
    
    # Page style
    style = "background-color: #ffffff; color: #000000;",
    
    ### Page Header
    column(12, id = "page-title-div-underline-blue",
           
           # Title
           tags$h3(text$item_label[text$item_id == "methods-process"])
           
    ),
    
    ### Intro and purpose
    column(12, id = "lr-spaced-div",
           
           # Text
           includeHTML("./text/04-methods-process/intro_and_purpose.html")
           
    ),
    
    ### Methods download button
    column(12, style = "text-align: center;", id = "tb-spaced-div",
                  
           downloadButton("db_download_methods", 
                          text$item_label[text$item_id == "db_download_methods"])
                  
    ),
    
    ### Contact us
    column(12, id = "spaced-div", style = "margin-bottom: 40px;",
           
           # More text
           includeHTML("./text/04-methods-process/contact_us.html")
           
    )
    
  ) # /fluidPage
  
  
