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
           tags$h4(text$item_label[text$item_id == "methods-process"])
           
    ),
    
    ### Intro and purpose
    column(12, id = "spaced-div",
           
           # Text
           includeHTML("./text/04-methods-process/intro_and_purpose.html")
           
    ),
    
    ### Methods download button
    column(12, style = "text-align: center;", id = "tb-spaced-div",
                  
           downloadButton("db_download_methods", 
                          text$item_label[text$item_id == "db_download_methods"])
                  
    ),
    
    ### Contact us
    column(12, id = "spaced-div",
           
           # More text
           includeHTML("./text/04-methods-process/contact_us.html")
           
    ),
    
    ### Logos
    column(12, id = "spaced-div", align = "center", style = "padding-bottom: 40px;",
           
           tags$table(id = "methods-process-table",
                      
                      tags$tr(id = "methods-process-table-row",
                              
                              tags$td(id = "methods-process-table-cell-1",
                                      
                                      tags$image(src = "pew_logo.jpg",
                                                 style = "height: 60px;")
                                      
                              ),
                              
                              tags$td(id = "methods-process-table-cell-2",
                                      
                                      tags$image(src = "ubc_logo.png",
                                                 style = "height: 60px;")
                                      
                              ),
                              
                              tags$td(id = "methods-process-table-cell-3",
                                      
                                      tags$image(src = "gfw_logo.png",
                                                 style = "height: 60px;")
                                      
                              )
                      )
           )
           
    )
    
  ) # /fluidPage
  
  
