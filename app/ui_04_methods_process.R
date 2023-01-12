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
    
    ### Methods download buttons
    fluidRow(
      
      # English
      column(4, style = "text-align: right;", id = "tb-spaced-div",
             
             downloadButton("db_download_methods", 
                            text$item_label[text$item_id == "db_download_methods"])
             
      ),
      # Spanish
      column(4, style = "text-align: center;", id = "tb-spaced-div",
             
             downloadButton("db_download_methods_es", 
                            text$item_label[text$item_id == "db_download_methods_es"])
             
      ),
      # French
      column(4, style = "text-align: left;", id = "tb-spaced-div",
             
             downloadButton("db_download_methods_fr", 
                            text$item_label[text$item_id == "db_download_methods_fr"])
             
      )
    ),
    
    ### FAQs
    column(12, id = "spaced-div",
           
           # Text
           includeHTML("./text/04-methods-process/faqs.html"),
           
           # one
           box(title = "How does the model work?",
               includeHTML("./text/04-methods-process/faq_three.html"),
               collapsible = T,
               collapsed = T,
               width = 12),
           
           # two
           box(title = "What are the main assumptions built into the model?",
               includeHTML("./text/04-methods-process/faq_one.html"),
               collapsible = T,
               collapsed = T,
               width = 12),
           
           # three
           box(title = "Where did the data come from for the model?",
               includeHTML("./text/04-methods-process/faq_two.html"),
               collapsible = T,
               collapsed = T,
               width = 12)

    ),
    
    ### Glossary
    column(12, id = "spaced-div",
           
           # Text
           includeHTML("./text/04-methods-process/glossary.html")
           
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
  
  
