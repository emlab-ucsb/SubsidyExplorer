### -------------------------------------------------------------------
### SubsidyExplorer
### An interactive toolkit to explore the tradeoffs across proposed fisheries subsidies disciplines
### 
### This script contains the content for the compare-fishery-stats tab
### --------------------------------------------------------------------

CompareFisheryStats = function(wto_members_and_observers, subsidy_types_sorted_sumaila) 
  fluidPage(
    
    # Page style
    style = "background-color: #ffffff; color: #000000;",
    
    ### Title and introductory text                                                  
    column(12, style = "padding: 25px 25px 0px;",
           
           # Title
           tags$h3(style = "text-align: left; padding: 0; margin: 0;", text$item_label[text$item_id == "compare-fishery-stats"]),
           
           #Text
           includeHTML("./text/04c_compare_fishery_stats_intro.html")
           
    ), # /column 12
    
    ### Widgets
    column(12, style = "padding: 0px 25px 10px;",
           
           fluidRow(
             
             # Input - select WTO Member or Observer
             column(6, style = "padding: 0 5px 0 0px;",
                    
                    selectizeInput("w_compare_fishery_stats_selected_country",
                                   label = text$item_label[text$item_id == "w_compare_fishery_stats_selected_country"],
                                   choices = wto_members_and_observers,
                                   selected = NULL,
                                   width = "100%",
                                   options = list(placeholder = 'Select...'))
                    
             ), # /column 6
             
             column(6, style = "padding: 0 0px 0 5px;",
                    
                    # Conditional panel - country selected
                    conditionalPanel(condition = "input.w_compare_fishery_stats_selected_country",
                                     
                                     # Input - select statistic to compare
                                     selectizeInput("w_compare_fishery_stats_plot_variable",
                                                    label = text$item_label[text$item_id == "w_compare_fishery_stats_plot_variable"],
                                                    choices = unlist(wid$choices[wid$item_id == "w_compare_fishery_stats_plot_variable"]),
                                                    selected = unlist(wid$selected[wid$item_id == "w_compare_fishery_stats_plot_variable"]),
                                                    width = "100%")
                                     
                    ) # /conditionalPanel - country selected
             ) # /column 6
             
           ), # /fluidRow
           
           fluidRow(
             
             column(6, style = "padding: 0 5px 0 0;",
                    
                    # Input - Select type of comparison
                    selectizeInput("w_compare_fishery_stats_method",
                                   label = text$item_label[text$item_id == "w_compare_fishery_stats_method"],
                                   choices = unlist(wid$choices[wid$item_id == "w_compare_fishery_stats_method"]),
                                   selected = unlist(wid$selected[wid$item_id == "w_compare_fishery_stats_method"]),
                                   width = "100%")
             ),
             
             column(6, style = "padding: 0 0 0 5px;",
                    
                    # Conditional panel - comparison method is to countries of choice
                    conditionalPanel(condition = "input.w_compare_fishery_stats_method == 'select'",
                                     
                                     # Input: Select states with which to compare
                                     selectizeInput("w_compare_fishery_stats_select_manual",
                                       label = text$item_label[text$item_id == "w_compare_fishery_stats_select_manual"],
                                       choices = wto_members_and_observers,
                                       selected = NULL,
                                       width = "100%",
                                       options = list(placeholder = 'Select...',
                                                      maxItems = 10),
                                       multiple = T
                                     )
                                     
                    ) # /conditionalPanel - comparison method is to countries of choice
             ) # /column 6
           ), # /fluidRow
           
           # fluidRow(
           #   
           #   # Explanatory text for plot
           #   uiOutput("compare_intro_text", container = tags$p)
           #   
           # ),
           
           fluidRow(
             
             # Conditional Panel - statistic involves subsidies
             conditionalPanel(condition = "input.w_compare_fishery_stats_plot_variable != 'landings' && input.w_compare_fishery_stats_plot_variable != 'revenue'",
                              
                              # Input - Select subsidy type(s) to include
                              selectizeInput("w_compare_fishery_stats_subsidy_types",
                                             label = tagList(tags$b(text$item_label[text$item_id == "w_compare_fishery_stats_subsidy_types"]),
                                                             # Info button
                                                             tags$button(id = "info_subsidy_type_to_plot",
                                                                         class = "btn action-button info-button",
                                                                         icon("info"))),
                                             
                                             choices = subsidy_types_sorted_sumaila,
                                             selected = subsidy_types_sorted_sumaila,
                                             width = "100%",
                                             multiple = T)
                              
                              
             ) # /conditionalPanel - statistic inlvoes subsidies
           ) # /fluidRow
           
    ), # /column 12 - widgets
    
    ### Bar plot 
    column(12, style = "padding: 0 25px 15px;",
           
           # Bar chart
           plotlyOutput("compare_fishery_stats_bar_plot", width = "100%")
           
           
    )
            
  ) # /fluidPage
  
  
