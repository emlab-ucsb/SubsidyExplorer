### -------------------------------------------------------------------
### SubsidyExplorer
### An interactive toolkit to explore the tradeoffs across proposed fisheries subsidies disciplines
### 
### This script contains the content for the selected-results tab
### --------------------------------------------------------------------

SelectedResults = function() 
  fluidPage(
    
    # Page style
    style = "background-color: #ffffff; color: #000000;",
    
    ### Top navigation button
    column(12,
           fluidRow(style = "padding: 5px 5px; background-color: #3c8dbc;",
                    
                    # Return to main menu
                    column(3,
                           tags$button(id = "ab_selected_results_to_introduction",
                                       class = "btn action-button nav-button-l",
                                       icon("undo"), text$item_label[text$item_id == "ab_selected_results_to_introduction"]
                           )
                    )
                    
           )
    ),
    
    ### Title and Introductory Text
    column(12, style = "padding: 25px 25px;",
           
           # Title
           tags$h3(style = "text-align: left; padding: 0; margin: 0 0 10px;", text$item_label[text$item_id == "selected-results"]),
           
           # Text
           includeHTML("./text/02a_selected_results_intro.html"),
           
           # Widgets
           fluidRow(
             
             column(6, align = "center",
                    
                    # Input - timeseries plot variable
                    selectizeInput("w_selected_results_timeseries_plot_variable", 
                                   label = tags$b(text$item_label[text$item_id == "w_selected_results_timeseries_plot_variable"]),
                                   choices = unlist(wid$choices[wid$item_id == "w_selected_results_timeseries_plot_variable"]),
                                   selected = unlist(wid$selected[wid$item_id == "w_selected_results_timeseries_plot_variable"]))
                    
             ),
             
             column(6, align = "center",
                    
                    # Input - timeseries plot resolution
                    radioButtons("w_selected_results_timeseries_plot_resolution", 
                                 label = tags$b(text$item_label[text$item_id == "w_selected_results_timeseries_plot_resolution"]),
                                 choices = unlist(wid$choices[wid$item_id == "w_selected_results_timeseries_plot_resolution"]),
                                 selected = unlist(wid$selected[wid$item_id == "w_selected_results_timeseries_plot_resolution"]),
                                 inline = T)
                                 
                    
             )
           )
           
    ),
    
    ### Plot
    column(12, style = "padding: 0px 0px 25px;",
           
           # Figure placeholder
           img(src = "/sample-images/02a_plot_image.png", width = "100%")
           
    ),
    
    ### Bottom navigation buttons
    column(12,
           fluidRow(style = "padding: 5px 5px; background-color: #3c8dbc;",
                    
                    # Next to edit-policies
                    column(3, offset = 9,
                           tags$button(id = "ab_selected_results_to_edit_policies",
                                       class = "btn action-button nav-button-r",
                                       text$item_label[text$item_id == "ab_selected_results_to_edit_policies"], icon("chevron-right")
                           )
                    )
                    
           )
    )
            
  ) # /fluidPage
  
  
