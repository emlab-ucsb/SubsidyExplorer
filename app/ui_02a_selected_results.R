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
    
    ### Navigation buttons
    column(12,
           fluidRow(style = "padding: 5px 5px; background-color: #3c8dbc;",
                    
                    # Next to edit-policies
                    column(3, offset = 9,
                           tags$button(id = "ab_selected_results_to_edit_policies",
                                       class = "btn action-button nav-button-r",
                                       button_text$text[button_text$id == "ab_selected_results_to_edit_policies"], icon("chevron-right")
                           )
                    )
                    
           )
    ),
    
    ### Title and Introductory Text
    column(12, style = "padding: 25px 25px;",
           
           # Title
           tags$h3(style = "padding: 0; margin: 0 0 10px;", tab_text$tab[tab_text$id == "selected-results"]),
           
           # Text
           includeHTML("./text/02a_selected_results_intro.html"),
           
           # Widgets
           fluidRow(
             column(6, align = "center",
                    
                    selectizeInput("timeseries_variable", label = tags$b("Plot... "),
                                   choices = list("Biomass" = "biomass", "Catches" = "catches_total", "Revenue" = "revenue_total"),
                                   selected = "biomass")
                    
             ),
             column(6, align = "center",
                    radioButtons("timeseries_resolution", label = tags$b("Resolution: "),
                                 choices = list("Global" = 1, "Regional" = 2), 
                                 selected = 1,
                                 inline = T)
                    
             )
           )
           
           # [NEED]
           
    ),
    
    ### Plot
    column(12, style = "padding: 0px 0px 25px;",
           
           # Figure placeholder
           img(src = "/sample-images/02a_plot_image.png", width = "100%")
           
    ),
    
    ### Bottom navigation buttons
    column(12,
           fluidRow(style = "padding: 5px 5px; background-color: #3c8dbc;",
                    
                    # Return to main menu
                    column(3,
                           tags$button(id = "ab_selected_results_to_introduction",
                                       class = "btn action-button nav-button-l",
                                       icon("undo"), button_text$text[button_text$id == "ab_selected_results_to_introduction"]
                           )
                    )
                    
           )
    )
            
    
    
  ) # /fluidPage
  
  
