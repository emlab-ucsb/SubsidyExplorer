### -------------------------------------------------------------------
### SubsidyExplorer
### An interactive toolkit to explore the tradeoffs across proposed fisheries subsidies disciplines
### 
### Creator(s): Kat Millage and Vienna Saccomanno
### Release date (v1): July 2019
### Release date (v2): 
### 
### This is the script outlining the user interface for the application
### User interface details for each page are specified in their own individual files
### --------------------------------------------------------------------

### ----------------------------------
### Initialize app -------------------
### ----------------------------------

rm(list = ls())
set.seed(123)

dir.create('~/.fonts')
file.copy("www/Avenir-Book.ttf", "~/.fonts")
system('fc-cache -f ~/.fonts')

# Load packages
# General
library(shiny) # shiny app widgets
library(shinyjs) # javascript functionality for shiny
library(shinydashboard) # dashboard theme for shiny
library(shinyWidgets) # fancy widgets
library(shinyalert) # popups
library(shinyBS) # tooltips and hover
library(rsconnect) # needed to deploy app to shinyapps.io
require(stats) # statistical functions
library(sf) # shapefiles
library(DT) # interactive data tables
library(countrycode)
library(tidyverse) # workhorse data manipulation

# Plotting
library(leaflet) # interactive maps 
library(plotly) # interactive charts
library(RColorBrewer) # other color scales

# Silence new dplyr grouping messages
options(dplyr.summarise.inform=F)

# The content for each tab is stored in a separate file. Source all .R files in the current directory that start with "ui_":  
sapply(list.files(
    pattern = "^ui_.*\\.R$",
    path = ".",
    full.names = TRUE
),
source)

# Functions needed for running the bioeconomic model are stored in separate files. Source all .R files in ./scripts/: 
sapply(list.files(
    pattern = "[.]R$",
    path = "./scripts/",
    full.names = TRUE
),
source)

# Source text and data needed for app
source("00_initialize_app.R")

### -----------------------------------
### User Interface (UI) ---------------
### -----------------------------------

# Define UI structure for application 
shinyUI(
    
    dashboardPage(
    
        ### Header ---------------------
        dashboardHeader(
            
            # Title - none for this application
            title = "",
            
            # Title width - none for this application
            titleWidth = "0%",
            
            # Title
            tags$li(
                id = "custom-title-container",
                class = "dropdown",
                tags$h3(text$item_label[text$item_id == "app-title"]) # /h3
            ), # /tags$li
            
            # SFG logo
            tags$li(
                class = "dropdown",
                a(href = 'http://sfg.msi.ucsb.edu/',
                  img(src = 'sfg-logo-white.png', title = text$item_label[text$item_id == "sfg_logo"], height = "40px"), 
                  style = "padding-top:10px; padding-bottom:10px;"
                  ) # /a
            ), # /tags$li
            
            # emLab logo
            tags$li(
                class = "dropdown",
                a(href = 'http://emlab.msi.ucsb.edu/',
                  img(src = 'emlab_logo_horizontal_w.png', title = text$item_label[text$item_id == "emlab_logo"], height = "40px"), 
                  style = "padding-top:10px; padding-bottom:10px;"
                ) # /a
            ) # /tags$li
                        
        ), # /dashboardHeader

        ### Sidebar menu ----------------------
        dashboardSidebar(
            
            # Width of sidebar menu
            width = "320px",
            
            # Want it collapsed by default
            collapsed = T,
            
            # Want it disabled by default
            disable = F,
            
            # Menu container
            sidebarMenu(
                
                # Variable name for selected menuItem
                id = "menu_items",
                
                ### Introduction ---
                menuItem(text = text$item_label[text$item_id == "introduction"],
                         tabName = "introduction", 
                         icon = NULL,
                         selected = TRUE),
                
                ### Explore results ---
                menuItem(text = text$item_label[text$item_id == "ab_introduction_to_explore_results"],
                         icon = NULL,
                         
                         # Explore results - Item # 1 - Explore the Results
                         menuSubItem(text$item_label[text$item_id == "explore-results"],
                                     tabName = 'explore-results',
                                     icon = NULL),
                         
                         # Explore results - Item #3 - Custom Policy
                         menuSubItem(text$item_label[text$item_id == "edit-policies"],
                                     tabName = 'edit-policies',
                                     icon = NULL)
                         
                ), 
                
                ### About fisheries subsidies ---
                menuItem(text = text$item_label[text$item_id == "ab_introduction_to_global_subsidies"],
                         icon = NULL,
                         
                         # About fisheries subsidies - Item #1 - Global subsidy map
                         menuItem(actionLink("al_global_subsidies", text$item_label[text$item_id == "global-subsidies"]),
                                  tabName = 'global-subsidies',
                                  icon = NULL),
                         
                         # About fisheries subsidies - Item #2 - Fishery statistics by state
                         menuItem(actionLink("al_country_fishery_stats", text$item_label[text$item_id == "country-fishery-stats"]),
                                  tabName = 'country-fishery-stats',
                                  icon = NULL),
                         
                         # About fisheries subsidies - Item #3 - Compare fishery statistics
                         menuItem(actionLink("al_compare_fishery_stats", text$item_label[text$item_id == "compare-fishery-stats"]),
                                  tabName = 'compare-fishery-stats',
                                  icon = NULL),
                         
                         # About fisheries subsidies - Item #4 - Global effort map
                         menuItem(actionLink("al_global_fishing_footprint", text$item_label[text$item_id == "global-fishing-footprint"]),
                                  tabName = 'global-fishing-footprint',
                                  icon = NULL)
                         
                ),
                
                ### About methods and process ---
                menuItem(text$item_label[text$item_id == "methods-process"],
                         icon = NULL,
                         tabName = "methods-process"
                         
                )
        
                                     
            ) # /sidebarMenu
        ), #/dashboardSidebar
        
        ### Main panel content ----------------------
        dashboardBody(
            
            # Custom stylesheet
            tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "subsidy_explorer.css")),
            
            # Allow for popups
            useShinyalert(),
            
            # Allow js functionality
            useShinyjs(),
            
            # Tabs
            tabItems(
                
                ### Introduction ---
                tabItem(tabName = "introduction",
                        Introduction()
                ),
                
                ### Explore results ---
                # Explore results - Item #1 - Explore results
                tabItem(tabName = "explore-results",
                        ExploreResults(proposal_choices)
                ),
                
                # Explore results - Item #3 - Edit policies and view those results
                tabItem(tabName = "edit-policies",
                        EditPolicies(wto_members_and_observers, subsidy_types_sorted_sumaila)
                ),
              
                
                ### More about fisheries subsidies ---
                
                # About fisheries subsidies - Item #1 - Global subsidy map
                tabItem(tabName = "global-subsidies",
                        
                        MoreAboutSubsidies(subsidy_types_sorted_sumaila, wto_members_and_observers)
                ),
                
                ### About methods and process ---
                tabItem(tabName = "methods-process",
                        MethodsProcess()
                )

            ) # /tabItems
            
        ) # /dashboardBody
                
    ) # /dashboardPage
) # /shinyUI
