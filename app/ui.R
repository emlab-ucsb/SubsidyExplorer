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

# Load packages
# General
library(shiny) # application widgets
library(shinyjs) # javascript functionality for shiny
library(shinydashboard) # dashboard theme
library(shinyBS)
library(rsconnect)
#library(purrr)
require(stats)
library(sf) # shapefiles
library(DT) # interactive data tables
library(tidyverse) # workhorse data manipulation

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
            
            # SFG logo
            tags$li(
                class = "dropdown",
                a(href = 'http://sfg.msi.ucsb.edu/',
                  img(src = 'sfg-logo-white.png', title = "The Sustainable Fisheries Group", height = "40px"), 
                  style = "padding-top:10px; padding-bottom:10px;"
                  ) # /a
            ), # /tags$li
            
            # emLab logo
            tags$li(
                class = "dropdown",
                a(href = 'http://emlab.msi.ucsb.edu/',
                  img(src = 'emlab_logo_horizontal_w.png', title = "The Environmental Market Solutions Lab", height = "40px"), 
                  style = "padding-top:10px; padding-bottom:10px;"
                ) # /a
            ) # /tags$li
                        
        ), # /dashboardHeader

        ### Sidebar menu ----------------------
        dashboardSidebar(
            
            # Width of sidebar menu
            width = "250px",
            
            # Want it collapsed by default
            collapsed = T,
            
            # Menu container
            sidebarMenu(
                
                # Variable name for selected menuItem
                id = "tabs",
                
                # Introduction
                menuItem("Introduction", 
                         tabName = "introduction", 
                         icon = NULL,
                         selected = TRUE),
                
                # Detailed results 
                menuItem("Explore Results",
                         icon = NULL,
                         
                         # Detailed results - Item #1 
                         menuSubItem('View Selected Results',
                                     tabName = 'selected-results',
                                     icon = NULL),
                                  
                         # Detailed results - Item #2
                         menuSubItem('Edit Policies (Advanced Users)',
                                     tabName = 'edit-policies',
                                     icon = NULL)
                         
                ), 
                
                # About Subsidies
                menuItem("Learn About Fisheries Subsidies", 
                         icon = NULL,
                         
                         menuItem('Global fisheries subsidies',
                                     tabName = 'global-subsidies',
                                     icon = NULL),
                         menuItem('Fishery profiles by state',
                                     tabName = 'country-profiles',
                                     icon = NULL),
                         menuItem('Compare fishery statistics',
                                     tabName = 'compare-fishery-statistics',
                                     icon = NULL),
                         
                         menuItem('Global fishing footprint',
                                     tabName = 'global-fishing-footprint',
                                     icon = NULL)
                         
                ),
                
                # About Subsidies
                menuItem("More Information", 
                         icon = NULL,
                         tabName = "more-info"
                         
                ),
                
                # Help and Contact
                menuItem("Need Help?", 
                         icon = NULL,
                         tabName = "need-help"
                         
                )
                                     
            ) # /sidebarMenu
        ), #/dashboardSidebar
        
        ### Main panel content ----------------------
        dashboardBody(
            
            # Custom stylesheet
            tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "subsidy_explorer.css")),
            
            # Tabs
            tabItems(
                
                # Introduction
                tabItem(tabName = "introduction",
                        introduction()
                        
                ),
                
                # Detailed results 
                tabItem(tabName = "detailed-results",
                        #detailed_results()
                ),
                
                # Selected results
                tabItem(tabName = "selected-results",
                        #selected_results()
                )
                
            ) # /tabItems
            
        ) # /dashboardBody
                
    ) # /dashboardPage
) # /shinyUI
