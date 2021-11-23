# Read in required libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(caret)
library(readr)
library(ggplot2)

# Read in data set
cbb20 <- read_csv(file = "cbb20.csv") 


# Define UI for application that draws a histogram
ui <- dashboardPage(
    # Add title
    dashboardHeader(title = "2020 NCAA Division I Basketball Stats", titleWidth = 1000),
    # Define tabs
    dashboardSidebar(sidebarMenu(
        menuItem("About", tabName = "about", icon = icon("archive")),
        menuItem("Data Exploration", tabName = "data exploration", icon = icon("laptop")),
        menuItem("Modeling", tabName = "modeling", icon = icon("laptop")),
        menuItem("Data", tabName = "data", icon = icon("laptop"))
    )),
    #Define the body of the app
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "about",
                    fluidRow(
                        img(src = "ncaabasketball4.jpg", height = 300, width = 1000),
                        #add in latex functionality if needed
                        withMathJax(),
                        #two columns for each of the two items
                        column(6,
                               #Description of App
                               h1("App Purpose", style="color:#005eb8"),
                               #box to contain description
                               box(background="navy",width=12,
                                   h4("The purpose of this app is to describe, summarize, and predict data from the NCAA DI basketball 2020 season. This app allows users to look at and customize various plots and tables of this data to see relationships between teams, conferences, wins/loses, and a team's statistics. This app also helps to predict a team's final ranking based off several statistical predictors.")
                               )
                        ),
                        
                        column(6,
                               #Data description
                               h1("Data Description", style="color:#005eb8"),
                               #box to contain description
                               box(background="navy",width=12,
                                   h4("This data was collected from a college basketball data set on kaggle found", a("here", href = "https://www.kaggle.com/andrewsundberg/college-basketball-dataset"),"."),
                                   h4(a("Kaggle", href = "https://www.kaggle.com/"), "is a huge repository full of community published data."),
                                   h4("The data used in this app is information about NCAA Division I men's basketball. It covers years from 2013-2021. It lists team names, conferences, win/loses, and overall team statistics throughout the season.")
                               )
                        ),
                        column(6,
                               #Purpose of each tab
                               h1("Purpose of Each Page", style="color:#005eb8"),
                               #box to contain description
                               box(background="navy",width=12,
                                   h4(strong("-Data Exploration"), ": this page allows users to customize numerical and graphical summeries involving the data."),
                                   h4(strong("-Modeling"), ": this page allows customization of three different types of supervised learning models. This page also includes three extra tabs: Modeling Info (describes the types of models being used), Model Fitting (enables user customizations for the different types of models), and Prediction (allows users to use one of the models to predict the response)."),
                                   h4(strong("-Data"), ": this page allows users to scroll through the dataset, subset the data, and download it.")
                               )
                        )
                    )
            )
    )
    )
)



server <- function(input, output) { }

shinyApp(ui, server)

