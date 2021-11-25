# Read in required libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(caret)
library(readr)
library(ggplot2)
library(DT)

# Read in data set
cbb <- read_csv(file = "cbb.csv") 
#create binary variable
cbbNew <- cbb %>% mutate(IN_TOURN =   if_else(is.na(cbb$POSTSEASON), 0, 1))
cbbNew



# Define UI for application that draws a histogram
ui <- dashboardPage(
    # Add title
    dashboardHeader(title = "2020 NCAA Division I Basketball Stats", titleWidth = 1000),
    # Define tabs
    dashboardSidebar(sidebarMenu(
        menuItem("About", tabName = "about", icon = icon("archive")),
        menuItem("Data Exploration", tabName = "exploration", icon = icon("laptop")),
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
            ),
            # Second tab item
            tabItem(tabName = "exploration",
                    fluidRow(
                        column(6,
                               h1("Data Exploration"))
                    )
                    ),
            # Third tab item
            tabItem(tabName = "modeling",
                    fluidRow(
                        column(6,
                               h1("Modeling"))
                    )
            ),
            # Fourth tab item
            tabItem(tabName = "data",
                    fluidRow(
                        column(6,
                               h1("Data")),
                        box(background="navy",width=5,
                            h3(strong("Variables:")),
                            h4("-TEAM (the Division I college basketball school)"),
                            h4("-CONF (the athletic conference the school participates in)"),
                            h4("-G (number of games played)"),
                            h4("-W (number of games won)"),
                            h4("-ADJOE (adjusted offensive efficiency)"),
                            h4("-ADJDE (adjusted defensive efficiency)"),
                            h4("-BARTHAG (power rating)"),
                            h4("-EFG_O (effective field goal shot)"),
                            h4("-EFG_D (effective field goal percentage allowed)"),
                            h4("-TOR (turnover rate)"),
                            h4("-TORD (steal rate)"),
                            h4("-ORB (offensive rebound rate)"),
                            h4("-DRB (offensive rebound rate allowed)"),
                            h4("-FTR (free throw ate)"),
                            h4("-FTRD (free throw rate allowed)"),
                            h4("-2P_O (2 point shooting percentage)"),
                            h4("-2P_D (2 point shooting percentage allowed)"),
                            h4("-3P_O (3 point shooting percentage)"),
                            h4("-3P_D (3 point shooting percentage allowed)"),
                            h4("-ADJ_T (wins above bubble)"),
                            h4("-POSTSEASON (round where the team was eliminated or their season ended)"),
                            h4("-SEED (seed in the NCAA March Madness tournament)"),
                            h4("-YEAR (season)"),
                            h4("-IN_TOUR (value = 1 if the team made the tournament and value = 0 if they did not)")
                        ),
                        dataTableOutput("mytable"),
                        # Button to download
                        downloadButton("downloadData", "Download")
                    )
            )
    )
    )
)



server <- function(input, output) {
    #create data table
    output$mytable = renderDataTable({
        cbbNew
    })
    #allow users to download the data
    output$downloadData <- downloadHandler(
        filename = function() {
            paste(input$dataset, ".csv", sep = "")
        },
        content = function(file) {
            write.csv(datasetInput(), file, row.names = FALSE)
        }
    )
    
}

shinyApp(ui, server)

