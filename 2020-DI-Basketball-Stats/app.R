# Read in required libraries
library(shiny)
library(shinydashboard)
library(tidyverse)
library(caret)
library(readr)
library(ggplot2)
library(DT)
library(randomForest)

# Read in data set
cbb <- read_csv(file = "cbb.csv") 

# Define UI for application 
ui <- dashboardPage(
    # Add title
    dashboardHeader(title = "NCAA Division I Men's Basketball Stats", titleWidth = 1000),
    # Define tabs
    dashboardSidebar(sidebarMenu(
        menuItem("About", tabName = "about", icon = icon("archive")),
        menuItem("Data Exploration", tabName = "exploration", icon = icon("bar-chart-o")),
        menuItem("Modeling", tabName = "modeling", icon = icon("laptop"), startExpanded = TRUE,
                menuSubItem("Modeling Info", tabName = "info"),
                menuSubItem("Model Fitting", tabName = "fit"),
                menuSubItem("Prediction", tabName = "predict")),
        menuItem("Data", tabName = "data", icon = icon("th"))
    )),
    # Define the body of the app
    dashboardBody(
        tabItems(
            # About tab content
            tabItem(tabName = "about",
                    fluidRow(
                        img(src = "ncaabasketball4.jpg", height = 300, width = 1000),
                        column(6,
                               # Description of App
                               h1("App Purpose", style="color:#005eb8"),
                               # Box to contain description
                               box(background="navy",width=12,
                                   h4("The purpose of this app is to describe, summarize, and predict data from NCAA DI men's basketball stats for seasons 2013-2021. This app allows users to look at and customize various plots and tables of this data to see relationships between teams, conferences, wins, and a team's statistics. In this app, you will be able to predict a team's number of wins based off several statistical predictors.")
                               )
                        ),
                        
                        column(6,
                               # Data description
                               h1("Data Description", style="color:#005eb8"),
                               # Box to contain description
                               box(background="navy",width=12,
                                   h4("This data was collected from a college basketball data set on kaggle found", a("here", href = "https://www.kaggle.com/andrewsundberg/college-basketball-dataset"),"."),
                                   h4(a("Kaggle", href = "https://www.kaggle.com/"), "is a huge repository full of community published data."),
                                   h4("The data used in this app is information about NCAA Division I men's basketball. It covers years from 2013-2021. It lists team names, conferences, win, and overall team statistics throughout the season.")
                               )
                        ),
                        column(6,
                               # Purpose of each tab
                               h1("Purpose of Each Page", style="color:#005eb8"),
                               # Box to contain description
                               box(background="navy",width=12,
                                   h4(strong("-Data Exploration"), ": this page allows users to customize numerical and graphical summeries involving the data."),
                                   h4(strong("-Modeling"), ": this page allows customization of three different types of supervised learning models. This page also includes three extra tabs: Modeling Info (describes the types of models being used), Model Fitting (enables user customizations for the different types of models), and Prediction (allows users to use one of the models to predict the response)."),
                                   h4(strong("-Data"), ": this page allows users to scroll through the dataset, subset the data, and download it.")
                               )
                        )
                    )
            ),
            # Data exploration tab content
            tabItem(tabName = "exploration",
                    fluidRow(
                               h1("Data Exploration"),
                        column(6,
                               h3("Summary table"),
                               h5("Select statistic to summarize:"),
                               # Select input for summary
                               selectizeInput("stat", "Statistic", 
                                              choices = list("ADJOE", "ADJDE", "BARTHAG", "EFG_O", "EFG_D", "TOR", "TORD", "ORB", "DRB", "FTR", "FTRD", "2P_O", "2P_D", "3P_O", "3P_D", "ADJ_T", "WAB")),
                              # Output data table
                                dataTableOutput("DT")),
                        column(6,
                               h3("Bar Plot of Conference vs. Number of Wins"),
                               # Output bar plot
                               plotOutput("barPlot"),
                               # Create check box
                               checkboxInput("color", h4("Add color")),
                               #only show this panel if color is checked
                               conditionalPanel(condition = "input.color",
                                                checkboxInput("horizontal", h5("Horizontal bar plot")))),
                        column(6,
                               # create slider input
                               sliderInput("bins", "Number of bins:", min = 1, max = 20, value = 10),
                               # Create action button
                               actionButton("reset", "Reset"),
                               # Output histogram
                               h3("Histogram of Number of Wins"),
                               plotOutput("distPlot"))
                        )
                               
                               
                        
                    ),
            # Model info tab content
            tabItem(tabName = "info",
                    fluidRow(
                               h1("Modeling info"),
                               br(),
                        column(6,
                            box(background="navy",width=12,
                               h3(strong("Multiple Linear Regression Model")),
                               h4("Multiple linear regression is a modeling technique that uses multiple explanatory variables to predict a response variable."),
                               h4("Benefits of this type of modeling are: the ability to use multiple predictors or higher order terms and it easily identifies outliers in the data."),
                               h4("A drawback of this type of modeling is: this model could lead to a false causation conclusion."),
                               br(),
                               h4("Formula:"),
                               withMathJax(),
                               uiOutput('MLRFormula')
                    )),
                        column(6,
                           box(background="navy",width=12,
                               h3(strong("Regression Tree Model")),
                               h4("Regression tree models split up predictor space into regions with different predictions for each region. They then predict a continuous response by using mean of observations as prediction."),
                               h4("Benefits of this model: easy to visualize and helps make quick decision because of its interpretability."),
                               h4("A drawback of this type of model: it involves extra time to train the model as compared to other methods."),
                               br(),
                               h4("Formula:"),
                               h5("For every possible value of each predictor, find Residual Sum of Squares and try to minimize that."),
                               withMathJax(),
                               uiOutput('RegTreeFormula'),
                               h5("We seek the value of j and s that minimize the equation."),
                               withMathJax(),
                               uiOutput('RegTreeFormula2')
                           )),
                        column(6,
                           box(background="navy",width=12,
                               h3(strong("Random Forest Model")),
                               h4("Random forest models creates multiple trees from bootstrap samples then averages the results."),
                               h4("Benefits of this model: it randomly selects a subset of predictors so a good predictor won't dominate the tree fits and it works well with categorical and continuous values."),
                               h4("A drawback of this type of model: it is complex and creates a lot of trees making its computation time longer."),
                               br(),
                               h4("Formula:"),
                               withMathJax(),
                               h5("For randomly selected predictors in classification use:"),
                               uiOutput('RFFormula'),
                               h5("For randomly selected predictors in regression use:"),
                               uiOutput('RFFormula2'),
                               h5("Then find", em("m"), "through OOB error and if", em("m = p"), "you have bagging")
                           ))
                    )),
            # Model fit tab content
            tabItem(tabName = "fit",
                    fluidRow(
                               h1("Model Fitting"),
                        column(6,
                               h4("Create Training Set Size"),
                               numericInput("ni","Enter a value between 0 and 1 for training set:", value = 0.8, min = 0, max = 1, step = 0.05)),
                        column(6,
                               h4("Variable Predictors"),
                               selectizeInput("pred", "Select predictors:", multiple = TRUE,
                                              choices = list("ADJOE", "ADJDE", "BARTHAG", "EFG_O", "EFG_D", "TOR", "TORD", "ORB", "DRB", "FTR", "FTRD", "2P_O", "2P_D", "3P_O", "3P_D", "ADJ_T", "WAB"))),
                        column(6,
                               h4("Regression Tree Cross Validation"),
                               numericInput("cv1", "Select the number of cross validation folds:", value = 5, min = 1, max =10, step = 1),
                               numericInput("repeat1", "Select the number of repetitions:", value = 3, min = 1, max =10, step = 1),
                               br(),
                               h4("Random Forest Cross Validation"),
                               numericInput("cv2", "Select the number of cross validation folds:", value = 5, min = 1, max =10, step = 1),
                               numericInput("repeat2", "Select the number of repetitions:", value = 3, min = 1, max =10, step = 1),
                               actionButton("fit", "Fit Model"),
                               conditionalPanel(condition = "input.fit == 1",
                                                h6("Please be patient while the models load", style = "color:red"))),
                        column(6,
                               h4("Multiple Linear Regression Summary Stats (Training Data):"),
                               verbatimTextOutput("MLRTrain"),
                               h4("Multiple Linear Regression Summary Stats (Test Data):"),
                               verbatimTextOutput("MLRTest"),
                               h4("Regression Tree Summary Stats (Training Data):"),
                               verbatimTextOutput("RegTreeTrain"),
                               h4("Regression Tree Summary Stats (Test Data):"),
                               verbatimTextOutput("RegTreeTest"),
                               h4("Random Forest Tree Summary Stats (Training Data):"),
                               verbatimTextOutput("RandForTrain"),
                               h4("Random Forest Tree Summary Stats (Test Data):"),
                               verbatimTextOutput("RandForTest")) 
                        )
                    
            ),
            # Prediction tab content
            tabItem(tabName = "predict",
                    fluidRow(
                            h1("Prediction"),
                            column(6,
                                   numericInput("pred1", "Select the value for offensive efficiency", value = 0, min = NA, max =NA, step = NA),
                                   numericInput("pred2", "Select the value for defensive efficiency", value = 0, min = NA, max =NA, step = NA),
                                   numericInput("pred3", "Select the value for turnover rate", value = 0, min = NA, max =NA, step = NA),
                                   numericInput("pred4", "Select the value for offensive rebound rate", value = 0, min = NA, max =NA, step = NA),
                                   actionButton("prediction", "Predict")),
                            column(6,
                                   h4("Model"),
                                   h5("For this prediction model we will use a multiple linear regression model to determine number of wins."),
                                   br(),
                                   h5("This model will use the following predictors:"),
                                   h5("-Offensive efficiency, Defensive efficiency, Turnover rate, and Offensive rebound rate"),
                                   br(),
                                   h5("This calculation uses the following coefficients to predict"),
                                   verbatimTextOutput("coefficients"),
                                   br(),
                                   br(),
                                   h5("Prediction:"),
                                   uiOutput("finalPred"))
                    )
            ),
            # Data tab content
            tabItem(tabName = "data",
                    fluidRow(
                            h1("Data"),
                        # Make box for variable names and descriptions
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
                            h4("-FTR (free throw rate)"),
                            h4("-FTRD (free throw rate allowed)"),
                            h4("-2P_O (2 point shooting percentage)"),
                            h4("-2P_D (2 point shooting percentage allowed)"),
                            h4("-3P_O (3 point shooting percentage)"),
                            h4("-3P_D (3 point shooting percentage allowed)"),
                            h4("-ADJ_T (wins above bubble)"),
                            h4("-POSTSEASON (round where the team was eliminated or their season ended)"),
                            h4("-SEED (seed in the NCAA March Madness tournament)"),
                            h4("-YEAR (season)")
                        ),
                        # Output data table
                        dataTableOutput("mytable"),
                        # Button to download
                        downloadButton("downloadData", "Download")
                    )
            )
    )))




server <- shinyServer(function(input, output, session) {
    
    # Create numeric summary
    output$DT <- renderDT({
        # Grab variables
        stat <- input$stat
        cbbSub <- cbb[, c("TEAM", "CONF","W", "SEED", stat),
                                        drop = FALSE]
        # Call the table
        cbbSub
    })
    
    # Create bar plot
    output$barPlot <- renderPlot({
        # Use conditional logic to create plot
        g <- ggplot(cbb, aes(x = CONF, y = W)) 
        if(input$horizontal){
            g + geom_bar(stat = 'identity', aes(fill = CONF)) + labs(x = "Conference", y = "Number of Wins") + coord_flip()
        }
        else if(input$color){
            g + geom_bar(stat = 'identity', aes(fill = CONF)) + guides(x = guide_axis(angle = 45)) + labs(x = "Conference", y = "Number of Wins")
        } else {
            g + geom_bar(stat = 'identity') + guides(x = guide_axis(angle = 45)) + labs(x = "Conference", y = "Number of Wins")
        }
    })
    
    # Create histogram
    output$distPlot <- renderPlot({
        # Add number of bins
        x    <- cbb$W
        bins <- seq(min(x), max(x), length.out = input$bins + 1)
        
        hist(x, breaks = bins, col = "#005eb8", border = "white",
             xlab = "Number of Wins",
             main = "Histogram of Wins")
    })
    
    # Create reset button with dynamic UI
    observeEvent(input$reset, {
        updateSliderInput(session = getDefaultReactiveDomain(), "bins", value = 10)
    })
    
    # Create formula for MLR
    output$MLRFormula <- renderUI({
        withMathJax(helpText('$$\\Rightarrow Y = \\hat{\\beta}_0 + \\hat{\\beta}_1 x_1 + \\hat{\\beta}_2 x_2 + E$$'))
    })
    
    # Create formula for regression tree
    output$RegTreeFormula <- renderUI({
        withMathJax(helpText('$$\\Rightarrow R_1(j, s) = \\{x|x_j < s\\}\\  R_2(j, s) = \\{x|x_j \\geq s\\}$$'))
    })
    
    output$RegTreeFormula2 <- renderUI({
        withMathJax(helpText('$$\\Rightarrow \\sum_{i:x_i\\in R_1(j,s)}(y_i-\\bar{y}_{R_1})^2+\\sum_{i:x_i\\in R_2(j,s)}(y_i-\\bar{y}_{R_2})^2$$'))
    })
    
    # Create formula for random forest
    output$RFFormula <- renderUI({
        withMathJax(helpText('$$\\Rightarrow m = \\sqrt{p}$$'))
    })
    
    output$RFFormula2 <- renderUI({
        withMathJax(helpText('$$\\Rightarrow m = p/3$$'))
    })
    
    # Create train and test data
    trainData <- reactive({
        cbbNew <- cbb %>% mutate_all(~replace(., is.na(.), 0)) 
        trainIndex <- createDataPartition(cbbNew$W, p = input$ni, list = FALSE)
        train <- cbbNew[trainIndex,]
        trainData <- as.data.frame(train)
    })
    
    testData <- reactive({
        cbbNew <- cbb %>% mutate_all(~replace(., is.na(.), 0)) 
        trainIndex <- createDataPartition(cbbNew$W, p = input$ni, list = FALSE)
        test <- cbbNew[-trainIndex,]
        testData <- as.data.frame(test)
    })
    
    # Fit MLR model for train data
    MLRTrain <- eventReactive(input$fit, {
        y <- "W"
        x <- input$pred
        mlrFormula <- as.formula(paste(y, paste(x, collapse = " + "), sep = " ~ "))
        mlrFit <- lm(mlrFormula, data = trainData())
        mlrFit
        summary(mlrFit)
    })
    
    # Output MLR model
    output$MLRTrain <- renderPrint({
        MLRTrain()
    })
    
    # Fit MLR model for test data
    MLRTest <- eventReactive(input$fit, {
        y <- "W"
        x <- input$pred
        mlrFormula <- as.formula(paste(y, paste(x, collapse = " + "), sep = " ~ "))
        mlrFit <- lm(mlrFormula, data = testData())
        summary(mlrFit)
    })
    
    # Output MLR model
    output$MLRTest <- renderPrint({
        MLRTest()
    })
    
    
    # Regression tree model for train data
    RegTreeTrain <- eventReactive(input$fit, {
        
        # Create tuning parameters
        cp <- 0:0.1
        df <- expand.grid(cp = cp)
        
        y <- "W"
        x <- input$pred
        regFormula <- as.formula(paste(y, paste(x, collapse = " + "), sep = " ~ "))
        
        regFit <- train(regFormula, data = trainData(), method = "rpart", trControl = trainControl(method = "repeatedcv", number = input$cv1, repeats = input$repeat1), tuneGrid = df)
        regFit    
    })
    
    # Output regression tree
    output$RegTreeTrain <- renderPrint({
        RegTreeTrain()
    })
    
    #regression tree model for test data
    RegTreeTest <- eventReactive(input$fit, {
        
        # Create tuning parameters
        cp <- 0:0.1
        df <- expand.grid(cp = cp)
        
        # Fit regression tree
        y <- "W"
        x <- input$pred
        regFormula <- as.formula(paste(y, paste(x, collapse = " + "), sep = " ~ "))
        regFit <- train(regFormula, data = testData(), method = "rpart", trControl = trainControl(method = "repeatedcv", number = input$cv1, repeats = input$repeat1), tuneGrid = df)
        regFit

    })
    
    # Output regression tree
    output$RegTreeTest <- renderPrint({
        RegTreeTest()
    })
    
    # Create random forest tree model for train data
    RFTrain <- eventReactive(input$fit, {
        
        # Create tuning parameters
        mtry <- 1:2
        df <- expand.grid(mtry = mtry)
        
        # fit random forest
        y <- "W"
        x <- input$pred
        rfFormula <- as.formula(paste(y, paste(x, collapse = " + "), sep = " ~ "))
        rfFit <- train(rfFormula, data = trainData(), method = "rf", trControl = trainControl(method = "repeatedcv", number = input$cv2, repeats = input$repeat2), tuneGrid = df)
        rfFit
    })
    
    # Output random forest model
    output$RandForTrain <- renderPrint({
        RFTrain()
    })
    
    # Create random forest model for test data
    output$RFTest <- eventReactive(input$fit, {
        
        # Create tuning parameters
        mtry <- 1:2
        df <- expand.grid(mtry = mtry)
        
        # Fit random forest model
        y <- "W"
        x <- input$pred
        rfFormula <- as.formula(paste(y, paste(x, collapse = " + "), sep = " ~ "))
        rfFit <- train(rfFormula, data = testData(), method = "rf", trControl = trainControl(method = "repeatedcv", number = input$cv2, repeats = input$repeat2), tuneGrid = df)
        rfFit
    })
    
    
    # Prediction model
    predModel <- eventReactive(input$prediction, {
        mlrFit <- lm(W ~ ADJOE + ADJDE + TOR + ORB, data = testData())
        mlrFit
        
        predict <- (mlrFit$coefficients[2]*input$pred1 + mlrFit$coefficients[3]*input$pred2 + mlrFit$coefficients[4]*input$pred3 + mlrFit$coefficients[5]*input$pred4)
        predict
    })
    
    # Prediction output
    output$finalPred <- renderUI({
        text1 <- "The estimated number of wins based on the predictor variables is "
        predict1 <- round(predModel(), digits = 0)
        paste(text1, predict1)
    })
    
    # Output coefficients
    output$coefficients <- renderPrint({
        mlrFit <- lm(W ~ ADJOE + ADJDE + TOR + ORB, data = cbb)
        mlrFit$coefficients
    })
    
    # Create data table
    output$mytable = renderDataTable({
        cbb
    })
    # Allow users to download the data
    output$downloadData <- downloadHandler(
        filename = function() {
            paste(input$dataset, ".csv", sep = "")
        },
        content = function(file) {
            write.csv(datasetInput(), file, row.names = FALSE)
        }
    )
    
})

shinyApp(ui, server)

