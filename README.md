# Project 3 - DI NCAA Basketball Stats App

## App Describtion
In this app, we took data from 2013-2021 seasons of DI NCAA Basketball found [here](https://www.kaggle.com/andrewsundberg/college-basketball-dataset). This data shows all the different teams in NCAA Division I and their ranking, conference, wins, and their team statistics by year. This app summerizes and plots this data in order to find correlation between a team's stats and the amount of wins they have. This app also models the data using a multiple linear regression model, a regression tree, and a random forest model.

## List of Packages
This is the list of the packages used.
- [`tidyverse`](https://www.tidyverse.org/): useful features for data
    science
- [`caret`](https://cran.r-project.org/web/packages/caret/vignettes/caret.html): set of functions that help to streamline the process for creating predictive models
- [`DT`](https://cran.r-project.org/web/packages/DT/index.html): converts R tables to HTML tables
- [`ggplot2`](https://ggplot2.tidyverse.org/): a package for making graphs and visualizations
- [`randomForest`](https://www.rdocumentation.org/packages/randomForest/versions/4.6-14/topics/randomForest): helps create random forest models
- [`readr`](https://readr.tidyverse.org/): a fast and easy way to read in rectangular data
- [`shiny`](https://shiny.rstudio.com/): makes it easy to create interactive webpages from R
- [`shinydashboard`](https://cran.r-project.org/web/packages/shinydashboard/index.html): creates dashboards for shiny apps

## Install Packages
This line of code installs all packages needed for the app.
```{r}
install.packages(tidyverse)
install.packages(caret)
install.packages(DT)
install.packages(ggplot2)
install.packages(randomForest)
install.packages(readr)
install.packages(shiny)
install.packages(shinydashboard)
```

## Run the App
 Use this code to run the app in RStudio.
 ```{r}
 shiny::runGitHub('Project3','kafrazi2', ref="main", subdir = "2020-DI-Basketball-Stats")
 ```
