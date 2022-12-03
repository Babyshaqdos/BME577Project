#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(ggpubr)
library(dplyr)
library(gsubfn)
library(RSQLite)
library(remotes)
library("Cyclops")
 
#install_github("ohdsi/Hades", upgrade = "always") 

serverFrame <- data.frame()

# Define UI for application that performs variable analysis of CSV data sets
ui <- fluidPage(

    # Application title
    titlePanel("Visual and Qualitative analysis of CSV Datasets"),
    
    #Layout defines the left side of the UI
    sidebarLayout(
      
      #This panel defines top left portion of UI, allows for user input of dataset and analysis option
      sidebarPanel(
        tags$h3('Please enter the dataset information and help define a cohort'),
        inputDataText <- textInput("inputDataText", label = "Path to dataset", 
                                   value ="",
                                   width = "50%",
                                   placeholder = "C:/Users/...."),
       actionButton("Add_dataset", "Save dataset in program", size="sm", color="warning"),
       verbatimTextOutput(outputId = "datasets"),
       tags$h3('Please enter your variables and save after each one'),
       textInput('variable1', 'Independent Variable'),
       textInput('variable2', 'Dependent Variables'),
       textInput('pvalue', 'P-Value/Mean (if necessary)'),
       textInput('joinVar', 'Column name to join on'),
       actionButton("Add_variables", "Save variables in program", size = "sm", color="warning"),
       verbatimTextOutput(outputId = "independentVars"),
       verbatimTextOutput(outputId = "dependentVars"),
       selectInput("selectAnalysis", "Analysis to perform", c("ANOVA", "T-Test", "Linear Regression", "Logistic Regression",
                                                              "Poisson Logistic Regression", "Cox Proportional Hazards Regression")),
       actionButton('run', 'Run'),
      ),
      
      #This panel contains the plots, table, and qualitative analysis summary
      mainPanel(
        tabsetPanel(
          tabPanel("Plot", plotOutput("plot")),
          tabPanel("Summary", verbatimTextOutput(outputId = "summary")),
          tabPanel("Table", tableOutput("table"))
        )
      )
    ),
    
  #This panel mostly for testing purposes atm  
  mainPanel(
      hr(),
      verbatimTextOutput(outputId = "text"), #contains the output in text
  )
    
    
)

# Hosts the R server, performs all of the logic in the application and outputs the results to the user
server <- function(input, output, session) {
  
  l <- reactiveValues() #Allows us to access our variables inputted by the user into the UI
  l$dataplots <- list() #Holds the CSV files
  l$dependentVars <- list() #Holds the dependent variables
  l$independentVars <- list() #Holds the independent variables
  l$firstIndependent <- NULL #Will want to revisit, used to hold the first independent variable to be used as criteria for joining of csv files
  l$joiner <- NULL
  l$pval <- NULL
  
  #Observer to watch the save dataset button, adds the filepath to a list then reads the CSV and places it into the list
  observeEvent(input$Add_dataset, { 
    #Save user input CSV to server side variable, then place variable into new position in csv list
    l$filePath <- input$inputDataText
    l$dataplots[l$filePath] <- l$filePath

    output$datasets <- renderPrint({ #Data check
      if (is.null(l$dataplots)) return (NULL)
      paste(l$dataplots)
    })
  })
  
  #Observer to watch the save variables button, checks if the appropriate lists contain the variable and adds it if it does not
  observeEvent(input$Add_variables, {
    #Save user input variables to a server side variable
    l$dependent <- input$variable2
    l$independent <- input$variable1
    l$joiner <- input$joinVar
    l$pval <- input$pvalue
    
    #Checks if an independent variable is saved to first independent, saves it if the first independent is null
    if (is.null(l$firstIndependent)){
      l$firstIndependent <- l$independent
    }
    #Data checks, ensures the variable lists only contain 1 of each variable
    if (!l$dependent %in% l$dependentVars){
      l$dependentVars[l$dependent] <- l$dependent
    }
    if (!l$independent %in% l$independentVars){
      l$independentVars[l$independent] <- l$independent
    }
    
    #Dependent Variables data check
    output$dependentVars <- renderPrint({
      if (is.null(l$dependentVars)) return (NULL)
      paste(l$dependentVars)
    })
    #independent variables data check
    output$independentVars <- renderPrint({
      if (is.null(l$independentVars)) return (NULL)
      paste(l$independentVars)
    })
    
  })
  
  
  #Puts an observer pattern on the run button, performs most functional requirements 
  observeEvent(input$run, {

  #  l$plot <- input$plot
    l$analysisType <- input$selectAnalysis #Gets the user selected analysis option and assigns it to a server side variable
    
    #Loop through our list of csv files, read them into a data frame then merge them by the first user input independent variable
    count <- 0
    for (csv in l$dataplots){
      #Check to ensure file exists (Will need to rework to display error to user in App instead of using Stop)
      if (!file.exists(csv)){
        stop("File not found, please check file path") #Prints the message to the R Studio console and stops program
      }
      #Reads csv and places into a data frame
      df <- read.csv(csv)   
      df2 <- data.frame(df)
      #Adds data frame to serverFrame
      if (count == 0){ 
        serverFrame <<- df2
        count <- 1
      }
      else{
        if (is.null(l$joiner)){
          stop("Did not indicate the columns to merge on")
        }
        serverFrame <<- merge(serverFrame, df2, by = l$joiner)
      }
    }

    
    #Selection of specific analysis algorithm
    #ANOVA: estimates how a quantitative dependent variable is affected by different levels of categorical independent variable(s)
    if (l$analysisType == "ANOVA"){
      #Grab values from dependent and independent lists 
      var1 <- l$dependentVars[[1]]
      var2 <- l$independentVars[[1]]
      anova <- aov(serverFrame[[var1]] ~ serverFrame[[var2]], data = serverFrame)
      l$anova <- anova
      output$summary <- renderPrint({ #Data Check
        if (is.null(l$anova)) return (NULL)
        paste(summary(l$anova))
      })
    }
    #T-Test: Determines whether the means of two groups (sampled with normal distributions with equal variance) are equivalent
    else if (l$analysisType == "T-Test"){
      if (is.null(l$pval)){ #If no mean is given, 2 sample test is assumed
        var1 <- l$independentVars[[1]]
        var2 <- l$independentVars[[2]]
        t_test <- t.test(serverFrame[[var1]], serverFrame[[var2]], paired = TRUE)
      }
      else{ #If mean given, 1 sample test is assumed
        var3 <- l$independentVars[[1]]
        print(l$independentVars[[1]])
        meanVal <- l$pval
        t_test <- t.test(as.numeric(serverFrame[[var3]]), mu = as.numeric(meanVal))
      }
      l$t_test <- t_test
      output$summary <- renderPrint({
        if (is.null(l$t_test)) return (NULL)
        paste(l$t_test)
      })
    }
    #Linear Regression: Analyses relationship between independent variable and 1 or more dependent variable 
    else if (l$analysisType == "Linear Regression"){
      formula <- formula(paste(names(l$dependentVars), " ~ ", paste(names(l$independentVars), collapse = " + ")))
      linearReg <- lm(formula, data = serverFrame)
      l$linearReg <- linearReg
      output$summary <- renderPrint({
        if (is.null(l$linearReg)) return (NULL)
        paste(summary(l$linearReg))
      })
    }
    #Poisson Logistic Regression: Similar to logistic regression but used when the outcome variable (dependent) is a count or rate of an event
    else if (l$analysisType == "Poisson Logistic Regression"){
      formula <- formula(paste(names(l$dependentVars), " ~ ", paste(names(l$independentVars), collapse = " + ")))
      cyclopsData <- createCyclopsData(formula, modelType = "pr", data = serverFrame)
      cyclopsFit <- fitCyclopsModel(cyclopsData, prior = createPrior("none"))
     #coefficient <- coef(cyclopsFit) #coef might not be necessary, gives us x intercept and slope
      prediction <- predict(cyclopsFit)
      l$prediction <- prediction
      output$summary <- renderPrint({ #Data check, prints summary of predictions to UI
        if (is.null(l$prediction)) return (NULL)
        paste(summary(l$prediction))
      })
      
    }
    #Cox Proportional Hazards Regression: Method of investing effects of multiple variables upon the timing of a specific event
    else if (l$analysisType == "Cox Proportional Hazards Regression"){
      formula <- formula(paste(names(l$dependentVars), " ~ ", paste(names(l$independentVars), collapse = " + ")))
      cyclopsData <- createCyclopsData(formula,time = serverFrame[[l$dependent]], modelType = "cox", data = serverFrame)
      cyclopsFit <- fitCyclopsModel(cyclopsData, prior = createPrior("none"))
     #coefficient <- coef(cyclopsFit) #coef might not be necessary, gives us x intercept and slope
      prediction <- predict(cyclopsFit)
      l$prediction <- prediction
      output$summary <- renderPrint({ #Data check, prints summary of predictions to UI
        if (is.null(l$prediction)) return (NULL)
        paste(summary(l$prediction))
      })
    }
    #Logistic Regression: Models probability of a dependent event occurring based on independent variables
    else if (l$analysisType == "Logistic Regression"){
      formula <- formula(paste(names(l$dependentVars), " ~ ", paste(names(l$independentVars), collapse = " + ")))
      cyclopsData <- createCyclopsData(formula, modelType = "lr", data = serverFrame)
      cyclopsFit <- fitCyclopsModel(cyclopsData, prior = createPrior("none"))
      #coefficient <- coef(cyclopsFit) #might not be necessary
      
      #Struggled to get confint to work, not required but useful for analysis. Gives confidence interval for each variable
      #Function could not find matching covariates, will revisit if time permits
      #confidence <- confint(cyclopsFit, names(l$independentVars)) 

      prediction <- predict(cyclopsFit)
      l$prediction <- prediction
      output$summary <- renderPrint({ #Data check
        if (is.null(l$prediction)) return (NULL)
        paste(summary(l$prediction))
      })
    }
  })
  
  #Mostly used for debugging purposes atm, returns user input to UI to be displayed
  output$text <- renderPrint({
    if (is.null(l$variable1)) return (NULL)
    if (is.null(l$variable2)) return (NULL)
    paste('Variable 1:', l$variable1, 'Variable 2:', l$variable2)
  
  })
  
 
  
}

# Run the application 
shinyApp(ui = ui, server = server)
