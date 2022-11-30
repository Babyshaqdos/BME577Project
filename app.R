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
library("CohortGenerator")
library("Cyclops")
#install_github("ohdsi/Hades", upgrade = "always")

serverFrame <- data.frame()

# Define UI for application that performs variable analysis of CSV data sets
ui <- fluidPage(

    # Application title
    titlePanel("Visual and Qualitative analysis of CSV Datasets"),
    
    #Layout defines the top portion of the UI
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
       actionButton("Add_variables", "Save variables in program", size = "sm", color="warning"),
       verbatimTextOutput(outputId = "independentVars"),
       verbatimTextOutput(outputId = "dependentVars"),
       selectInput("selectAnalysis", "Analysis to perform", c("ANOVA", "T-Test", "Linear Regression", "Logistic Regression",
                                                              "Poisson Logistic Regression", "Cox Logistic Regression")),
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

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  l <- reactiveValues()
  l$dataplots <- list()
  l$dependentVars <- list()
  l$independentVars <- list()
  l$firstIndependent <- NULL
  
  
  #Observer to watch the save dataset button, adds the filepath to a list then reads the CSV and puts it into a dataframe
  observeEvent(input$Add_dataset, { 
    l$filePath <- input$inputDataText
    l$dataplots[l$filePath] <- l$filePath
  #  df <- read.csv(l$filePath)   ##Need to include a check here to ensure the file exists, if not throws an error instead of crashing
  #  df2 <- data.frame(df)
  #  serverFrame <<- bind_rows(df2, serverFrame)

    output$datasets <- renderPrint({ #Data check
      if (is.null(l$dataplots)) return (NULL)
      paste(l$dataplots)
    })
  })
  
  observeEvent(input$Add_variables, {
    l$dependent <- input$variable2
    l$independent <- input$variable1
    if (is.null(l$firstIndependent)){
      l$firstIndependent <- l$independent
    }
    if (!l$dependent %in% l$dependentVars){
      l$dependentVars[l$dependent] <- l$dependent
    }
    if (!l$independent %in% l$independentVars){
      l$independentVars[l$independent] <- l$independent
    }
    
    output$dependentVars <- renderPrint({
      if (is.null(l$dependentVars)) return (NULL)
      paste(l$dependentVars)
    })
    
    output$independentVars <- renderPrint({
      if (is.null(l$independentVars)) return (NULL)
      paste(l$independentVars)
    })
    
  })
  
  
  #Puts an observer pattern on the run button, performs most functional requirements 
  observeEvent(input$run, {

    #takes the user inputted variable in the pop out menu and 
    #assigns them to a variable in l that can be accessed by UI
    l$plot <- input$plot
    l$analysisType <- input$selectAnalysis
    
    count <- 0
    for (csv in l$dataplots){
      df <- read.csv(csv)   ##Need to include a check here to ensure the file exists, if not throws an error instead of crashing
      df2 <- data.frame(df)
      if (count == 0){
        serverFrame <<- df2
        count <- 1
      }
      else{
        serverFrame <<- merge(serverFrame, df2, by = l$firstIndependent)
      }
    }

    
    #Selection of specific analysis algorithm
    if (l$analysisType == "ANOVA"){
    }
    else if (l$analysisType == "T-Test"){

    }
    else if (l$analysisType == "Linear Regression"){

    }
    else if (l$analysisType == "Poisson Logistic Regression"){
      formula <- formula(paste(names(l$dependentVars), " ~ ", paste(names(l$independentVars), collapse = " + ")))
      cyclopsData <- createCyclopsData(formula, modelType = "pr", data = serverFrame)
      cyclopsFit <- fitCyclopsModel(cyclopsData, prior = createPrior("none"))
      coefficient <- coef(cyclopsFit)
      prediction <- predict(cyclopsFit)
      l$prediction <- prediction
      output$summary <- renderPrint({
        if (is.null(l$prediction)) return (NULL)
        paste(summary(l$prediction))
      })
      
    }
    else if (l$analysisType == "Cox Logistic Regression"){
      formula <- formula(paste(names(l$dependentVars), " ~ ", paste(names(l$independentVars), collapse = " + ")))
      cyclopsData <- createCyclopsData(formula,time = serverFrame[[l$dependent]], modelType = "cox", data = serverFrame)
     # print("sanity check")
       cyclopsFit <- fitCyclopsModel(cyclopsData, prior = createPrior("none"))
     #  print(summary(cyclopsFit))
     # coefficient <- coef(cyclopsFit)
      prediction <- predict(cyclopsFit)
      l$prediction <- prediction
      output$summary <- renderPrint({
        if (is.null(l$prediction)) return (NULL)
        paste(summary(l$prediction))
      })
    }
    else if (l$analysisType == "Logistic Regression"){
      formula <- formula(paste(names(l$dependentVars), " ~ ", paste(names(l$independentVars), collapse = " + ")))
        cyclopsData <- createCyclopsData(formula, modelType = "lr", data = serverFrame)
      #  print(summary(cyclopsData))
        cyclopsFit <- fitCyclopsModel(cyclopsData, prior = createPrior("none"))
       # print("test", summary(cyclopsFit))
        coefficient <- coef(cyclopsFit)
      #  print(coefficient)
      #  confidence <- confint(cyclopsFit, names(l$independentVars))
      #  print("confidence", confidence)
        prediction <- predict(cyclopsFit)
      #  print("prediction", prediction)
       # print(prediction)
        l$prediction <- prediction
      #  print(l$prediction)
        output$summary <- renderPrint({
        #  print("Testing our output", l$prediction)
          if (is.null(l$prediction)) return (NULL)
          paste(summary(l$prediction))
        })
      
    }
   
    
    
    
    
    
    
    
    
     output$plot <- renderPlot({

  
    })
  
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
