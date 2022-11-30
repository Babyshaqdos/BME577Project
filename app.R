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
        inputDataText <- textInput("inputDataVar", label = "Cohort Criteria", value = "",
                                   width = "50%", placeholder = "Traumatic Brain Injury, heart failure, stroke etc."),
       verbatimTextOutput(outputId = "datasets"),
       tags$h3('Please enter a variable and indicate dependent or independent'),
       textInput('variable1', 'Variable 1'),
       radioButtons("varType1", 'Variable Type', c("Dependent", "Independent")),
       textInput('variable2', 'Variable 2'),
       radioButtons('varType2', 'Variable Type', c("Dependent", "Independent")),
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
  
  
  #Observer to watch the save dataset button, adds the filepath to a list then reads the CSV and puts it into a dataframe
  observeEvent(input$Add_dataset, { 
    removeModal()
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
  
  
  #Puts an observer pattern on the run button, performs most functional requirements 
  observeEvent(input$run, {
    removeModal()

    #takes the user inputted variable in the pop out menu and 
    #assigns them to a variable in l that can be accessed by UI
    l$variable1 <- input$variable1
    l$variable2 <- input$variable2
    variable1 <- l$variable1
    variable2 <- l$variable2
    l$varType1 <- input$varType1
    l$varType2 <- input$varType2
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
        serverFrame <<- merge(serverFrame, df2, by = variable1)
      }
    }


    
    #Selection of specific analysis algorithm
    if (l$analysisType == "ANOVA"){
      if (l$varType1 == 'Independent'){
        
      }
      else if (l$varType2 == 'Independent'){
        
      }
    }
    else if (l$analysisType == "T-Test"){
      if (l$varType1 == 'Independent'){
        
      }
      else if (l$varType2 == 'Independent'){
        
      }
    }
    else if (l$analysisType == "Linear Regression"){
      if (l$varType1 == 'Independent'){
       
      }
      else if (l$varType2 == 'Independent'){
        
      }
    }
    else if (l$analysisType == "Poisson Logistic Regression"){
      if (l$varType1 == 'Independent'){
        
      }
      else if (l$varType2 == 'Independent'){
        
      }
    }
    else if (l$analysisType == "Cox Logistic Regression"){
      if (l$varType1 == 'Independent'){
        
      }
      else if (l$varType2 == 'Independent'){
        
      }
    }
    else if (l$analysisType == "Logistic Regression"){
      if (l$varType1 == 'Independent'){
        cyclopsData <- createCyclopsData(serverFrame[[variable2]] ~ serverFrame[[variable1]], modelType = "lr")
      #  print(summary(cyclopsData))
        cyclopsFit <- fitCyclopsModel(cyclopsData, prior = createPrior("none"))
       # print("test", summary(cyclopsFit))
       # coefficient <- coef(cyclopsFit)
      #  confidence <- confint(cyclopsFit, c(variable1,variable2))
      #  print("confidence", confidence)
        prediction <- predict(cyclopsFit)
      #  print("prediction", prediction)
        print("uhhhhhh why no print")
       # print(prediction)
        l$prediction <- prediction
      #  print(l$prediction)
        output$summary <- renderPrint({
        #  print("Testing our output", l$prediction)
        #  if (is.null(l$prediction)) return (NULL)
          paste(summary(l$prediction))
        })
      }
      else if (l$varType2 == 'Independent'){
        cyclopsData <- createCyclopsData(serverFrame[[variable1]] ~ serverFrame[[variable2]], modelType = "lr")
        #  print(summary(cyclopsData))
        cyclopsFit <- fitCyclopsModel(cyclopsData, prior = createPrior("none"))
        # print("test", summary(cyclopsFit))
        # coefficient <- coef(cyclopsFit)
        #  confidence <- confint(cyclopsFit, c(variable1,variable2))
        #  print("confidence", confidence)
        prediction <- predict(cyclopsFit)
        #  print("prediction", prediction)
        print("uhhhhhh why no print")
        # print(prediction)
        l$prediction <- prediction
        #  print(l$prediction)
        output$summary <- renderPrint({
          #  print("Testing our output", l$prediction)
          #  if (is.null(l$prediction)) return (NULL)
          paste(summary(l$prediction))
        })
      }
    }
   
    
    
    
    
    
    
    
    
     output$plot <- renderPlot({

  
    })
  
  })
  
  #Mostly used for debugging purposes atm, returns user input to UI to be displayed
  output$text <- renderPrint({
    if (is.null(l$variable1)) return (NULL)
    if (is.null(l$variable2)) return (NULL)
    paste('Variable 1:', l$variable1, l$varType1, 'Variable 2:', l$variable2, l$varType2)
  
  })
  
 
  
}

# Run the application 
shinyApp(ui = ui, server = server)
