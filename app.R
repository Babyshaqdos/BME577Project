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
       selectInput("selectAnalysis", "Analysis to perform", c("ANOVA", "T-Test", "Linear Regression")),
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
  dataPlot <- read.csv("C:/Users/nicks/Downloads/income.data_/income.data.csv")
  l <- reactiveValues()
  
  output$datasets <- renderPrint({
    l$temp <- input$Add_dataset
    paste(l$temp)
  })

  
  #Puts an observer pattern on the run button, takes the user inputted variable in the pop out menu and 
  #assigns them to a variable in l that can be accessed by UI
  observeEvent(input$run, {
    removeModal()
    l$variable1 <- input$variable1
    l$variable2 <- input$variable2
    l$varType1 <- input$varType1
    l$varType2 <- input$varType2
    l$plot <- input$plot
    output$plot <- renderPlot({
      var1 <- 'income'
      var2 <- 'happiness'
      var1DF <- dataPlot[ , var1]
      print(summary(dataPlot))
      var2DF <- dataPlot[ , var2]
    #  print(var1DF)
    #  print(var2DF)
   #   print(sapply(lapply(var1DF, unique), length))
        if (input$varType1 == 'Independent'){
      #    lm(var1 ~ var2, data = dataPlot)
          plot(var1DF ~ var2DF, data = na.omit(dataPlot))
          var1LM <- lm(var1DF ~ var2DF, data = na.omit(dataPlot))
          l$summary <- summary(var1LM)
        #  par(mfrow=c(2,2))
        #  plot(var1LM)
        #  par(mfrow=c(1,1))
          var1Graph <- ggplot(na.omit(dataPlot), aes(x=var1DF, y = var2DF)) + theme_bw() + geom_smooth(method = "lm", col="black")+ stat_regline_equation(label.x = 3, label.y = 7)
       #   var1Graph <- var1Graph + stat_regline_equation(label.x = 3, label.y = 7)
          var1Graph
        }
        if (input$varType2 == 'Independent'){
        #  lm(var2 ~ var1, data = na.omit(dataPlot))
        #  hist(dataPlot$var2)
       }
    })
    
    output$summary <- renderPrint({
      if (is.null(l$summary)) return (NULL)
      paste(l$summary)
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
