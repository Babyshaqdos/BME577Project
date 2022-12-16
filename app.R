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
library(tidyverse)
library(gginference)
library(shinyjs)
library(rstatix)
if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")

#BiocManager::install("survcomp")
library(survcomp)
library(finalfit)
library(sandwich)
library("broom")
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
       textInput('pvalue', 'P-Value/Mean/Cut Off (if necessary)'),
       textInput('joinVar', 'Column name to join on'),
       actionButton("Add_variables", "Save variables in program", size = "sm", color="warning"),
       verbatimTextOutput(outputId = "independentVars"),
       verbatimTextOutput(outputId = "dependentVars"),
       selectInput("selectAnalysis", "Analysis to perform", c("ANOVA", "T-Test", "Linear Regression", "Logistic Regression",
                                                              "Poisson Regression", "Cox Proportional Hazards Regression")),
       actionButton('run', 'Run'),
       actionButton('reset', 'Reset'),
      ),
      
      #This panel contains the plots, table, and qualitative analysis summary
      mainPanel(
        tabsetPanel(
          tabPanel("Plot", plotOutput("plot")),
          tabPanel("Summary", verbatimTextOutput(outputId = "summary")),
          tabPanel("Table",
                   fluidRow(column(12, verbatimTextOutput("table"))),
                  fluidRow(column(12, verbatimTextOutput("table2")))
          )
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
  l$firstIndependent <- NULL #used to hold the first independent variable to be used as criteria for joining of csv files
  l$joiner <- NULL #holds the column name upon which to merge csvs
  l$pval <- NULL #holds the p value specified by the user
  
  
  #Observer that resets all UI elements and Server variables when reset button clicked
  observeEvent(input$reset, {
    reset("inputDataText")
    reset("variable1")
    reset("variable2")
    reset("pvalue")
    reset("joinVar")
    reset("independentVars")
    reset("dependentVars")
    reset("datasets")
    l$dataplots <- list()
    l$dependentVars <- list()
    l$independentVars <- list()
    l$firstIndependent <- NULL
    l$joiner <- NULL
    l$pval <- NULL
    serverFrame <<- data.frame()
    output$summary <- renderText({
    })
    output$plot <- renderPlot({
    })
    output$table <- renderText({
    })
    output$table2 <- renderText({
    })
  })
  
  
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
    
    if (is.null(l$firstDependent)) {
      l$firstDependent <- l$dependent
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
  
  tryCatch({
  #Puts an observer pattern on the run button, performs most functional requirements 
  observeEvent(input$run, {
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
        if (is.null(l$joiner)){ #checks that we are provided a column to merge on
          stop("Did not indicate the columns to merge on")
        }
        serverFrame <<- merge(serverFrame, df2, by = l$joiner)
      }
    }

    #Test Data: field_data.csv
    #Selection of specific analysis algorithm
    #ANOVA: estimates how a quantitative dependent variable is affected by different levels of categorical independent variable(s)
    if (l$analysisType == "ANOVA"){
      #Grab values from dependent and independent lists 
      var1 <- l$dependentVars[[1]]
      var2 <- l$independentVars[[1]]
      #Perform anova
      anova <- aov(serverFrame[[var1]] ~ serverFrame[[var2]], data = serverFrame)
      l$anova <- anova
      output$summary <- renderText({ #Data Check
        if (is.null(l$anova)) return (NULL)
        if (l$pval > summary(anova)[[1]][[1, "Pr(>F)"]]){ #checks the pval 
          paste(output)
        }
        else{
          paste("The ANOVA P value is less than or equivalent to the provided p value, therefore we reject the null hypothesis",
          "We conclude that the independent variable does seem to have an affect on the dependent variable.", sep = '\n')
        }
      })
      output$table <- renderText({
        if (is.null(l$anova)) return (NULL)
        paste(capture.output(summary(anova)), collapse = "\n")
        
        
      })
      output$plot <- renderPlot({
        if (is.null(l$anova)) return (NULL)
       # par(mfrow=c(2,2))
        plot(anova)
      #  par(mfrow=c(1,1))
      })
    }
    #Test Data: LungCapData (1 sample t test)
    #T-Test: Determines whether the means of two groups (sampled with normal distributions with equal variance) are equivalent
    else if (l$analysisType == "T-Test"){
     # if (is.null(l$pval) || l$pval == ""){ #If no mean is given, 2 sample test is assumed
        # Not working, idk why. Throws an error that there are NA values despite me omitting them. Ran out of time to debug
      #  var1 <- l$independentVars[[1]]
      #  var2 <- l$independentVars[[2]]
      #  t_test <- t.test(na.omit(serverFrame[[var1]]), na.omit(serverFrame[[var2]]))
      #  tPlot <- ggboxplot(serverFrame, x = var1, y = var2, ylab = var2, xlab = var1, add = "jitter")
      #}
      #else{ #If mean given, 1 sample test is assumed
        var3 <- l$independentVars[[1]]
        meanVal <- l$pval
        t_test <- t.test(as.numeric(serverFrame[[var3]]), mu = as.numeric(meanVal)) #Perform T-Test
        #Generate Plot
        tPlot <- ggdensity(serverFrame, x = var3, rug = TRUE, fill = 'lightgray') +
          scale_x_discrete(limits = c(0, length(serverFrame[[var3]]))) + stat_central_tendency(type = "mean", color = "red", linetype = "dashed") +
          geom_vline(xintercept = meanVal, color = "blue", linetype = "dashed")
    #  }
      l$t_test <- t_test
      output$summary <- renderText({
        if (is.null(l$t_test)) return (NULL)
        if (l$pval > .05){
          paste("The T-Test p value is greater than .05 we fail to reject the null hypothesis.",
          "The mean of the variables does not show variance", sep = '\n')
        }
        else{
          paste("The T-Test p value is less than .05 therefore we reject the null hypothesis.",
          "The evidence is sufficient to say the mean of the variables is different", sep='\n')
        }
      })
      output$plot <- renderPlot({
        if (is.null(l$t_test)) return (NULL)
        plot(tPlot)
      })
      output$table <- renderText({
        if (is.null(l$t_test)) return (NULL)
        #paste(l$t_test)
        paste(capture.output(l$t_test), collapse = "\n")
        
      })
    }
    #Test Data: marketing_budget
    #Linear Regression: Analyses relationship between independent variable and 1 or more dependent variable 
    else if (l$analysisType == "Linear Regression"){
      formula <- formula(paste(names(l$dependentVars), " ~ ", paste(names(l$independentVars), collapse = " + ")))
      linearReg <- lm(formula, data = serverFrame)
      l$linearReg <- linearReg
      #Generates a data frame to hold all ofthe value of the LM model for easy display
      reg_table <- data.frame("Estimate" = coef(l$linearReg), "Std.Error" = summary(l$linearReg)$coefficients[, "Std. Error"],
                              "t.value" = summary(l$linearReg)$coefficients[, "t value"], "P.value" = summary(l$linearReg)$coefficients[, "Pr(>|t|)"])
      
      output$table <- renderText({
        if (is.null(l$linearReg)) return (NULL)
        paste(capture.output(print(reg_table, row.names = TRUE)), collapse = "\n")
      })
      output$plot <- renderPlot({
        if (is.null(l$linearReg)) return (NULL)
        plot(l$linearReg)
        
      })
      output$summary <- renderPrint({
        if (is.null(l$linearReg)) return (NULL)
        strOutput <- ""
        countVars <- 0
        sigVars <-0
        for (variable in reg_table$P.value){ 
          if (variable >= .05){
            strOutput <- cat(strOutput, " variable ", countVars,  " has a p value greater than or equal to .05 and should be significant")
            strOutput <- cat(strOutput, "\n")
            sigVars <- sigVars + 1
          }
          else{
            strOutput <- cat(strOutput, " variable ", countVars, " has a p value less than .05 and should not be considered significant")
            strOutput <- cat(strOutput, "\n")
          }
          countVars <- countVars + 1
        }
        varStr <- "We have a total of "
        varStr <- cat(varStr, sigVars, " significant variables")
        paste(strOutput, varStr, sep = "\n")
      })
    }
    #Test Data: data (or framingham/other logistic regression sets)
    #Poisson Logistic Regression: Similar to logistic regression but used when the outcome variable (dependent) is a count or rate of an event
    else if (l$analysisType == "Poisson Regression"){
      serverFrame[is.na(serverFrame)] <- 0 #Sets all NA values to 0 (tried ommitting them and couldnt get it to work)
      formula <- formula(paste(names(l$dependentVars), " ~ ", paste(names(l$independentVars), collapse = " + "))) 
      #Cyclops implementation
      cyclopsData <- createCyclopsData(formula, modelType = "pr", data = serverFrame)
      cyclopsFit <- fitCyclopsModel(cyclopsData, prior = createPrior("none"))
      #Get coefficients and prediction values
      coefficient <- coef(cyclopsFit, rescale = FALSE, ignoreConvergence = TRUE) 
      l$coefficient <- coefficient
      prediction <- predict(cyclopsFit, interval = "prediction", se.fit = TRUE, type = "link")
      l$prediction <- prediction
      l$cyclops <- cyclopsFit
      
      #Grabbing residuals
      depend <- names(l$dependentVars)[1]
      originalVar <- serverFrame[depend]
      predDF <- data.frame(Predictions = prediction)
      residualDF <- data.frame(Residual = numeric())
      residualDF <- originalVar - predDF
      
      
      #Create a data frame to hold all locally relevant information
      newDF <- data.frame(Dependent_Variable = serverFrame[names(l$dependentVars)[1]])
      for (var in names(l$independentVars)){
        newDF <- cbind(newDF, serverFrame[var])
      }

      #Normalize our predictor variables so they are between 0 and 1
      min_max_norm <- function(x) {
        (x - min(x)) / (max(x) - min(x))
      }
      normDF <- as.data.frame(lapply(newDF[2:length(newDF)], min_max_norm))

      #Join normalized variables with predictions
      newDF <- cbind(newDF[1], normDF)
      newDF <- cbind(newDF, predDF)
      
      #Create data frame to hold original observation and calculated residual 
      plotDF <- cbind(originalVar, residualDF)
      
      #Create column names to access data
      colnames(plotDF) <- c("Original", "Residual")
      colnames(newDF) <- c("Dependent_Variable", paste("Independent_Variable", 1:(ncol(newDF)-2)), "Prediction")
      
    
 
      
      output$summary <- renderPrint({ #Data check
        if (is.null(l$prediction)) return (NULL)
        outStr <- cat("If our Fitted Vs Residuals graph shows a fairly even distribution above and below the 0 line then we can reasonably assume our model is accurate.", "\n")
        outStr <- cat("If there is noticable clustering then it is possible that the model has a high variance or is not linear.", "\n")
        outStr <- cat("To further understand the model please look at the table tab for a full breakdown of the weights of each variable and their effect on the predicted outcome")
      })
      output$plot <- renderPlot({
        if (is.null(l$prediction)) return (NULL)
        counter <- 2
        par(mfrow = c(2,1)) #Create 2 data tables
        par(mar = c(4,4,2,0))
        #First Data Table shows distribution of normalized variables and their effect on predicted outcome 
        plot(x = newDF$"Dependent_Variable", y = newDF$"Independent_Variable 1", ylim=c(0,1), pch = 1, col = "red",
             xlab = "Dependent Variable", ylab = "Normalized Independent", main = "Distribution of predictor variables")
        for (indVars in names(newDF[3:length(newDF)-1])){
          points(newDF$"Dependent_Variable", newDF[[indVars]], pch = counter, col = counter)
          counter <- counter + 1
        }
        legend("topright", c(paste("Independent Variable", 1:(ncol(newDF)-2))), pch = c(paste(1:(ncol(newDF)-2))), col = c(paste(1:(ncol(newDF)-2))))
        
        
        #Second data table to show the observed vs residuals 
        plot(x = newDF$"Prediction", y = plotDF$"Residual", xlab = "Fitted Values", ylab = "Standardized Residuals",
             main = "Fitted vs Residuals") 
        abline(0, 0)
       
          
      })
      output$table <- renderText({
        if (is.null(l$prediction)) return (NULL)
        newDF <- cbind(newDF, originalVar)
        colnames(newDF) <- c("Dependent_Variable", paste(names(l$independentVars)), "Prediction", "Original")
        paste(capture.output(newDF, row.names = FALSE), collapse = "\n")
      })
    }
    #Sample Data: echocardiogram.csv
    #Cox Proportional Hazards Regression: Method of investing effects of multiple variables upon the timing of a specific event
    else if (l$analysisType == "Cox Proportional Hazards Regression"){
      serverFrame[is.na(serverFrame)] <- 0 #set NA values to 0
      formDF <- names(l$dependentVars)[2:length(l$dependentVars)] #Get all dependent variables besides first one 
      formula <- formula(paste(formDF, " ~ ", paste(names(l$independentVars), collapse = " + ")))

      #Censor the data, remove data values where outcome result falls outside designated time window
      #Getting the value from the user inputted pval (fix later)
      censoredDF <- data.frame()
      pVal <- l$pval
      for (i in 1:nrow(serverFrame)){
        tempVal <- serverFrame[i, 1]
        if (tempVal <= pVal){
          censoredDF <- rbind(censoredDF, serverFrame[i,])
        }
      }
      

      #Cyclops implementation
      cyclopsData <- createCyclopsData(formula,time = censoredDF[[l$firstDependent]], modelType = "cox", data = censoredDF)
      cyclopsFit <- fitCyclopsModel(cyclopsData, prior = createPrior("none"))
      coefficient <- coef(cyclopsFit) 
      prediction <- predict(cyclopsFit)
      l$prediction <- prediction

      hazardRatio <- data.frame()
      counter <- 1
      #calculate the hazard ratio
      for (names in names(l$independentVars)){
        tempDFMerge <- exp(coefficient[counter])
        hazardRatio <-rbind(hazardRatio, tempDFMerge)
        counter <- counter + 1
      }
      
      #Create data frame to hold the independent variables and hazard ratios for each to be displayed in a plot
      plotFrame <- data.frame(Predictor = names(l$independentVars), HR = hazardRatio[1:nrow(hazardRatio),])
      

      #Data frame containing all the independent variable names
      varFrame <- data.frame(names(l$independentVars))
      
      #Places coefficients into data frame
      coefFrame <- data.frame()
      for (varcof in coefficient){
        coefFrame <- rbind(coefFrame, varcof)
      }

      #Binds variable names, coefficients, and hazard ratio into a data frame to be printed to table
      summaryFrame <- data.frame()
      summaryFrame <- data.frame(Variables = varFrame, Coefficients = coefFrame, Hazard_Ratios = hazardRatio)
      colnames(summaryFrame) <- c("Variables", "Coefficients", "Hazard Ratios")

      output$plot <- renderPlot({
        if (is.null(l$prediction)) return (NULL)
        ggplot(plotFrame, aes(x=Predictor, y = HR)) +
          geom_point()
          
      })
      l$coefficients <- coefficients 
      output$table <- renderText({
         if (is.null(l$prediction)) return (NULL)
         paste(capture.output(print(summaryFrame, row.names = FALSE)), collapse = "\n")
        
      })
    
      
      output$summary <- renderPrint({ #Data check, prints summary of predictions to UI
        if (is.null(l$prediction)) return (NULL)
        # If hazard ratio == 1 no correlation, if > 1 increased risk, <1 decreased risk
        varCount <- 1
        for (variable in 1:nrow(hazardRatio)){
          if (hazardRatio[variable, ] > 1){
            cat(names(l$independentVars)[varCount], " has a hazard ratio of ", hazardRatio[variable, ], " which is greater than 1, indicating an increased risk", "\n")
          }
          else if (hazardRatio[variable, ] < 1){
            cat(names(l$independentVars)[varCount], " has a hazard ratio of ", hazardRatio[variable, ], " which is less than 1, indicating a decreased risk", "\n")
          }
          else{
            cat(names(l$independentVars)[varCount], " has a hazard ratio of ", hazardRatio[variable, ], " which indicates no correlation with risk", "\n")
          }
          varCount <- varCount + 1
        } 
      })
    }
    #Logistic Regression: Models probability of a dependent event occurring based on independent variables
    else if (l$analysisType == "Logistic Regression"){
      formula <- formula(paste(names(l$dependentVars), " ~ ", paste(names(l$independentVars), collapse = " + ")))
      #Cyclops Implementation
      cyclopsData <- createCyclopsData(formula, modelType = "lr", data = serverFrame)
      cyclopsFit <- fitCyclopsModel(cyclopsData, prior = createPrior("none"))
      #Get coefficients and predictions
      coefficient <- coef(cyclopsFit, rescale = FALSE, ignoreConvergence = TRUE) 
      l$coefficient <- coefficient
      prediction <- predict(cyclopsFit)
      l$prediction <- prediction
      countID <- 1
      #Generate data frames containing all instances of true and false predictions
      yes_frame <- data.frame(Instance = numeric(0), Confidence_Percentage = numeric(0))
      no_frame <- data.frame(Instance = numeric(0), Confidence_Percentage = numeric(0))
      for (variable in 1:nrow(serverFrame)){
        if (l$prediction[countID] >= .5){
          yes_frame[nrow(yes_frame)+1, ] <- c(Instance = countID, Confidence_Percentage = l$prediction[countID])
        }
        else{
          no_frame[nrow(no_frame)+1, ] <- c(Instance = countID, Confidence_Percentage = l$prediction[countID])
        }
        countID <- countID + 1
      }
      output$summary <- renderPrint({ #Data check
        if (is.null(l$prediction)) return (NULL)
        strOut <- "There are "
        strOut <- cat(strOut, nrow(yes_frame), " True predictions and ", nrow(no_frame), " False predictions")
        countVar <- 1
        for (var in names(l$independentVars)){
          if (l$coefficient[countVar] > 0 ){
            strOut <- cat("\n", var, " has a positive correlation with ", names(l$dependentVars[1]))
          }
          else{
            strOut <- cat("\n", var, " does not seem to be correlated with ", names(l$dependentVars[1]))
          }
          countVar <- countVar + 1
        }
      })
      output$plot <- renderPlot({
        if (is.null(l$prediction)) return (NULL)
        combined_frame <- rbind(yes_frame, no_frame)
        combined_frame$Predictions <- c(rep("True Prediction", nrow(yes_frame)), rep("False Prediction", nrow(no_frame)))
        ggplot(data = combined_frame, aes(x=Predictions, y = Confidence_Percentage)) + geom_boxplot() + geom_jitter()
      })
      output$table <- renderText({
        if (is.null(l$prediction)) return (NULL)
        paste("True Predictions:", capture.output(print(yes_frame, row.names = FALSE)), collapse = "\n")
         })
      output$table2 <- renderText({
        if (is.null(l$prediction)) return (NULL)
        paste("False Predictions:", capture.output(print(no_frame, row.names = FALSE)), collapse = "\n")
        
      })
    }
  })
  #Mostly used for debugging purposes atm, returns user input to UI to be displayed
  output$text <- renderPrint({
    if (is.null(l$variable1)) return (NULL)
    if (is.null(l$variable2)) return (NULL)
    paste('Variable 1:', l$variable1, 'Variable 2:', l$variable2)
    
  })
  
  },
  warning = function(warn){
    showNotification(paste(warn), type = 'warning')
  },
  error = function(err){
    showNotification(paste(err), type = 'err')
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
