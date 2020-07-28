#Jessica Speer
#ST 558
#Purpose: Create R Shiny App for project 3

library(shiny)
library(shinydashboard)
library(RColorBrewer)
library(caret)
library(tidyverse)
library(caret)
library(dplyr)
library(ggplot2)

ui <- dashboardPage(
  dashboardHeader(title = "Project3 Shiny App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Intro", tabName = "intro"),
      menuItem("Data Exploration", tabName = "dataex"),
      menuItem("Princpal Components Analysis", tabName = "pca"),
      menuItem("Model (kNN)", tabName = "knn"),
      menuItem("Model (Logistic)", tabName = "logistic"),
      menuItem("View Data", tabName = "viewdata")
    )
  ),
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "intro",
      #Introduction
        h2("Introduction"),
        h4(em("P. Cortez and A. Silva. Using Data Mining to Predict Secondary School Student 
              Performance. In A. Brito and J. Teixeira Eds., Proceedings of 5th FUture BUsiness 
              TEChnology Conference (FUBUTEC 2008) pp. 5-12, Porto, Portugal, April, 2008, 
              EUROSIS, ISBN 978-9077381-39-7.")),
        h4(uiOutput("link")),
        h4(paste(" "), strong("From the authors:"), "'This data approach student achievement in secondary education of two 
            Portuguese schools. The data attributes include student grades, demographic, social 
            and school related features) and it was collected by using school reports and 
            questionnaires. Two datasets are provided regarding the performance in two distinct 
            subjects: Mathematics and Portuguese language'"),
        h3("Data Used in the App"),
        h4(paste("In this app, we use the variable"), code("sex"), "as the outcome variable of
           interest. For consistency, this app uses the naming conventions defined by the authors.
           We used the Portuguese Subject data set, and selected a subset of 
           variables, which are described below.", sep=" "),
        h5(paste(" "), code("sex"), "- student's sex (F-female or M-male)", sep=" "),
        h5(paste(" "), code("school"), "student's school (GP-Gabriel Pereira or MS-Mousinho da Silveira)", sep=" "),
        h5(paste(" "), code("studytime"), "- weekly study time (1: <2 hours, 2: 2 to 5 hours, 3: 5 to 10 hours, or 4: >10 hours)", sep=" "),
        h5(paste(" "), code("activities"), "- extra-curricular activities (yes or no)", sep=" "),
        h5(paste(" "), code("romantic"), "- with a romantic relationship (yes or no)", sep=" "),
        h5(paste(" "), code("famrel"), "- quality of family relationships (1: very bad to 5: excellent)", sep=" "),
        h5(paste(" "), code("freetime"), "- free time after school (1: very low to 5: very high)", sep=" "),
        h5(paste(" "), code("goout"), "- going out with friends (1: very low to 5: very high)", sep=" "),
        h5(paste(" "), code("Walc"), "- weekend alcohol consumption (1: very low to 5: very high)", sep=" "),
        h3("Capabilities of the App"),
        h5(paste(" "), em("Data Exploration"), "- allows the user to select variables to view bar graphs, crosstabs, and means
         for each respective variable. Additionally, the user may click on the plots to determine count values.", sep=" "),
        h5(paste(" "), em("Principal Components Analysis"), "- allows the user to choose to center and/or scale the data, and allows a
         selection between 2 plot types.", sep=" "),
        h5(paste(" "), em("k-Nearest Neighbor"), "- allows the user to adjust the tuneLength parameter, select an option to display
         a plot of the tuning parameter (k), and make predictions.", sep=" "),
        h5(paste(" "), em("Logistic Regression"), "- allows the user to view 2 different types of output and to select the predictor
        variables to be included in the model.", sep=" "),
        h5(paste(" "), em("View Data"), "- allows the user to view the data and choose between the full data, a subset
           based on", code("sex"), "or between the Training and Test data used for the supervised
           models.", sep=" ")
        ),
      
      tabItem(tabName = "dataex",
      #data exploration 
      h2("Data Exploration"),
      h4("This page allows the user to select variables to view bar graphs, crosstabs, and means
         for each respective variable (note: means are not calculated for binary variables.
         Additionally, the user may click on the plots to determine count values (this initializes 
         after first click on plot)"),
      selectizeInput("barvar", "Choose Variable:", selected = "sex", 
                     choices = c("sex", "school", "studytime", "activities", "romantic",
                                 "famrel", "freetime", "goout", "Walc")),
      box(title = "Crosstabs", height=250,
          verbatimTextOutput("tabs")),
      box(title = "Means (Female)",
        verbatimTextOutput("Fmeans")),
      box(title = "Means (Male)",
        verbatimTextOutput("Mmeans")),
      h3(textOutput("clicki")),
      plotOutput("barplot", width=800, click="plot_click")
      ),

      tabItem(tabName = "pca",
      #PCA
      h2("Principal Components Analaysis"),
      h4("This page allows the user to choose to center and/or scale the data, and allows a
         selection between 2 plot types."),
      checkboxInput("center", h4("Center Variables"), value=TRUE),
      checkboxInput("scale", h4("Scale Variables"), value=TRUE),
        conditionalPanel(condition = "input.logOptVars == true", uiOutput("pcavars")
        ),
        verbatimTextOutput("pcares"),
      selectizeInput("pcapick", "Choose plot:", selected = "Biplot", 
                     choices = c("Biplot", "Prop. of Variance Explained")),
        plotOutput("pcaplot", width=600)
      ),
      
      tabItem(tabName = "knn",
      #KNN Model
      h2("k-Nearest Neighbors"),
      h4("This page allows the user to adjust the tuneLength parameter, select an option to display
         a plot of the tuning parameter (k), and make predictions. The default model (no tuneLength
         specified) is optimized at k=9. Thus, this is the model used to make predictions."),
       checkboxInput("knnOptTL", h4("Adjust tuneLength")),
         conditionalPanel(condition = "input.knnOptTL == true",
           sliderInput("knnSetTL", "Choose value for tuneLength", min = 1, max = 20, 
                                    value=10, step = 1)
       ),
       verbatimTextOutput("knnprint"),
       checkboxInput("knnpchk", h4("Show plot of k values")),
         conditionalPanel(condition = "input.knnpchk == true",
           plotOutput("knnplot", width=600)
       ),
       checkboxInput("knnpred", h4("Enter values to make predictions (optimized model)")),
         conditionalPanel(condition = "input.knnpred == true",
           selectizeInput("schoolp", "School", selected = "GP", 
                          choices = c("GP", "MS")
           ),
           selectizeInput("activp", "Activities", selected = "yes", 
                          choices = c("yes", "no")
           ),
           selectizeInput("romp", "Romantic partnership", selected = "yes", 
                          choices = c("yes", "no")
           ),
           sliderInput("studyp", "Study Time", min = 1, max = 4, value=2, step = 1),
           sliderInput("famp", "Quality of Family Relationship", min = 1, max = 5, 
                       value=2, step = 1),
           sliderInput("freep", "Free Time", min = 1, max = 5, value=2, step = 1),
           sliderInput("outp", "Out with Friends (Frequency)", min = 1, max = 5, 
                       value=2, step = 1),
           sliderInput("walcp", "Weekend Alcohol Consumption", min = 1, max = 5, 
                       value=2, step = 1),
           h2(textOutput("knnpred"))
         )
      ),
      
     tabItem(tabName = "logistic",
     #Logistic Regression
     h2("Logistic Regression"),
     h4("This page allows the user to view 2 different types of output and to select the predictor
        variables to be included in the model. This default model/output is the optimized model with
        all variables included. As a reminder, here is the form of the logistic model:"),
     h3(withMathJax(helpText("$$ln(p/1-p) = \\beta_0x_0 + \\beta_1x_1 + \\beta_2x_2 + ... + \\beta_nx_n$$"))),
      selectizeInput("logop", "Output options:", selected = "Model Summary", 
                     choices = c("Model Summary", "Training Summary")
      ),
      verbatimTextOutput("logsum"),
      checkboxInput("logOptVars", h4("Choose predictor variables to include")),
        conditionalPanel(condition = "input.logOptVars == true", uiOutput("predictors")
      )
      ),
     
     tabItem(tabName = "viewdata",
     #View data
     h2("View Data"),
     h4(paste("This page allows the user to view the data and choose between the full data, a subset
        based on"), code("sex"), "or between the Training and Test data used for the supervised
        models.", sep=" "),
       selectizeInput("subset", "Choose full or subset of data:", selected = "Full", 
                            choices = c("Full", "Females", "Males", "Training", "Test")),
     tableOutput("table")
     )
     
     
     
    )
  )
)  

server <- function(input, output, session) {
  
  #get and limit data
  getData <- reactive({
    data=read.table("data/student-por.csv",sep=";",header=TRUE)
    data <- data %>% select(sex, school, studytime, activities, romantic, famrel,
                            freetime, goout, Walc)
  })
  
  #set up URL for hyperlink
  url <- a("Data available here", href="https://archive.ics.uci.edu/ml/datasets/Student+Performance")
  
  #output hyperlink
  output$link <- renderUI({
    tagList(url)
  })
  
  #output kNN results  
  output$knnprint <- renderPrint({
    data <- getData()
    
    trctrl <- trainControl(method = "repeatedcv", number = 3, repeats = 1)
    
    if(input$knnOptTL){
      knn_fit <- train(sex ~ school + studytime + activities + romantic +
                       famrel + freetime + goout + Walc, data = data, method = "knn", 
                       trControl=trctrl, preProcess = c("center", "scale"), 
                       tuneLength=input$knnSetTL)
      knn_fit
    }
    else{
      knn_fit <- train(sex ~ school + studytime + activities + romantic +
                     famrel + freetime + goout + Walc, data = data, method = "knn", 
                     trControl=trctrl, preProcess = c("center", "scale"))
      knn_fit
      }
  })
  
  #output kNN plot
  output$knnplot <- renderPlot({
    data <- getData()
    
    trctrl <- trainControl(method = "repeatedcv", number = 3, repeats = 1)
    
    if(input$knnOptTL){
      knn_fit <- train(sex ~ school + studytime + activities + romantic +
                         famrel + freetime + goout + Walc, data = data, method = "knn", 
                       trControl=trctrl, preProcess = c("center", "scale"), 
                       tuneLength=input$knnSetTL)
      plot(knn_fit)
    }
    else{
      knn_fit <- train(sex ~ school + studytime + activities + romantic +
                         famrel + freetime + goout + Walc, data = data, method = "knn", 
                       trControl=trctrl, preProcess = c("center", "scale"))
      plot(knn_fit)
    }
  })

  #output kNN predictions
  output$knnpred <- renderText({
    data <- getData()
    
    trctrl <- trainControl(method = "repeatedcv", number = 3, repeats = 1)
    
    knn_fit <- train(sex ~ school + studytime + activities + romantic +
                         famrel + freetime + goout + Walc, data = data, method = "knn", 
                       trControl=trctrl, preProcess = c("center", "scale"))
    new.obs <- data.frame(school=input$schoolp, studytime=input$studyp, activities=input$activp,
                            romantic=input$romp, famrel=input$famp, freetime=input$freep,
                            goout=input$outp, Walc=input$walcp)
    
    test_pred <- predict(knn_fit, newdata = new.obs)
    
    paste("The model predicts:", test_pred, sep = " ")
  })
  
  #Dynamic UI for variable selection (logistic)
  output$predictors <- renderUI({
    checkboxGroupInput("predictors", "Predictor Variables:", choices = c("school", "studytime", "activities", "romantic",
                       "famrel", "freetime", "goout", "Walc"), selected="school")
  })
  
  #output logistic regression output
  output$logsum <- renderPrint({
    data <- getData()
    
    trctrl <- trainControl(method = "repeatedcv", number = 3, repeats = 1)
    
    if(input$logOptVars & !is.null(input$predictors)){
      
      log1<-train(as.formula(paste("sex","~",paste(input$predictors,collapse="+"))), data = data, 
                trControl=trctrl,method="glm",family=binomial())
    
    
      if(input$logop=="Model Summary"){
        summary(log1)
      }
      else{
        log1 
      }
    }
    else{
      log1<-train(sex ~ school + studytime + activities + romantic +
                    famrel + freetime + goout + Walc, data = data, 
                  trControl=trctrl,method="glm",family=binomial())
      
      if(input$logop=="Model Summary"){
        summary(log1)
      }
      else{
        log1 
      }
    }
  })
  
  #Dynamic UI for variable selection (PCA)
  output$pcavars <- renderUI({
    checkboxGroupInput("pcavars", "Variables:", choices = c("studytime","famrel", "freetime", "goout", "Walc"))
  })
  
  #output PCA plots
  output$pcaplot <- renderPlot({  
    data<-getData()
      PCs <- prcomp(select(data, studytime, famrel, freetime, goout, Walc) , center=input$center, scale =input$scale)
      if(input$pcapick=="Biplot"){
        biplot(PCs, xlabs = rep(".", nrow(data)), cex = 1.2)
      }
      else{
        par(mfrow = c(1, 2))
        plot(PCs$sdev^2/sum(PCs$sdev^2), xlab = "Principal Component", 
             ylab = "Proportion of Variance Explained", ylim = c(0, 1), type = 'b')
        plot(cumsum(PCs$sdev^2/sum(PCs$sdev^2)), xlab = "Principal Component", 
             ylab = "Cum. Prop of Variance Explained", ylim = c(0, 1), type = 'b')
      }
    
  })
  
  #output PCA results
  output$pcares <- renderPrint({  
    data<-getData()
    
    PCs <- prcomp(select(data, studytime, famrel, freetime, goout, Walc) , center=input$center, scale =input$scale)
    PCs
    
  }) 

  #output barplots
  #I hit an error trying to use input$barvar so had to take a more manual approach
  output$barplot <- renderPlot({  
    data<-getData()
     if(input$barvar=="sex"){
       g <- ggplot(data, aes(x=sex))
       g + geom_bar(aes(fill = data$sex)) +  scale_fill_brewer(palette="Accent")
     }
     else if(input$barvar=="school"){
       g <- ggplot(data, aes(x=school))
       g + geom_bar(aes(fill = data$sex)) + scale_fill_brewer(palette="Accent")
     }
     else if(input$barvar=="studytime"){
       g <- ggplot(data, aes(x=studytime))
       g + geom_bar(aes(fill = data$sex)) + scale_fill_brewer(palette="Accent")
     }
     else if(input$barvar=="activities"){
       g <- ggplot(data, aes(x=activities))
       g + geom_bar(aes(fill = data$sex)) + scale_fill_brewer(palette="Accent")
     }
     else if(input$barvar=="romantic"){
       g <- ggplot(data, aes(x=romantic))
       g + geom_bar(aes(fill = data$sex)) + scale_fill_brewer(palette="Accent")
     }
     else if(input$barvar=="famrel"){
       g <- ggplot(data, aes(x=famrel))
       g + geom_bar(aes(fill = data$sex)) + scale_fill_brewer(palette="Accent")
     }
     else if(input$barvar=="freetime"){
       g <- ggplot(data, aes(x=freetime))
       g + geom_bar(aes(fill = data$sex)) + scale_fill_brewer(palette="Accent")
     }
     else if(input$barvar=="goout"){
       g <- ggplot(data, aes(x=goout))
       g + geom_bar(aes(fill = data$sex)) + scale_fill_brewer(palette="Accent")
     }
     else if(input$barvar=="Walc"){
       g <- ggplot(data, aes(x=Walc))
       g + geom_bar(aes(fill = data$sex)) + scale_fill_brewer(palette="Accent")
     }
}) 

  #Female means
  output$Fmeans <- renderPrint({  
    data<-getData()
    if(input$barvar=="sex"){
      paste("NA")
    }
    else if(input$barvar=="school"){
      paste("NA")
    }
    else if(input$barvar=="studytime"){
      mean(data$studytime[data$sex=="F"])
    }
    else if(input$barvar=="activities"){
      mean(data$activities[data$sex=="F"])
    }
    else if(input$barvar=="romantic"){
      print("NA")
    }
    else if(input$barvar=="famrel"){
      mean(data$famrel[data$sex=="F"])  
    }
    else if(input$barvar=="freetime"){
      mean(data$famrel[data$sex=="F"]) 
    }
    else if(input$barvar=="goout"){
      mean(data$freetime[data$sex=="F"]) 
    }
    else if(input$barvar=="Walc"){
      mean(data$Walc[data$sex=="F"])
    }
  }) 
  
  #Plot click functionality
  output$clicki <- renderText({
    
    y_str <- function(e) {
      if(is.null(e)) return("NULL\n")
      paste0(round(e$y, 1), "\n")
    }
    
    paste("Count =", y_str(input$plot_click), sep=" ")
    
  })
  
  #Male means
  output$Mmeans <- renderPrint({  
    data<-getData()
    if(input$barvar=="sex"){
      paste("NA")
    }
    else if(input$barvar=="school"){
      paste("NA")
    }
    else if(input$barvar=="studytime"){
      mean(data$studytime[data$sex=="M"])
    }
    else if(input$barvar=="activities"){
      mean(data$activities[data$sex=="M"])
    }
    else if(input$barvar=="romantic"){
      print("NA")
    }
    else if(input$barvar=="famrel"){
      mean(data$famrel[data$sex=="M"])  
    }
    else if(input$barvar=="freetime"){
      mean(data$famrel[data$sex=="M"]) 
    }
    else if(input$barvar=="goout"){
      mean(data$freetime[data$sex=="M"]) 
    }
    else if(input$barvar=="Walc"){
      mean(data$Walc[data$sex=="M"])
    }
  })
  
  #crosstabs
  output$tabs <- renderPrint({  
    data<-getData()
    if(input$barvar=="sex"){
      table(data$sex)
    }
    else if(input$barvar=="school"){
      table(data$school, data$sex)
    }
    else if(input$barvar=="studytime"){
      table(data$studytime, data$sex)
    }
    else if(input$barvar=="activities"){
      table(data$activities, data$sex)
    }
    else if(input$barvar=="romantic"){
      table(data$romantic, data$sex)
    }
    else if(input$barvar=="famrel"){
      table(data$famrel, data$sex) 
    }
    else if(input$barvar=="freetime"){
      table(data$freetime, data$sex)  
    }
    else if(input$barvar=="goout"){
      table(data$goout, data$sex) 
    }
    else if(input$barvar=="Walc"){
      table(data$Walc, data$sex)
    }
  })
  
  #Output data   
  output$table <- renderTable({
    if(input$subset=="Full"){  
      getData()
    }
    else if(input$subset=="Females"){  
      data<-getData()
      data[data$sex=="F",]
    }
    else if(input$subset=="Males"){  
      data<-getData()
      data[data$sex=="M",]
    }
  })


}

shinyApp(ui, server)