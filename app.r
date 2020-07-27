library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Project3 Shiny App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Exploration", tabName = "data"),
      menuItem("Princpal Components Analysis", tabName = "pca"),
      menuItem("Model (kNN)", tabName = "knn"),
      menuItem("Model (Logistic)", tabName = "logistic")
    )
  ),
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "data",
      #data exploration 
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
      checkboxInput("center", h4("Center Variables"), value=TRUE),
      checkboxInput("scale", h4("Scale Variables"), value=TRUE),
        conditionalPanel(condition = "input.logOptVars == true", uiOutput("pcavars")
        ),
        verbatimTextOutput("pcares"),
        plotOutput("biplot", width=600)
      ),
      
      tabItem(tabName = "knn",
      #KNN Model   
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
      selectizeInput("logop", "Output options:", selected = "Model Summary", 
                     choices = c("Model Summary", "Training Summary")
      ),
      verbatimTextOutput("logsum"),
      checkboxInput("logOptVars", h4("Choose predictor variables to include")),
        conditionalPanel(condition = "input.logOptVars == true", uiOutput("predictors")
      )
      )
    )
  )
)  





  




server <- function(input, output, session) {
  
  getData <- reactive({
    data=read.table("data/student-por.csv",sep=";",header=TRUE)
    data <- data %>% select(sex, school, studytime, activities, romantic, famrel,
                            freetime, goout, Walc)
  })
  
  getTrain <- reactive({
    data<-getData()
    set.seed(1)
    train <- sample(1:nrow(data), size = nrow(data)*0.8)
  })
  getTest <- reactive({
    data<-getData()
    train<-getTrain()
    set.seed(1)
    test <- dplyr::setdiff(1:nrow(data), train)
  })
  
  output$knnprint <- renderPrint({
    train <- getTrain()
    dataTrain <- data[train, ]
    
    trctrl <- trainControl(method = "repeatedcv", number = 3, repeats = 1)
    
    if(input$knnOptTL){
      knn_fit <- train(sex ~ school + studytime + activities + romantic +
                       famrel + freetime + goout + Walc, data = dataTrain, method = "knn", 
                       trControl=trctrl, preProcess = c("center", "scale"), 
                       tuneLength=input$knnSetTL)
      knn_fit
    }
    else{
      knn_fit <- train(sex ~ school + studytime + activities + romantic +
                     famrel + freetime + goout + Walc, data = dataTrain, method = "knn", 
                     trControl=trctrl, preProcess = c("center", "scale"))
      knn_fit
      }
  })
  
  output$knnplot <- renderPlot({
    train <- getTrain()
    dataTrain <- data[train, ]
    
    trctrl <- trainControl(method = "repeatedcv", number = 3, repeats = 1)
    
    if(input$knnOptTL){
      knn_fit <- train(sex ~ school + studytime + activities + romantic +
                         famrel + freetime + goout + Walc, data = dataTrain, method = "knn", 
                       trControl=trctrl, preProcess = c("center", "scale"), 
                       tuneLength=input$knnSetTL)
      plot(knn_fit)
    }
    else{
      knn_fit <- train(sex ~ school + studytime + activities + romantic +
                         famrel + freetime + goout + Walc, data = dataTrain, method = "knn", 
                       trControl=trctrl, preProcess = c("center", "scale"))
      plot(knn_fit)
    }
  })

  output$knnpred <- renderText({
    train <- getTrain()
    test <- getTest()
    dataTrain <- data[train, ]
    dataTest <- data[test, ]
    
    trctrl <- trainControl(method = "repeatedcv", number = 3, repeats = 1)
    
    knn_fit <- train(sex ~ school + studytime + activities + romantic +
                         famrel + freetime + goout + Walc, data = dataTrain, method = "knn", 
                       trControl=trctrl, preProcess = c("center", "scale"))
    new.obs <- data.frame(school=input$schoolp, studytime=input$studyp, activities=input$activp,
                            romantic=input$romp, famrel=input$famp, freetime=input$freep,
                            goout=input$outp, Walc=input$walcp)
    
    test_pred <- predict(knn_fit, newdata = new.obs)
    
    paste("The model predicts:", test_pred, sep = " ")
  })
  
  output$predictors <- renderUI({
    checkboxGroupInput("predictors", "Predictor Variables:", choices = c("school", "studytime", "activities", "romantic",
                       "famrel", "freetime", "goout", "Walc"), selected="school")
  })
  
  output$logsum <- renderPrint({
    train <- getTrain()
    test <- getTest()
    dataTrain <- data[train, ]
    
    trctrl <- trainControl(method = "repeatedcv", number = 3, repeats = 1)
    
    if(input$logOptVars & !is.null(input$predictors)){
      
      log1<-train(as.formula(paste("sex","~",paste(input$predictors,collapse="+"))), data = dataTrain, 
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
                    famrel + freetime + goout + Walc, data = dataTrain, 
                  trControl=trctrl,method="glm",family=binomial())
      
      if(input$logop=="Model Summary"){
        summary(log1)
      }
      else{
        log1 
      }
    }
  })
  
  output$pcavars <- renderUI({
    checkboxGroupInput("pcavars", "Variables:", choices = c("studytime","famrel", "freetime", "goout", "Walc"))
  })
  
  output$biplot <- renderPlot({  
    data<-getData()
      
      PCs <- prcomp(select(data, studytime, famrel, freetime, goout, Walc) , center=input$center, scale =input$scale)
      biplot(PCs, xlabs = rep(".", nrow(data)), cex = 1.2)
    
  })
  
  output$pcares <- renderPrint({  
    data<-getData()
    
    PCs <- prcomp(select(data, studytime, famrel, freetime, goout, Walc) , center=input$center, scale =input$scale)
    PCs
    
  }) 

output$barplot <- renderPlot({  
  data<-getData()
  if(input$barvar=="sex"){
    g <- ggplot(data, aes(x=sex))
    g + geom_bar(aes(fill = data$sex))
  }
  else if(input$barvar=="school"){
    g <- ggplot(data, aes(x=school))
    g + geom_bar(aes(fill = data$sex))
  }
  else if(input$barvar=="studytime"){
    g <- ggplot(data, aes(x=studytime))
    g + geom_bar(aes(fill = data$sex))
  }
  else if(input$barvar=="activities"){
    g <- ggplot(data, aes(x=activities))
    g + geom_bar(aes(fill = data$sex))
  }
  else if(input$barvar=="romantic"){
    g <- ggplot(data, aes(x=romantic))
    g + geom_bar(aes(fill = data$sex))
  }
  else if(input$barvar=="famrel"){
    g <- ggplot(data, aes(x=famrel))
    g + geom_bar(aes(fill = data$sex))
  }
  else if(input$barvar=="freetime"){
    g <- ggplot(data, aes(x=freetime))
    g + geom_bar(aes(fill = data$sex))
  }
  else if(input$barvar=="goout"){
    g <- ggplot(data, aes(x=goout))
    g + geom_bar(aes(fill = data$sex))
  }
  else if(input$barvar=="Walc"){
    g <- ggplot(data, aes(x=Walc))
    g + geom_bar(aes(fill = data$sex))
  }
}) 

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

output$clicki <- renderText({
  
  y_str <- function(e) {
    if(is.null(e)) return("NULL\n")
    paste0(round(e$y, 1), "\n")
  }
  
  paste("Frequency =", y_str(input$plot_click), sep=" ")
  
})

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


}

shinyApp(ui, server)