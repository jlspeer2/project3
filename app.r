library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Project3 Shiny App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Model (kNN)", tabName = "knn"),
      menuItem("Model (Logistic)", tabName = "logistic")
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
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
                plotOutput("knnplot")
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
             checkboxInput("logOptVars", h4("Choose predictor variables to include")),
      conditionalPanel(condition = "input.logOptVars == true", uiOutput("predictors")
      ),
      verbatimTextOutput("logsum")
      )
    )
  )
)  





  




server <- function(input, output) {
  
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
    checkboxGroupInput("predictors", "Predictor Variables:", choices = c("school", "studytime"), selected="school")
  })
  
  output$logsum <- renderPrint({
    train <- getTrain()
    test <- getTest()
    dataTrain <- data[train, ]
    
    trctrl <- trainControl(method = "repeatedcv", number = 3, repeats = 1)
    
    if(input$logOptVars){
      
      log1<-train(as.formula(paste("sex","~",paste(input$predictors,collapse="+"))), data = dataTrain, 
                trControl=trctrl,method="glm",family=binomial())
    
    summary(log1)
    }
    else{
      log1<-train(sex ~ school + studytime + activities + romantic +
                    famrel + freetime + goout + Walc, data = dataTrain, 
                  trControl=trctrl,method="glm",family=binomial())
      
      summary(log1)
    }
  })
  
}

shinyApp(ui, server)