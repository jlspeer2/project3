library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Project3 Shiny App"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Models", tabName = "models"),
      menuItem("intro", tabName = "intro")
    )
  ),
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "models",
              fluidRow(
                
                box(
                  selectizeInput("mod1rep", "Response Variable", selected = "sex", 
                                   choices = c("sex","school", "activities", "romantic")
                  )
                ),
                                     
                box(
                  width=11,verbatimTextOutput("mod1sum")
                ),
                
                box(
                  width=7,
                  checkboxInput("mod1pred", h4("Make predictions using default model (response=sex)")),
                  conditionalPanel(condition = "input.mod1pred == true",
                    selectizeInput("schoolp", "School", selected = "GP", 
                                   choices = c("GP", "MS")
                    ),
                    selectizeInput("activp", "Activities", selected = "yes", 
                                   choices = c("yes", "no")
                    ),
                    selectizeInput("romp", "Romantic partnership", selected = "yes", 
                                   choices = c("yes", "no")
                    ),
                    selectizeInput("romp", "Romantic partnership", selected = "yes", 
                                   choices = c("yes", "no")
                    ),
                    sliderInput("studyp", "Study Time", min = 1, max = 4, value=2, step = 1),
                    sliderInput("famp", "Quality of Family Relationship", min = 1, max = 5, 
                                value=2, step = 1),
                    sliderInput("freep", "Free Time", min = 1, max = 5, value=2, step = 1),
                    sliderInput("freep", "Out with Friends (Frequency)", min = 1, max = 5, 
                                value=2, step = 1),
                    sliderInput("walcp", "Weekend Alcohol Consumption", min = 1, max = 5, 
                                value=2, step = 1)
                  )
                )
              )
      ),
      
      # Second tab content
      tabItem(tabName = "intro",
              h2("Widgets tab content")
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
  
  output$mod1sum <- renderPrint({
    data <- getData()
    if(input$mod1rep=="sex"){
      mod1<-glm(sex~., data=data, family="binomial")
    }
    if(input$mod1rep=="school"){
      mod1<-glm(school~., data=data, family="binomial")
    }
    if(input$mod1rep=="activities"){
      mod1<-glm(activities~., data=data, family="binomial")
    }
    if(input$mod1rep=="romantic"){
      mod1<-glm(romantic~., data=data, family="binomial")
    }
    summary(mod1)
  })
}

shinyApp(ui, server)