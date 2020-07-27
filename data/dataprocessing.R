#Jessica Speer
#ST 558
#Purpose: Explore the data and set up the static code to be used in the shiny app

library(dplyr)
library(tidyverse)
library(caret)

#Read and limit data
data=read.table("data/student-por.csv",sep=";",header=TRUE)
data <- data %>% select(-c(G1, G2))

#logistic regression
mod1<-glm(sex~school + studytime + activities + romantic +
            famrel + freetime + goout + Walc, data=data, family="binomial")
summary(mod1)

#KNN
set.seed(1)
train <- sample(1:nrow(data), size = nrow(data)*0.8)
test <- dplyr::setdiff(1:nrow(data), train)
dataTrain <- data[train, ]
dataTest <- data[test, ]

trctrl <- trainControl(method = "repeatedcv", number = 3, repeats = 1)
set.seed(16)
knn_fit <- train(sex ~ school + studytime + activities + romantic +
                   famrel + freetime + goout + Walc, data = dataTrain, method = "knn", 
                   trControl=trctrl, preProcess = c("center", "scale"))
plot(knn_fit)
knn_fit
names(knn_fit)
knn_fit$results
#make predictions on test data
test_pred <- predict(knn_fit, newdata = dataTest)
res <- confusionMatrix(test_pred, dataTest$sex)
res
#misclassification rate
1-sum(diag(res$table))/sum(res$table)

rf_fit <- train(sex ~ school + studytime + activities + romantic +
                   famrel + freetime + goout + Walc, data = dataTrain, method = "rf", 
                 trControl=trctrl, preProcess = c("center", "scale"), tuneLength=20)
plot(rf_fit)
rf_fit
#make predictions on test data
test_pred <- predict(rf_fit, newdata = dataTest)
res <- confusionMatrix(test_pred, dataTest$sex)
res
#misclassification rate
1-sum(diag(res$table))/sum(res$table)


log1<-train(sex ~ . -Walc -school, data = dataTrain, 
             trControl=trctrl,method="glm",family=binomial())

summary(log1)

test_pred <- predict(log1, newdata = dataTest)
res <- confusionMatrix(test_pred, dataTest$sex)
res
#misclassification rate
1-sum(diag(res$table))/sum(res$table)


#PCA
PCs <- prcomp(select(data, studytime, famrel, freetime, goout, Walc) , center=T, scale = F)
PCs
biplot(PCs, xlabs = rep(".", nrow(data)), cex = 1.2)
screeplot(PCs, type = "lines")

par(mfrow = c(1, 2))
plot(PCs$sdev^2/sum(PCs$sdev^2), xlab = "Principal Component", 
     ylab = "Proportion of Variance Explained", ylim = c(0, 1), type = 'b')
plot(cumsum(PCs$sdev^2/sum(PCs$sdev^2)), xlab = "Principal Component", 
     ylab = "Cum. Prop of Variance Explained", ylim = c(0, 1), type = 'b')


#Logistic Regression Model box        
box(
  checkboxInput("logOptVars", h4("Choose predictor variables to include")),
  conditionalPanel(condition = "input.logOptVars == true",
                   uiOutput("predictors")
  ),
  width=11,verbatimTextOutput("knnprint")
) 

output$predictors <- renderUI({
  checkboxGroupInput("predictors", "Predictor Variables:", choices = c("school", "studytime"))
})




