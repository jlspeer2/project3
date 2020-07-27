library(dplyr)
library(tidyverse)
library(caret)


data=read.table("data/student-por.csv",sep=";",header=TRUE)
data <- data %>% select(-c(G1, G2))

mod1<-glm(sex~school + studytime + activities + romantic +
            famrel + freetime + goout + Walc, data=data, family="binomial")
summary(mod1)

set.seed(1)
train <- sample(1:nrow(data), size = nrow(data)*0.8)
test <- dplyr::setdiff(1:nrow(data), train)
dataTrain <- data[train, ]
dataTest <- data[test, ]

trctrl <- trainControl(method = "repeatedcv", number = 3, repeats = 1)

knn_fit <- train(sex ~ school + studytime + activities + romantic +
                   famrel + freetime + goout + Walc, data = dataTrain, method = "knn", 
                   trControl=trctrl, preProcess = c("center", "scale"))
plot(knn_fit)
#make predictions on test data
test_pred <- predict(knn_fit, newdata = dataTest)
res <- confusionMatrix(test_pred, dataTest$sex)
res
#misclassification rate
1-sum(diag(res$table))/sum(res$table)







