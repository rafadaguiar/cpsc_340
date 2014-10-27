# install.packages("caret")
library(rpart)
library(caret)

setwd("~/Workspace/cpsc_340/Assignment_2")
train_data <- read.csv("data/2014CensusTraining.csv")
train_half_data <- read.csv("data/2014HalfCensusTraining.csv")
test_data <- read.csv("data/2014NewCensusTest.csv")

# Entire data
F5 <- rpart(train_data$class ~ age+workclass+fnlwgt+education+education.num, train_data, method="class")
F10 <- rpart(train_data$class ~ age+workclass+fnlwgt+education+education.num+marital.status+occupation+relationship+race+sex, train_data, method="class")
F14 <- rpart(train_data$class ~ ., train_data, method="class")

# Half Data
H5 <- rpart(train_half_data$class ~ age+workclass+fnlwgt+education+education.num, train_half_data, method="class")
H10 <- rpart(train_half_data$class ~ age+workclass+fnlwgt+education+education.num+marital.status+occupation+relationship+race+sex, train_half_data, method="class")
H14 <- rpart(train_half_data$class ~ ., train_half_data, method="class")

# Predictions
models <- list(F5, F10, F14, H5, H10, H14)
names <- c("F5", "F10", "F14", "H5", "H10", "H14")
for (i in 1:length(models)){
  cat("Model: ", names[i], "\n")
  attribute <- rownames(models[[i]]$split)[1]
  point <- models[[i]]$split[1,"index"]
  
  # Categorial attributes
  if (! is.null(attributes(models[[i]])$xlevels[[attribute]])){
    index <- models[[i]]$csplit[point,]
    point <- attributes(models[[i]])$xlevels[[attribute]][index==1]
  }
  cat("Root split point: ", attribute,"=", point, "\n")
  prediction <- predict(models[[i]], test_data, type="class")
  prediction <- table(test_data$class, prediction)
  #   print (prediction)  
  cat("(Sensitivity, Specificity) = (", sensitivity(prediction), ",", specificity(prediction), ")\n")
  cat("Accuracy = ", (prediction[1,1]+prediction[2,2])/sum(prediction), "\n\n")
}
