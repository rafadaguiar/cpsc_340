# install.packages('lm') WE DON'T NEED TO ON THE R 3.1.1
# library(lm)

setwd("~/Workspace/cpsc_340/tutorials/lab3/data")
train<- read.table("age-height-train.csv", sep = ",", header = TRUE)
test <- read.table("age-height-test.csv", sep = ",", header = TRUE)
train
plot(train$x, train$y,ylab='Height in meters',xlab='Age in years')

lr_model = lm(y~x,data=train)
lr_model
summary(lr_model)
abline(lr_model,col="green")

prediction <- predict(lr_model,interval="prediction",newdata=test)
prediction
prediction <- as.data.frame(prediction)

points(test$x,test$y,col="red")
points(test$x,prediction$fit,col="blue",pch=17)

cbind(test$y , prediction$fit)


########### PART 2 ##############

train <- read.table("housing-train.csv",sep = ",",header=TRUE)
test <- read.table("housing-test.csv",sep = ",",header=TRUE)

plot(train$price, train$area,xlab='Price',ylab='Area')
plot(train$price, train$bedrooms,xlab='Price',ylab='Number of bedrooms')

model <- lm(price ~ area + bedrooms, data=train)
model
summary(model)
plot(model)

#The first plot gives an idea of whether there is any curvature in the data. If the red line is strongly curved, a quadratic or other model may be better. 
#  (less important at this stage) The second plot is to check whether the residuals (y-y^) are normally distributed. 

#The third plot is used to check if the variance is contant (ie, if the standard deviation among the residuals appears to be about constant). If the red line is strongly tilted up/down, that is a red flag.
# (less important at this stage) The last plot is used to check to see if there were any overly influential points 

hist(resid(model))

prediction <- predict(model,interval="prediction",newdata=test)
prediction <- as.data.frame(prediction)
# view the real and predicted prices
cbind(test$price , prediction$fit)

sum(abs(test$price-prediction$fit))/length(test$price)

# optional: use scale() function
feature.scale = function (dataset, columns) {
  for (column in columns) {
    sigma = sd(dataset[,column])
    mu = mean(dataset[,column])
    dataset[paste(names(dataset[column]), ".scale", sep = "")] = (dataset[,column] - mu)/sigma
  }
  return(dataset)
}

train.scaled <- feature.scale(train, c("area", "bedrooms"))
test.scaled <- feature.scale(test, c("area", "bedrooms"))
# model.scale <- lm(price ~ scale(area) + scale(bedrooms), data=train)
model.scale <- lm(price ~ area.scale + bedrooms.scale, data=train.scaled)
model.scale
plot(model.scale)
hist(resid(model.scale))
prediction.scale <- predict(model.scale,interval="prediction",newdata=test.scaled)
prediction.scale <- as.data.frame(prediction.scale)
# view the real and predicted prices
cbind(test$price , prediction.scale$fit)

# The residuals average:
sum(abs(test$price-prediction.scale$fit))/length(test$price)


cbind(test$price , prediction.scale$fit)

