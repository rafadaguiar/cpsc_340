library(caret)
setwd("~/Workspace/cpsc_340/Assignment_3")

train <- read.csv("data/Assign3trainMissingValues.csv")
test <- read.csv("data/Assign3Test.csv")
trueValues <- read.csv("data/Assign3TrueValues.csv")

#summary(train)

# Categorizing data
train$default10yr <- factor(train$default10yr)
test$default10yr <- factor(test$default10yr)

# Removing loan = NA samples from train dataset
trainNoNA <- na.omit(train)

# Training model
MClean <- glm(default10yr ~ income+age+loan, data=trainNoNA, family="binomial")

# Assign probabilities to a class value depending on the cutoff
perf = function(cut, mod, y)
{
  yhat = (mod$fit>cut)
  w = which(y==1)
  sensitivity = mean( yhat[w] == 1 ) 
  specificity = mean( yhat[-w] == 0 ) 
  out = t(as.matrix(c(sensitivity, specificity)))
  colnames(out) = c("sensitivity", "specificity")
  return(out)
}
best.cutoff = function(model, dataset){
  # Sensitivity and Specificity
  sensitivity=c()
  specifity=c()
  
  for(i in seq(0.1, 0.9, by=0.1)){
    sensitivity <- c(sensitivity,perf(i,model,dataset$default10yr)[1])
    specifity <- c(specifity,perf(i,model,dataset$default10yr)[2])
  }
  
  # find the cutoff
  plot(seq(0.1, 0.9, by=0.1), sensitivity , type="l", lty=1, col="blue", xlab="cutoff points", ylab="sensetivity/specifity", lwd=2)
  lines(seq(0.1, 0.9, by=0.1),specifity, type="l", lty=1, col="red", lwd=2)
}
# Best Cutoff is 0.2
best.cutoff(MClean, trainNoNA)
cutoff <- 0.2

predictClean <- ifelse(predict(MClean, newdata=test, type="response") > cutoff, 1, 0)
confusionMatrixClean <- table(predictClean, test$default10yr)
cat("Model: ","MClean", "\n")
cat("Sensitivity = ", sensitivity(confusionMatrixClean)," Specificity = ", specificity(confusionMatrixClean))


# Missing Value Imputation

#   Elements of train missing loan values
missLoan = train[is.na(train$loan),]

#   Training linear regression models for loan
RIncome <- lm(loan ~ income, data=train)
pRIncome <- as.data.frame(predict(RIncome, interval="prediction", newdata=missLoan))
#     Residual
# sum(abs(trueValues$loan-pRIncome$fit))/length(trueValues$loan)

RAge <- lm(loan ~ age, data=train)
pRAge <- as.data.frame(predict(RAge, interval="prediction", newdata=missLoan))
#     Residual
# sum(abs(trueValues$loan-pRAge$fit))/length(trueValues$loan)

RBoth <- lm(loan ~ income+age, data=train)
pRBoth <- as.data.frame(predict(RBoth, interval="prediction", newdata=missLoan))
#     Residual
# sum(abs(trueValues$loan-pRBoth$fit))/length(trueValues$loan)

# >>> So RIncome has less residual error.


# Training the models again (now with loan values for all elements in the dataset)
loanInputs <- list(pRIncome$fit, pRAge$fit, pRBoth$fit)
names <- c('RIncome', 'RAge', 'RBoth')
for (i in 1:length(loanInputs)){
  trainAllLoan <- train
  trainAllLoan[is.na(trainAllLoan$loan),'loan'] <- loanInputs[i]
  model <- glm(default10yr ~ income+age+loan, data=trainAllLoan, family="binomial")
  #   best.cutoff(model, trainAllLoan) # 0.2,0.2,0.2
  predict <- ifelse(predict(model, newdata=test, type="response") > 0.2, 1, 0)
  confusionMatrix <- table(predict, test$default10yr)
  cat("Model: ", names[i], "\n")
  cat("Sensitivity = ", sensitivity(confusionMatrix)," Specificity = ", specificity(confusionMatrix),"\n")
}
