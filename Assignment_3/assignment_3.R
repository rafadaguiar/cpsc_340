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
predictClean <- predict(MClean, newdata = test, type = "response") 

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

sensitivity=c()
specifity=c()

for(i in seq(0.1, 0.9, by=0.1)){
  sensitivity <- c(sensitivity,perf(i,MClean,trainNoNA$default10yr)[1])
  specifity <- c(specifity,perf(i,MClean,trainNoNA$default10yr)[2])
}

#find the cutoff
plot(seq(0.1, 0.9, by=0.1), sensitivity , type="l", lty=1, col="blue", xlab="cutoff points", ylab="sensetivity/specifity", lwd=2)
lines(seq(0.1, 0.9, by=0.1),specifity, type="l", lty=1, col="red", lwd=2)
