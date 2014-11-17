install.packages("randomForest")
library(e1071)
library(randomForest)
setwd("~/Workspace/cpsc_340/tutorials/lab7/data")
training<- read.table("LabNov10-train.csv", sep = ",", header = TRUE)
testing<- read.table("LabNov10-test.csv", sep = ",", header = TRUE)

rf <- randomForest(y~x1+x2+x3,data=training,ntree=10)
predictions.rf<-predict(rf,newdata=testing)
error.rf<-sqrt((sum((testing$y-predictions.rf)^2))/nrow(testing))
error.rf

rf <- randomForest(y~x1+x2+x3,data=training,ntree=100)
predictions.rf<-predict(rf,newdata=testing)
error.rf<-sqrt((sum((testing$y-predictions.rf)^2))/nrow(testing))
error.rf

rm <- glm(y ~ x1 + x2 + x3, data = training)
predictions.rm<-predict(rm,newdata=testing)
error.rm<-sqrt((sum((testing$y-predictions.rm)^2))/nrow(testing))
error.rm

svm <- svm(y~x1+x2+x3,data=training)
predictions.svm<-predict(svm,newdata=testing)
error.svm<-sqrt((sum((testing$y-predictions.svm)^2))/nrow(testing))
error.svm

predictions.svm_rm<-(predictions.svm + predictions.rm)/2
error.svm_rm<-sqrt((sum((testing$y-predictions.svm_rm)^2))/nrow(testing))
error.svm_rm

predictions.svm_rm<-(predictions.svm * 9 + predictions.rm)/10
error.svm_rm<-sqrt((sum((testing$y-predictions.svm_rm)^2))/nrow(testing))
error.svm_rm



