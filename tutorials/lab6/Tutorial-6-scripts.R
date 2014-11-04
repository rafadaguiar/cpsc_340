# install.packages("e1071")
set.seed(1)
library(SnowballC)
library(e1071)
library(tm)

setwd("~/Workspace/cpsc_340/tutorials/lab2/epinions")
train<- read.table("epinions-train.csv", sep = ",", header = TRUE)
test<- read.table("epinions-test.csv", sep = ",", header = TRUE)

train_set <- train[sample(nrow(train), 200),]
test_set <- test[sample(nrow(test), 100),]

train_test_set <- rbind(train_set,test_set) 

# Preprocessing
train_test_corpus <- Corpus(VectorSource(train_test_set$text))
train_test_corpus <- tm_map(train_test_corpus, content_transformer(tolower))
train_test_corpus <- tm_map(train_test_corpus, content_transformer(removePunctuation))
train_test_corpus <- tm_map(train_test_corpus, content_transformer(removeWords), c(stopwords('english')))
train_test_corpus <- tm_map(train_test_corpus, content_transformer(stripWhitespace))
DTM_train_test <- DocumentTermMatrix(train_test_corpus, control=list(stemming = TRUE, weighting = weightTfIdf))
# DTM_train_test <- DocumentTermMatrix(train_test_corpus)
train_test_dataframe <- as.data.frame(inspect( DTM_train_test ))

classes <- c(train_test_set$class)
train_df_withoutclass <- head (train_test_dataframe,200)
test_df_withoutclass <- tail (train_test_dataframe,100)
train_classes <- factor(head (classes,200))
test_classes <- factor(tail(classes,100))



NB <- naiveBayes(train_df_withoutclass,train_classes,laplace = 1)
PredictionNB <- predict(NB,test_df_withoutclass)
# table(PredictionNB,test_classes)
prop.table(table(test_classes==PredictionNB))

# SVM <- svm(train_df_withoutclass,train_classes,kernel="linear")
# SVM <- svm(train_df_withoutclass, train_classes, kernel="polynomial", degree=3)
SVM <- svm(train_df_withoutclass, train_classes, kernel='polynomial', degree=5)
# summary(SVM)
PredictionSVM <- predict(SVM,test_df_withoutclass)
# table(PredictionSVM,test_classes)
prop.table(table(test_classes==PredictionSVM))
# summary(SVM)

#try:
#kernel="polynomial"
#degree =3,5



