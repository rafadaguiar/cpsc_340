#install.packages("SnowballC")
#install.packages("rpart")

library(SnowballC)
library(tm)
library(rpart)
library(survival)
setwd("~/Workspace/cpsc_340/tutorials/lab2")

train<- read.table("./epinions/epinions-train.csv", sep = ",", header = TRUE)
test<- read.table("./epinions/epinions-test.csv", sep = ",", header = TRUE)

train_set <- train[sample(nrow(train), 100),]
test_set <- test[sample(nrow(test), 50),]

train_test_set <- rbind(train_set,test_set) 
# train_test_set$class
# train_test_set$text

train_test_corpus <- Corpus(VectorSource(train_test_set$text))

# Data pre-processing
train_test_corpus <- tm_map(train_test_corpus, content_transformer(tolower))
train_test_corpus <- tm_map(train_test_corpus, content_transformer(removePunctuation))
train_test_corpus <- tm_map(train_test_corpus, content_transformer(removeWords) , c(stopwords('english')))
# train_test_corpus[[1]]
train_test_corpus <- tm_map(train_test_corpus, content_transformer(stripWhitespace))
# train_test_corpus[[1]]

# Convert to DTM
DTM_train_test <- DocumentTermMatrix(train_test_corpus,control=list(stemming = TRUE))
DTM_train_test

# Convert to Data frame
train_test_dataframe <- as.data.frame(inspect( DTM_train_test ))
classification <- c(train_test_set$class)
train_test_df_withclass <- cbind( train_test_dataframe, classification)

train_df_withclass <- head (train_test_df_withclass,100)
test_df_withclass <- tail (train_test_df_withclass,50)

# dot "." means that we are training using all the features
# test_df_withclass is the new data that we want to predict the labels
dt_model <- rpart(classification ~ ., train_df_withclass, method="exp")
prediction <- predict(dt_model, test_df_withclass, type="vector")
# table (test_df_withclass$classification, prediction)

# Only using the camera and car features to create de model
dt_model <- rpart(classification ~ camera+car,train_df_withclass,method="class")
prediction <- predict(dt_model, test_df_withclass, type="class")
# table (test_df_withclass$classification, prediction)

# Only use feature "also"; using ANOVA(Analysis of Variance) method 
dt_model <- rpart(classification ~ also,train_df_withclass,method="anova")
prediction <- predict(dt_model, test_df_withclass, type="vector")
# table (test_df_withclass$classification, prediction)


dt_model <- rpart(classification ~ Surv(also),train_df_withclass,method="poisson")
prediction <- predict(dt_model, test_df_withclass, type="vector")
# table (test_df_withclass$classification, prediction)

# Inspecting the dataset
# colnames(test_df_withclass)