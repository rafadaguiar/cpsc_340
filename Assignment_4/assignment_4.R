# install.packages("cvTools")
library(cvTools)
library(e1071)
library(tm)
set.seed(340)

setwd("~/Workspace/cpsc_340/Assignment_4")

clean_corpus <- function (myCorpus) {
  # remove author
  myCorpus <- tm_map(myCorpus, content_transformer(function(x) gsub("From:[^\n]+\n*","", x)))
  # convert to lower case
  myCorpus <- tm_map(myCorpus, content_transformer(tolower))
  # remove punctuation
  myCorpus <- tm_map(myCorpus, content_transformer(removePunctuation))
  # remove numbers
  myCorpus <- tm_map(myCorpus, content_transformer(removeNumbers))
  # remove XML
  myCorpus <- tm_map(myCorpus, content_transformer(PlainTextDocument))
  # remove stopwords (stop words are the list of words that are frequent and have less value in terms on content such as: a, the, is, ...)
  myStopwords <- c(stopwords('english'))
  myCorpus <- tm_map(myCorpus, content_transformer(removeWords), myStopwords)
  # remove extra spaces
  myCorpus <- tm_map(myCorpus, content_transformer(function(x) gsub("\\s+"," ",x)))
  myCorpus <- tm_map(myCorpus, content_transformer(function(x) gsub("^ | $","",x)))
}

pre_processing <- function (myCorpus, sparsity) {
  myCorpus <- clean_corpus(myCorpus)
  # create term frequency vector for each document with word length of at least one character
  myDTM <- DocumentTermMatrix(myCorpus, control=list(stemming = TRUE, weighting = weightTfIdf))
  myDTM2 <- removeSparseTerms(myDTM, sparse=sparsity)
  # Convert to matrix
  myDF <- as.data.frame(as.matrix(myDTM2))
}

# Training Data
path <- "data/20news-bydate-train"
comp_data_files <- list.files(path, pattern = "^comp.")
other_data_files <- setdiff(list.files(path),comp_data_files)

comp_train_Corpus <- VCorpus(DirSource(paste(path, comp_data_files, sep="/"), encoding="UTF-8"))
other_train_Corpus <- VCorpus(DirSource(paste(path, other_data_files, sep="/"), encoding="UTF-8"))

# Test Data
path <- "data/20news-bydate-test"

comp_test_Corpus <- VCorpus(DirSource(paste(path, comp_data_files, sep="/"), encoding="UTF-8"))
other_test_Corpus <- VCorpus(DirSource(paste(path, other_data_files, sep="/"), encoding="UTF-8"))

###
trainCorpus <- c(comp_train_Corpus, other_train_Corpus)
testCorpus <- c(comp_test_Corpus, other_test_Corpus)

train_test_set <- c(trainCorpus,testCorpus)
train_test_df <- pre_processing(train_test_set, sparsity=0.99)

train_classes <- factor(c(rep(1, length(comp_train_Corpus)), rep(0, length(other_train_Corpus))))
test_classes  <- factor(c(rep(1, length(comp_test_Corpus)), rep(0, length(other_test_Corpus))))

train_df_withoutclass <- head (train_test_df, length(trainCorpus))
test_df_withoutclass <- tail (train_test_df, length(testCorpus))

# SVM with different Kernels
SVM <- svm(train_df_withoutclass, train_classes, kernel='radial')
PredictionSVM <- predict(SVM,test_df_withoutclass)
cm <- table(PredictionSVM,test_classes)
cat("Sensitivity = ", cm[2,2]/(cm[2,2]+cm[1,2])," Specificity = ", cm[1,1]/(cm[1,1]+cm[2,1])," Accuracy = ", (cm[1,1]+cm[2,2])/(cm[1,1]+cm[1,2]+cm[2,1]+cm[2,2]))

# Best results in terms of accuracy
  # (Sparsity = 0.99, 1687 features)
  #   Linear Kernel: Sensitivity =  0.7769821  Specificity =  0.9044289  Accuracy =  0.8713489
  # (Sparsity = 0.8, 25 features)
  #   Radial Kernel, coef0 = 0: Sensitivity =  0.2056266  Specificity =  0.9483593  Accuracy =  0.7555762
  #   Linear Kernel: Sensitivity =  0.04143223  Specificity =  0.9926484  Accuracy =  0.7457515
  #   Sigmoid Kernel, coef0 = 4: Sensitivity =  0.004092072  Specificity =  0.9987448  Accuracy =  0.7405736  
  #   Polinomial Kernel, degree = 3: Sensitivity =  0.1570332  Specificity =  0.9426215  Accuracy =  0.7387148 
#

# >>>> 2nd Part <<<<

# I'm getting the same results as before when using the cross parameter
SVM <- svm(train_df_withoutclass, train_classes, kernel='radial', cross=5)
PredictionSVM <- predict(SVM,test_df_withoutclass)
cm <- table(PredictionSVM,test_classes)
cat("Sensitivity = ", cm[2,2]/(cm[2,2]+cm[1,2])," Specificity = ", cm[1,1]/(cm[1,1]+cm[2,1])," Accuracy = ", (cm[1,1]+cm[2,2])/(cm[1,1]+cm[1,2]+cm[2,1]+cm[2,2]))

# Let's try using cvTools
folds <- cvFolds(nrow(train_test_df), K=5)
train_fold <- folds$subset[1:floor(4*folds$n/5)]
test_fold <- na.omit(folds$subset[floor(4*folds$n/5)+1:folds$n])
classes <- factor(c(
  rep(1, length(comp_train_Corpus)), rep(0, length(other_train_Corpus)),
  rep(1, length(comp_test_Corpus)),  rep(0, length(other_test_Corpus))
))

SVM <- svm(train_test_df[train_fold,], classes[train_fold], kernel='polynomial',degree=3)
PredictionSVM <- predict(SVM, train_test_df[test_fold,])
cm <- table(PredictionSVM, classes[test_fold])
cat("Sensitivity = ", cm[2,2]/(cm[2,2]+cm[1,2])," Specificity = ", cm[1,1]/(cm[1,1]+cm[2,1])," Accuracy = ", (cm[1,1]+cm[2,2])/(cm[1,1]+cm[1,2]+cm[2,1]+cm[2,2]))

# Best results in terms of accuracy (using 5-fold)
  # Radial Kernel, coef0 = 0: Sensitivity =  0.2137643  Specificity =  0.9590893  Accuracy =  0.769496
  # Linear Kernel: Sensitivity =  0  Specificity =  1  Accuracy =  0.7456233
  # Polinomial Kernel, degree = 3: Sensitivity =  0.1532847  Specificity =  0.9484169  Accuracy =  0.7461538
  # Sigmoid Kernel, coef0 = 4: Sensitivity =  0.003128259  Specificity =  0.998577  Accuracy =  0.7453581  
  
#