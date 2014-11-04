#install.packages("neuralnet")
setwd("~/Workspace/cpsc_340/tutorials/lab5")
set.seed(1)
library(neuralnet)
wine <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data",sep=",")
#wine <- read.table("wine.data",head=TRUE,sep=",")
head(wine)
#randomize order of rows
wine = wine[sample(nrow(wine)),] 
wine_train = wine[1:100,]
wine_test = wine[101:178,]

# train a neuralnet model based on all features
system.time(modelnn <- neuralnet(V1 ~ V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 , wine_train, hidden = 10, algorithm='rprop+',learningrate=0.01,rep=1))

# get the test set in the format that neuralnet compute accepts
temp_test <- subset(wine_test,select = c("V2","V3","V4","V5","V6","V7","V8","V9","V10","V11","V12","V13"))

# calculate accuracy (true) --> compute is a fucntion that predict in neuralnet library
prop.table(table(wine_test$V1 == round((compute(modelnn, temp_test))$net.result)))

# PCA 

#library("prcomp")

wine_train_scaled <- as.data.frame(scale(wine_train[2:14]))
wine_test_scaled <- as.data.frame(scale(wine_test[2:14]))
wine_train_PCA <- prcomp(wine_train_scaled)
wine_test_PCA <- prcomp(wine_test_scaled)


# same rotation
# wine_train_PCA <- prcomp(wine_traine[2:14], scale. = True)
# wine_test_PCA <- predict(wine_train_PCA, wine_test_PCA[2:14])

# the feature values after PCA run
head(wine_train_PCA$x)

#standard deviations
sapply(wine_train[2:14],sd)
wine_train_PCA$sdev

# Plot in order to decide how many Principal Components to retain
screeplot(wine_train_PCA, type="lines")
#The most obvious change in slope in the scree plot occurs at component 4, which is the “elbow” of the scree plot. Therefore, it cound be argued based on the basis of the scree plot that the first three components should be retained.

#prepare data for training and test
wine_train.pca <- data.frame(V1 = wine_train[, "V1"], wine_train_PCA$x)
wine_test.pca <- subset(wine_test_PCA$x,select = c("PC1","PC2","PC3","PC4"))
# train model and record the time
system.time(modelnn.pca <- neuralnet(V1 ~ PC1 + PC2 + PC3 + PC4 , wine_train.pca, hidden = 10, algorithm='rprop+',learningrate=0.01,rep=1))

prop.table(table(wine_test$V1 == round((compute(modelnn.pca, wine_test.pca))$net.result)))

correct <- rep(0,50)
time <- rep(0,50)
nam <- c()
for (i in 1:50){
  set.seed(i)
  wine = wine[sample(nrow(wine)),] 
  wine_train = wine[1:100,]
  wine_test = wine[101:178,]
  wine_train_scaled <- as.data.frame(scale(wine_train[2:14]))
  wine_test_scaled <- as.data.frame(scale(wine_test[2:14]))
  wine_train_PCA <- prcomp(wine_train_scaled)
  wine_test_PCA <- prcomp(wine_test_scaled)
  wine_train.pca <- data.frame(V1 = wine_train[, "V1"], wine_train_PCA$x)
  wine_test.pca <- subset(wine_test_PCA$x,select = c("PC1","PC2","PC3","PC4"))
  
  t <- system.time(modelnn.pca <- neuralnet(V1 ~ PC1 + PC2 + PC3 + PC4 , 
                                            wine_train.pca, hidden = 10, 
                                            algorithm='rprop+',
                                            learningrate=0.01,
                                            rep=1))
  a <- prop.table(table(wine_test$V1 == round((compute(modelnn.pca, wine_test.pca))$net.result)))
  correct[i] <- a[2]
  time[i] <- t[3]
  nam <- c(nam, names(a)[2]) #check that R isn't flipping the true and false values in prop.table
}
hist(correct)
hist(time)
nam # Indeed, all the recorded values are the "proption true"

for (i in 1:50){
  set.seed(i)
  wine = wine[sample(nrow(wine)),] 
  wine_train = wine[1:100,]
  wine_test = wine[101:178,]
  
  # train a neuralnet model based on all features
  t <- system.time(modelnn <- neuralnet(V1 ~ V2 + V3 + V4 + V5 + V6 + V7 + V8 + V9 + V10 + V11 + V12 + V13 , wine_train, hidden = 10, algorithm='rprop+',learningrate=0.01,rep=1))
  
  
  # get the test set in the format that neuralnet compute accepts
  temp_test <- subset(wine_test,select = c("V2","V3","V4","V5","V6","V7","V8","V9","V10","V11","V12","V13"))
  
  # calculate accuracy (true) --> compute is a fucntion that predict in neuralnet library
  a <- prop.table(table(wine_test$V1 == round((compute(modelnn, temp_test))$net.result)))
  
  correct[i] <- a[2]
  time[i] <- t[3]
  nam <- c(nam, names(a)[2]) #check that R isn't flipping the true and false values in prop.table
}
hist(correct)
hist(time)
time_full <- time 
correct_full <- correct
nam # Indeed, all the recorded values are the "proption true"