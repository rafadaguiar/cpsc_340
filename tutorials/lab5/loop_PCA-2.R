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