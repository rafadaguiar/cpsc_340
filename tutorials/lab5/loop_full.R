correct <- rep(0,50)
time <- rep(0,50)
nam <- c()
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
  prop.table(table(wine_test$V1 == round((compute(modelnn, temp_test))$net.result)))
  
  correct[i] <- a[2]
  time[i] <- t[3]
  nam <- c(nam, names(a)[2]) #check that R isn't flipping the true and false values in prop.table
}
hist(correct)
hist(time)
time_full <- time 
correct_full <- correct
nam # Indeed, all the recorded values are the "proption true"