#install.packages("tm","proxy")
library(tm)
library(proxy)
setwd("~/Workspace/cpsc_340/Assignment_1")

clean_corpus <- function (myCorpus) {
  # convert to lower case
  myCorpus <- tm_map(myCorpus, content_transformer(tolower))
  # remove punctuation
  myCorpus <- tm_map(myCorpus, content_transformer(removePunctuation))
  # remove numbers
  myCorpus <- tm_map(myCorpus, content_transformer(removeNumbers))
  # remove URLs
  removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
  myCorpus <- tm_map(myCorpus, content_transformer(removeURL))
  # remove stopwords (stop words are the list of words that are frequent and have less value in terms on content such as: a, the, is, ...)
  myStopwords <- c(stopwords('english'))
  myCorpus <- tm_map(myCorpus, content_transformer(removeWords), myStopwords)
}

pre_processing <- function (myCorpus) {
  myCorpus <- clean_corpus(myCorpus)
  # create term frequency vector for each document with word length of at least one character
  myTdm <- DocumentTermMatrix(myCorpus, control = list(weighting = weightTfIdf)) # control=list(wordLengths=c(1,Inf))
  myTdm2 <- removeSparseTerms(myTdm, sparse=0.96)
  #convert to matrix
  m <- as.matrix(myTdm2)
}

top_10_words <- function(kmeansResult, k) {
  #check the top 10 words in every cluster
  for (i in 1:k) {
    cat(paste("cluster ", i, ": ", sep=""))
    s <- sort(kmeansResult$centers[i,], decreasing=T)
    cat(names(s)[1:10], "\n")
  }
}ch

k_means <- function(m, k) {
  # set a fixed random seed for k-means
  set.seed(122)
  # k-means clustering 
  kmeansResult <- kmeans(m, k)
  cat("Elements:", table(kmeansResult$cluster),"\n")
  kmeansResult
}

hierarchical_clustering <- function(m, k, method="euclidean") {
  distMatrix <- dist(scale(m), method=method)
  hclustResults <- hclust(distMatrix, method="ward.D2")
  
  plot(hclustResults, cex=0.5, hang=-1, main="News Dendogram")
  rect.hclust(hclustResults,k)
  cutree(hclustResults, k)
}

k <- 3

myCorpus <- VCorpus(DirSource("./data", encoding="UTF-8"))
m <- pre_processing(myCorpus)
kmeansResult <- k_means(m,k)
top_10_words(kmeansResult,k)
hierarchical_clustering(m, k, "cosine")


