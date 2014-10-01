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

pre_processing <- function (myCorpus, sparse) {
  myCorpus <- clean_corpus(myCorpus)
  # create term frequency vector for each document with word length of at least one character
  myTdm <- DocumentTermMatrix(myCorpus, control=list(weighting = weightTfIdf)) #  list(weighting = weightTfIdf)
  myTdm2 <- removeSparseTerms(myTdm, sparse=sparse)
  # Convert to matrix
  m <- as.matrix(myTdm2)
  
#   # Selecting relevant features:
#   features <- c(
#     "access", "article", "bit",  "called", "computer",
#     "data", "disk", "drive", "file", "information",
#     "internet", "machine", "medical", "memory", "monitor",
#     "program", "scimed", "science", "software", "system",
#     "technology", "version", "windows")
#   m <- m[,features]
}

top_10_words <- function(kmeansResult, k) {
  #check the top 10 words in every cluster
  for (i in 1:k) {
    cat(paste("cluster ", i, ": ", sep=""))
    s <- sort(kmeansResult$centers[i,], decreasing=T)
    cat(names(s)[1:10], "\n")
  }
}

k_means <- function(m, k) {
  # set a fixed random seed for k-means
  set.seed(122)
  # k-means clustering 
  kmeansResult <- kmeans(m, k)
  cat("K-means elements:", table(kmeansResult$cluster),"\n")
  kmeansResult
}

hierarchical_clustering <- function(m, k, method="euclidean") {
  distMatrix <- dist(scale(m), method=method)
  hclustResults <- hclust(distMatrix, method="ward.D2")
  plot(hclustResults, cex=0.5, hang=-1, main="News Dendogram")
  rect.hclust(hclustResults,k)
  h <- cutree(hclustResults, k)
  cat("H-clust elements:", table(h))
  h
}

K <- 6
sparse <- c(0.75, 0.9, 0.93, 0.945, 0.96, 0.99)
for (k in 3:3) {
  for (s in sparse[3]) {
    cat("K = ", k, " S = ", s, "\n")
    myCorpus <- VCorpus(DirSource("./data", encoding="UTF-8"))
    m <- pre_processing(myCorpus, s)
    kmeansResult <- k_means(m,k)
    top_10_words(kmeansResult,k)
    h <- hierarchical_clustering(m, k, "cosine")
  
  }
}


# Aside code for question 6 reasoning .: Hierarquical Clustering percentages
documents = as.numeric(names(h))
comp_hard <- documents  >= 50419 & documents <= 52404
comp_os <- documents  >= 9141 & documents <= 10942
sci_med <- documents  >= 58061 & documents <= 59652

for (i in 1:3) {
  cluster_percent <- c(
    length(h[h==i & comp_hard])*100/length(h[h==i]), 
    length(h[h==i & comp_os])*100/length(h[h==i]),
    length(h[h==i & sci_med])*100/length(h[h==i]))
  cat("Cluster (",i,") percentages ", cluster_percent, "\n")
}
