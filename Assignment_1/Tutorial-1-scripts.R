#dataset and other needed libraries. If the libraries aren't installed, then install them first

load('rdmTweets.RData')
rdmTweets
library(tm)
library(twitteR)

###PREPROCESSING

# convert tweets to a data frame
df <- do.call("rbind", lapply(rdmTweets, as.data.frame))

#creating a corpus based on the data frame
myCorpus <- Corpus(VectorSource(df$text))
#inspect the corpus elements
inspect(myCorpus[11:15])

# convert to lower case
myCorpus <- tm_map(myCorpus, tolower)
# remove punctuation
myCorpus <- tm_map(myCorpus, removePunctuation)
# remove numbers
myCorpus <- tm_map(myCorpus, removeNumbers)
# remove URLs
removeURL <- function(x) gsub("http[[:alnum:]]*", "", x)
myCorpus <- tm_map(myCorpus, removeURL)
# remove stopwords (stop words are the list of words that are freqeunt and have less value in terms on content such as: a, the, is, ...)
myStopwords <- c(stopwords('english'))
myCorpus <- tm_map(myCorpus, removeWords, myStopwords)
# view the changes
inspect(myCorpus[11:15])

# create term frequency vector for each document with word length of at least one character
myTdm <- TermDocumentMatrix(myCorpus, control=list(wordLengths=c(1,Inf)))
myTdm


#As we can see from the above result, the term-document matrix is composed of ? terms and 154 documents. It is very sparse, with 99% of the entries being zero. 
# lots of analysis can be done on terms with their frequencies and association. For example:

#We can have a look at the ﬁrst six terms starting with “r” and tweets numbered 101 to 110.
idx <- which(dimnames(myTdm)$Terms == "r")
inspect(myTdm[idx+(0:5),101:110])

# inspect frequent words with low frequency
findFreqTerms(myTdm, lowfreq=10)
# which words are associated with "mining" with correlation no less than 0.25?
findAssocs(myTdm, mining, 0.25)

# remove sparse terms
myTdm2 <- removeSparseTerms(myTdm, sparse=0.96)

####CLUSTERING:


#conversion to matrix:
m2 <- as.matrix(myTdm2)
#transpose the term-document matrix to a document-term one. 
m3 <- t(m2)
# set a fixed random seed to be able to reproduce the results
set.seed(122)
# k-means clustering 
k <- 2
kmeansResult <- kmeans(m3, k)
kmeansResult$cluster
table(kmeansResult$cluster)

#check the top 10 words in every cluster

for (i in 1:k) {
  cat(paste("cluster ", i, ": ", sep=""))
  s <- sort(kmeansResult$centers[i,], decreasing=T)
  cat(names(s)[1:10], "\n")
}

k <- 3
kmeansResult <- kmeans(m3, k)
kmeansResult$cluster
table(kmeansResult$cluster)

#check the top 10 words in every cluster

for (i in 1:k) {
  cat(paste("cluster ", i, ": ", sep=""))
  s <- sort(kmeansResult$centers[i,], decreasing=T)
  cat(names(s)[1:10], "\n")
}

# hirerachical clustering

distmatrix <- dist(scale(m3))
hclustResults <- hclust(distMatrix,method="ward")

plot(hclustResults,cex=0.5,hang=-1,main="Hclust dendogram of tweets")
rect.hclust(hclustResults,k=2)
cutree(hclustResults, k=2)
#close the plot 
#print the plot again for 3 clusters
plot(hclustResults,cex=0.5,hang=-1,main="Hclust dendogram of tweets")
rect.hclust(hclustResults,k=3)
cutree(hclustResults, k=3)



