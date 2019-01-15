#referenced from http://www.rdatamining.com/

## @knitr init
install.packages(twitteR)
library(twitteR)
# 
# n <- 200
# 
# keyword <- "»ï¼ºÀüÀÚ"
# 
# keyword <- enc2utf8(keyword)
# 
# rdmTweets <- searchTwitter(keyword,  n)

load(url("http://dl.dropbox.com/u/8686172/twitter.RData"))

nDocs <- length(rdmTweets)



## @knitr preprocess
library(KoNLP)
library(tm)


df <- do.call("rbind", lapply(rdmTweets, as.data.frame))

removeTwit <- function(x) {gsub("@[[:graph:]]*", "", x)}

df$ptext <- sapply(df$text, removeTwit)

removeURL <- function(x) { gsub("http://[[:graph:]]*", "", x)}

df$ptext <- sapply(df$ptext, removeURL)
useSejongDic()
df$ptext <- sapply(df$ptext, function(x) {paste(extractNoun(x), collapse=" ")}) 

#build corpus
myCorpus_ <- Corpus(VectorSource(df$ptext))
myCorpus_ <- tm_map(myCorpus_, removePunctuation)
myCorpus_ <- tm_map(myCorpus_, removeNumbers)
myCorpus_ <- tm_map(myCorpus_, tolower)
myStopwords <- c(stopwords('english'), "rt")
myCorpus_ <-tm_map(myCorpus_, removeWords, myStopwords)



## @knitr eda

myTdm <- TermDocumentMatrix(myCorpus, control=list(wordLengths=c(2,Inf)))

#inspect frequent term
findFreqTerms(myTdm, lowfreq=10)

#inspect associations 
findAssocs(myTdm,'lg',0.25)


## @knitr barplot
library(ggplot2)

termFrequency <- rowSums(as.matrix(myTdm))
termFrequency <- subset(termFrequency,termFrequency>=10)

ggplot(data.frame(term = names(termFrequency), freq=termFrequency), aes(term, freq)) + geom_bar() + coord_flip()


## @knitr wordcloud
#Word Cloud 

library(wordcloud)

m <- as.matrix(myTdm)

wordFreq <- sort(rowSums(m),decreasing=TRUE)

set.seed(375)

pal <- brewer.pal(8,"Dark2")

wordcloud(words=names(wordFreq),freq=wordFreq,min.freq=10,random.order=F, rot.per=.1,colors=pal)



## @knitr hclust
myTdm2<-removeSparseTerms(myTdm,sparse=0.95)
m2<-as.matrix(myTdm2)

distMatrix<-dist(scale(m2))

fit<-hclust(distMatrix,method="ward")

plot(fit)

rect.hclust(fit,k=10)

#(groups<-cutree(fit,k=10))



## @knitr kmeans
m3 <- t(m2)
k <- 4
kmres <- kmeans(m3, k)

round(kmres$centers, digits=3)


for(i in 1:k){
  cat(paste("cluster ", i, " : ", sep=""))
  s <- sort(kmres$centers[i, ], decreasing=T)
  cat(names(s)[1:3], "\n")
  #print(head(rdmTweets[which(kmres$cluster ==i)],n=3))
}


## @knitr kmedoid
library(fpc)
pamResult <- pamk(m3, metric="manhattan")
(k <- pamResult$nc)

pamResult <- pamResult$pamobject
#print cluster medoids

for(i in 1:k){
  cat(paste("cluster",i,":"))
  cat(colnames(pamResult$medoids)[which(pamResult$medoids[i,]==1)],"\n")
  # print tweets in cluster i
  #print(rdmTweets[pamResult$clustering==i])
}
