
#Get the data

#Set the working directory in the folder contain the file "music_survey.csv".

#Read data in and name it as data_music.

setwd("D:/R/TA/Day1")

data_music<-read.csv("music_survey.csv", stringsAsFactor=FALSE)


#View your data

View(data_music)


#Creat Text Corpus
#Install and load library

#install.packages("NLP")
#install.packages("tm")
library(NLP)
library(tm)

#Read Question 1 into new variable called comment1
#Print first 6 comments from comment1

comment1<-data_music$Q1..What.do.you.like.most.about.this.portable.music.player.
head(comment1)


#Creat a Corpus, which can be processed by tm package. Print comments.

vector1<-VectorSource(comment1)
corpus1<-VCorpus(vector1)

for( i in 1:6){
  print(corpus1[[i]][1])
}    

#Normalize text

#Remove punctuation
corpus1<-tm_map(corpus1,removePunctuation)

for( i in 1:6){
  print(corpus1[[i]][1])
}

#Change the letters to lower cases
corpus1<-tm_map(corpus1, content_transformer(tolower))

for( i in 1:6){
  print(corpus1[[i]][1])
}    


#Remove numbers and stopwords
corpus1 <- tm_map(corpus1, removeNumbers)

myStopwords <- c(stopwords('english'), 'can','lot')
corpus1 <- tm_map(corpus1, removeWords, myStopwords)

for( i in 1:6){
  print(corpus1[[i]][1])
}    


#Install and load library for stemming

#install.packages("SnowballC")
library(SnowballC) 

#Word steming
corpus1<-tm_map(corpus1, stemDocument)

for( i in 1:6){
  print(corpus1[[i]][1])
}    


#Remove White space
corpus1<-tm_map(corpus1, stripWhitespace)

for( i in 1:6){
  print(corpus1[[i]][1])
}    


#Generate dtm 

# 1.Generate dtm (frequency count)

doc<-corpus1

dtm <- DocumentTermMatrix(doc) 
dtm


#2.Generate dtm (TfIdf)

dtm2 <- DocumentTermMatrix(doc,
           control = list(weighting = function(x) weightTfIdf(x,normalize =FALSE)))
dtm2

#3.Generate dtm (binary)

dtm3 <- DocumentTermMatrix(doc,control = list(weighting=weightBin))
dtm3

#Save dtm into CSV file

m <- as.matrix(dtm)   
dim(m)   
write.csv(m, file="Zhenzhen_dtm.csv")

#Frequency

#Count the global frequency of each concept
#Which DTM should we use?
#What if we want to get the document frequency of each concepts?

freq <- colSums(as.matrix(dtm))  #Sum each column

#sort frequency in order
sor<-sort(freq,decreasing = TRUE)
head(sor)

#Geneate dataframe of concepts and their frequency (Only frequency greater than 15)

subfreq<-subset(freq, freq>15)
dffreq<-data.frame(term=names(subfreq), fre=subfreq)

#Plot the frequency

#install.packages('ggplot2')
library(ggplot2)
ggplot(dffreq, aes(x=dffreq$term, y=dffreq$fre))+ geom_bar(stat="identity")+ coord_flip()

#word cloud

#Generate word cloud

#install.packages("RColorBrewer")
#install.packages("wordcloud")

library(RColorBrewer)  
library(wordcloud) 

wordcloud(names(freq), freq, min.freq=10)

#Add color to wordcloud

dark2 <- brewer.pal(6, "Dark2")   
wordcloud(names(freq), freq, max.words=50, rot.per=0.4, colors=dark2)

myStopwords <- c(stopwords('english'), 'can','lot', 'also', 'music')

corpus1<-VCorpus(vector1)
corpus1<-tm_map(corpus1, content_transformer(tolower))
corpus1 <- tm_map(corpus1, removeNumbers)
corpus1 <- tm_map(corpus1, removeWords, myStopwords)
corpus1<-tm_map(corpus1,removePunctuation)
corpus1 <- tm_map(corpus1, removeWords, myStopwords)
corpus1<-tm_map(corpus1, stemDocument)
doc<-corpus1
dtm <- DocumentTermMatrix(doc)
freq <- colSums(as.matrix(dtm))  #Sum each column
wordcloud(names(freq), freq, min.freq=10)
