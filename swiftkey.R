#This is the script for creating the swiftkey capstone project

#Set the seed
set.seed(1)

#Load libraries
library(tidytext)
library(tibble)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(stringr)

#Set the system locale to UTF-8
Sys.setlocale("LC_CTYPE", "en_US.UTF-8")

#Set working directory
setwd("~/Documents/Work/Study/Coursera/Data-Scientist/datasciencecoursera/Capstone Project - Swiftkey")

#Download the file
#url<-"https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
#file_name<-"Coursera-SwiftKey.zip"
#if(!file.exists(file_name)){
 #       download.file(url,file_name,method="curl")
#}

#Create folder names
#folder<-"final"

#Unzip the dataset
#if(!dir.exists(folder)){
#        unzip(file_name)
#}

#Create folder and file names
#lang<-c("fi_FI","en_US","ru_RU","de_DE")
#lang<-c("en_US")
#data_type<-c("twitter","blogs","news")

#Load the profanity file
prof_file<-"/Users/manavsehgal/Documents/Work/Study/DataScientist/Profanity/en"
prof_data<-read.table(prof_file,sep="\n",colClasses = "character")

#Create sample for each file
source("fileToSample.R")
en_US_twitter_Sample<-fileToSample("final/en_US/en_US.twitter.txt")
en_US_blogs_Sample<-fileToSample("final/en_US/en_US.blogs.txt")
en_US_news_Sample<-fileToSample("final/en_US/en_US.news.txt")


#Combine the files and convert into a tibble
en_US_Sample<-c(en_US_twitter_Sample,en_US_blogs_Sample,en_US_news_Sample)
en_US_Sample<-tibble(en_US_Sample)


# Create Ngram Tokens
en_US_Sample_Unigram<-unnest_tokens(en_US_Sample,word,en_US_Sample,token='ngrams',n=1)
en_US_Sample_Bigram<-unnest_tokens(en_US_Sample,word,en_US_Sample,token='ngrams',n=2)
en_US_Sample_Trigram<-unnest_tokens(en_US_Sample,word,en_US_Sample,token='ngrams',n=3)
en_US_Sample_Fourgram<-unnest_tokens(en_US_Sample,word,en_US_Sample,token='ngrams',n=4)
en_US_Sample_Fivegram<-unnest_tokens(en_US_Sample,word,en_US_Sample,token='ngrams',n=5)

#Create word counts
en_US_Sample_Unigram_count<-en_US_Sample_Unigram%>%group_by(word)%>%count(sort=T)
en_US_Sample_Bigram_count<-en_US_Sample_Bigram%>%group_by(word)%>%count(sort=T)
en_US_Sample_Trigram_count<-en_US_Sample_Trigram%>%group_by(word)%>%count(sort=T)
en_US_Sample_Fourgram_count<-en_US_Sample_Fourgram%>%group_by(word)%>%count(sort=T)
en_US_Sample_Fivegram_count<-en_US_Sample_Fivegram%>%group_by(word)%>%count(sort=T)

# #Exploratory Analysis
# #Unigrams
# ggplot(en_US_Sample_Unigram_count[1:20,],mapping=aes(x=en_US_Sample_Unigram_count$word[1:20],y=en_US_Sample_Unigram_count$n[1:20]))+geom_col()+theme(legend.position = "none",axis.text.x = element_text(angle = 45, hjust = 1))+labs(x="words",y="frequency",title="Histogram of top 20 Unigrams")
# pal <- brewer.pal(12,"Dark2")
# layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
# par(mar=rep(0, 4))
# plot.new()
# text(x=0.5, y=0.5, "Wordcloud of top 100 unigrams")
# wordcloud(en_US_Sample_Unigram_count$word,en_US_Sample_Unigram_count$n,max.words = 100,random.color = TRUE,colors = pal)
# 
# #Bigrams
# ggplot(en_US_Sample_Bigram_count[1:20,],mapping=aes(x=en_US_Sample_Bigram_count$word[1:20],y=en_US_Sample_Bigram_count$n[1:20]))+geom_col()+theme(legend.position = "none",axis.text.x = element_text(angle = 45, hjust = 1))+labs(x="words",y="frequency",title="Histogram of top 20 Bigrams")
# pal <- brewer.pal(12,"Dark2")
# layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
# par(mar=rep(0, 4))
# plot.new()
# text(x=0.5, y=0.5, "Wordcloud of top 50 bigrams")
# wordcloud(en_US_Sample_Bigram_count$word,en_US_Sample_Bigram_count$n,max.words = 50,random.color = TRUE,colors = pal)
# 
# #Trigrams
# ggplot(en_US_Sample_Trigram_count[1:20,],mapping=aes(x=en_US_Sample_Trigram_count$word[1:20],y=en_US_Sample_Trigram_count$n[1:20]))+geom_col()+theme(legend.position = "none",axis.text.x = element_text(angle = 45, hjust = 1))+labs(x="words",y="frequency",title="Histogram of top 20 Trigrams")
# pal <- brewer.pal(12,"Dark2")
# layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
# par(mar=rep(0, 4))
# plot.new()
# text(x=0.5, y=0.5, "Wordcloud of top 30 trigrams")
# wordcloud(en_US_Sample_Trigram_count$word,en_US_Sample_Trigram_count$n,max.words = 30,random.color = TRUE,colors = pal)
#
# ggplot(en_US_Sample_Fourgram_count[1:20,],mapping=aes(x=en_US_Sample_Fourgram_count$word[1:20],y=en_US_Sample_Fourgram_count$n[1:20]))+geom_col()+theme(legend.position = "none",axis.text.x = element_text(angle = 45, hjust = 1))+labs(x="words",y="frequency",title="Histogram of top 20 Fourgrams")
# pal <- brewer.pal(12,"Dark2")
# layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
# par(mar=rep(0, 4))
# plot.new()
# text(x=0.5, y=0.5, "Wordcloud of top 25 fourgrams")
# wordcloud(en_US_Sample_Fourgram_count$word,en_US_Sample_Fourgram_count$n,max.words = 25,random.color = TRUE,colors = pal)
#
# #Fivegrams
# ggplot(en_US_Sample_Fivegram_count[1:20,],mapping=aes(x=en_US_Sample_Fivegram_count$word[1:20],y=en_US_Sample_Fivegram_count$n[1:20]))+geom_col()+theme(legend.position = "none",axis.text.x = element_text(angle = 45, hjust = 1))+labs(x="words",y="frequency",title="Histogram of top 20 Fivegrams")
# pal <- brewer.pal(12,"Dark2")
# layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
# par(mar=rep(0, 4))
# plot.new()
# text(x=0.5, y=0.5, "Wordcloud of top 25 Fivegrams")
# wordcloud(en_US_Sample_Fivegram_count$word,en_US_Sample_Fivegram_count$n,max.words = 25,random.color = TRUE,colors = pal)

#Function to split the ngrams into firstwords and last words
splitngrams<-function(ngram){
        #load the library
        library(stringr)
        ngram$firstwords<-word(ngram$word,1,-2)
        ngram$lastwords<-word(ngram$word,-1)
        return(ngram)
}

en_US_Sample_Unigram_count<-splitngrams(en_US_Sample_Unigram_count)
en_US_Sample_Bigram_count<-splitngrams(en_US_Sample_Bigram_count)
en_US_Sample_Trigram_count<-splitngrams(en_US_Sample_Trigram_count)
en_US_Sample_Fourgram_count<-splitngrams(en_US_Sample_Fourgram_count)
en_US_Sample_Fivegram_count<-splitngrams(en_US_Sample_Fivegram_count)

#WFunction to calculate the discount factor
#d=((r+1)*N(r+1))/(r*N(r))
#r= frequency
#N = number of words/phrases with same frequency

discount_factor<-function(ngram){
        ngram$discount=rep(1,nrow(ngram))
        for(i in 5:1){#Frequencies greater than 5 are reliable so their discounting factor is 1
                discount<-(i+1)/i*(nrow(ngram[ngram$n==(i+1),])/nrow(ngram[ngram$n==(i),]))
                ngram[ngram$n==i,]$discount=discount
        }
        return(ngram)
}

en_US_Sample_Trigram_count<-discount_factor(en_US_Sample_Trigram_count)
en_US_Sample_Bigram_count<-discount_factor(en_US_Sample_Bigram_count)
en_US_Sample_Unigram_count<-discount_factor(en_US_Sample_Unigram_count)
en_US_Sample_Fourgram_count<-discount_factor(en_US_Sample_Fourgram_count)
en_US_Sample_Fivegram_count<-discount_factor(en_US_Sample_Fivegram_count)

en_US_Sample_Trigram_count$disc_n<-en_US_Sample_Trigram_count$n*en_US_Sample_Trigram_count$discount
en_US_Sample_Bigram_count$disc_n<-en_US_Sample_Bigram_count$n*en_US_Sample_Bigram_count$discount
en_US_Sample_Unigram_count$disc_n<-en_US_Sample_Unigram_count$n*en_US_Sample_Unigram_count$discount
en_US_Sample_Fourgram_count$disc_n<-en_US_Sample_Fourgram_count$n*en_US_Sample_Fourgram_count$discount
en_US_Sample_Fivegram_count$disc_n<-en_US_Sample_Fivegram_count$n*en_US_Sample_Fivegram_count$discount

en_US_Sample_Fivegram_count<-en_US_Sample_Fivegram_count[order(-en_US_Sample_Fivegram_count$disc_n),c("firstwords","lastwords","n","disc_n")]
en_US_Sample_Fourgram_count<-en_US_Sample_Fourgram_count[order(-en_US_Sample_Fourgram_count$disc_n),c("firstwords","lastwords","n","disc_n")]
en_US_Sample_Trigram_count<-en_US_Sample_Trigram_count[order(-en_US_Sample_Trigram_count$disc_n),c("firstwords","lastwords","n","disc_n")]
en_US_Sample_Bigram_count<-en_US_Sample_Bigram_count[order(-en_US_Sample_Bigram_count$disc_n),c("firstwords","lastwords","n","disc_n")]
en_US_Sample_Unigram_count<-en_US_Sample_Unigram_count[order(-en_US_Sample_Unigram_count$disc_n),c("firstwords","lastwords","n","disc_n")]

save(prof_data,en_US_Sample_Bigram_count,en_US_Sample_Trigram_count,en_US_Sample_Unigram_count,file = "swiftkeydata.RData")

# en_US_Sample_Fivegram_count$ngram<-5
# en_US_Sample_Fourgram_count$ngram<-4
# en_US_Sample_Trigram_count$ngram<-3
# en_US_Sample_Bigram_count$ngram<-2
# en_US_Sample_Unigram_count$ngram<-1

#en_US_Sample_count<-rbind(en_US_Sample_Fivegram_count,en_US_Sample_Fourgram_count,en_US_Sample_Trigram_count,en_US_Sample_Bigram_count,en_US_Sample_Unigram_count)
#write.csv(en_US_Sample_count[,c("firstwords","lastwords","n","disc_n","ngram")])

#Remove unrequired data sets
rm(prof_file)
rm(en_US_twitter_Sample)
rm(en_US_blogs_Sample)
rm(en_US_news_Sample)
rm(en_US_Sample_Unigram)
rm(en_US_Sample_Bigram)
rm(en_US_Sample_Trigram)
rm(en_US_Sample_Fourgram)
rm(en_US_Sample_Fivegram)


#source("SwiftKeyPrediction.R")