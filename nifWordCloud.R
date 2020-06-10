# ------------------------------------------------------------->
# Archive of Our Own: Nirvana in Fire (TV) Fanfic Text Word Cloud
# Scraped: 8 June 2020
# © Seow 2020

# clear all
rm(list = ls())

##### -------------------------------------------------------------> #####
# Libaries
library(tidyverse)
library(tokenizers)
library(DescTools)
library(wordcloud)
#library(wordcloud2)
library(stopwords)

path = 'C:/Users/dream/Downloads/AO3Scraper-master/fanfics.csv_text_files/'
setwd(path)
txtFiles<-list.files(path)

# Chinese fic
exclud1 <- c('9377837', '9348611', '9250082', '7298401', '11375991' ,'11375886' ,'11375856' ,'11375799','12710559','11510832','23839219')
# Unrelated drabble collection
exclud2 <- c('602170','13595709','18125552')
# mdzs fic with no nif connection
exclud3 <- c('20459813','16087568')
# naruto fic that is not a crossover
exclud4 <- c('11366736')

# podfics
excludpod<-c('24480028', '6474526' ,'24057001', '24055243', '24033550' ,'18287543' ,'10401897', '10369809' ,'10369521' , '7022773',
             '20762336' ,'17682113' ,'17192180' ,'14556363' ,'10553534' ,'10394805', '10365609' , '9287243', '10521804', '10193807')

# art
excludart<-c('24047197', '23237959', '23237806', '22988230', '22972963', '22699726', '22510771', '22214635', '22053370', '21684658', '21709666', '21460609',
             '21685291', '21786529', '21786463', '21296192', '20733743', '19725622', '19417252', '19417057', '19416988', '19394656', '19330720', '19330666', 
             '18962173', '18962152', '18885790', '18803383', '18803206', '18792922', '16711459', '17085371', '17234711', '17109101', '17234303', '17103905', 
             '17218685', '14881031', '13181817', '13152774', '13119618', '13051287', '13049769', '13035255', '12968175', '12958746', '12822456', '12749973', 
             '12461193')

# vid
excludvid<-c('23236627', '17010738', '21754528', '14136024', '16451231', '23809921', '16975728')

# Exclude all to leave only english fic
exclud<-c(exclud1,exclud2,exclud3,exclud4,excludpod,excludart,excludvid)
exclud.txt<-paste(exclud, '.txt', sep="")

# Read fic data
txtFiles.left<-txtFiles[!txtFiles %in% exclud.txt]
txtFiles.num<-length(txtFiles.left)

#txtFiles.num =10
words.pp<-vector()
for (i in 1:txtFiles.num){

  temp<-read.delim(txtFiles.left[i],header=FALSE, quote="")
  temp3<-paste(unlist(t(temp)), collapse=",")

  words.pp<-paste(words.pp, temp3, sep=" ")

}

# remove lower case
temp<-tolower(words.pp)
# remove 's
temp<-sapply(temp, function(x) gsub("'s", "", x))
# stopwords
allStop<- c( 'said', stopwords::stopwords(language = "en"))

# data cleaning
words <- tokenize_words(temp ,stopwords = allStop)

cloud<-as.data.frame(table(words))
colnames(cloud)<-c("word","freq")
cloud<-cloud[  with(cloud, order(-cloud$freq, cloud$word)), ]

# plot
set.seed(1234) # for reproducibility 
wordcloud(words = cloud$word, freq = cloud$freq, min.freq = 1, max.words=300, random.order=FALSE)
