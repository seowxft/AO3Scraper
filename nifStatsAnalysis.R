# --------------------------------------------------------------------------->
# Archive of Our Own: Nirvana in Fire (TV) Fanwork Meta-data Stats Analysis
# Scraped: 8 June 2020
# © Seow 2020


# clear all
rm(list = ls())

#####
# libaries
##### -------------------------------------------------------------> #####
library(tidyverse)
library(DescTools)
library(ggplot2)

#####

# load data
##### -------------------------------------------------------------> #####
path = 'C:/Users/dream/Downloads/AO3Scraper-master/'
setwd(path)
metaData.e<-read.csv('nif_metaData.csv')


#####

# manual exclusions
##### -------------------------------------------------------------> #####
# Chinese fanwork mislabelled as English
exclud1 <- c('9377837', '9348611', '9250082', '7298401', '11375991' ,'11375886' ,'11375856' ,'11375799','12710559','11510832','23839219')
# Unrelated drabble collection
exclud2 <- c('602170','13595709','18125552')
# MDZS fanworks with no NIF connection
exclud3 <- c('20459813','16087568')
# Naruto fanwork that is not a crossover
exclud4 <- c('11366736')

exclud<-c(exclud1,exclud2,exclud3,exclud4)
metaData<-metaData.e[!metaData.e$work_id %in% exclud,]

#####

# Fic publish by year
##### -------------------------------------------------------------> #####
metaData$timeValue <- as.numeric( gsub("-", "", as.character(metaData$published)) )
metaData$year<-as.numeric(substr(metaData$timeValue, 1, 4))

year.freq<-as.data.frame(table(metaData$year))
colnames(year.freq)<-c("Year", "Freq")
year.freq$Day<-factor(year.freq$Year, ordered = TRUE, levels=year.freq$Year)

year.freq.p<- year.freq  %>%
  ggplot( aes(x=Year, y=Freq, label = Freq)) +
  geom_bar(stat="identity", fill="#008080", alpha=.6, width=.4) +
  geom_text(color="black", size=5, position=position_dodge(width=0.9), vjust=-0.25)+ #coord_flip() +
  labs(subtitle="Fanworks Published Date by Year", x= 'Year', y= 'No. of Fanworks') +
  theme_classic() +   theme(text = element_text(size=15)) +
  theme(panel.border = element_blank(), panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line( size=.1, color="grey" ) ,
        axis.line = element_line(colour = "black",size=1),axis.ticks = element_line(colour = "black",size=1), 
        axis.title = element_text(size=15),
        axis.text.x  = element_text(colour = "black",size=15),
        axis.text.y  = element_text(colour = "black",size=15))
year.freq.p


#####


# Fic publish by month, cumulative
##### -------------------------------------------------------------> #####
metaData$month<-as.numeric(substr(metaData$timeValue, 5, 6))
monthSpell<-c("Jan","Feb","Mar","April","May","June","July","Aug","Sep","Oct","Nov","Dec")
metaData$monthSpell<-monthSpell[metaData$month]

metaData <- within(metaData,  dateText <- paste(monthSpell, year, sep="-"))
metaData<-metaData[with(metaData, order(metaData$year,metaData$month)),  ]

date.freq<-as.data.frame(table(metaData$dateText))
colnames(date.freq)<-c("Date", "Freq")

#ordered = TRUE, 
date.freq$Date<-factor(date.freq$Date, levels=unique(metaData$dateText))
date.freq<-with(date.freq, date.freq[order(Date),])

date.freq.p<-ggplot(date.freq, aes(x=Date, y=cumsum(Freq))) + geom_point()+
  labs(subtitle="Cumulative No. of Fanworks Over Time", x= 'Date', y= 'No. of Fanworks') +
  theme_classic() +   theme(text = element_text(size=15)) +
  theme(panel.border = element_blank(), panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line( size=.1, color="grey" ) ,
        axis.line = element_line(colour = "black",size=1),axis.ticks = element_line(colour = "black",size=1), 
        axis.title = element_text(size=15),
        axis.text.x  = element_text(colour = "black",size=10,angle = 60,vjust = 1, hjust = 1),
        axis.text.y  = element_text(colour = "black",size=15))
date.freq.p

with(date.freq, date.freq[order(date.freq$Freq),])

metaData[metaData$timeValue==min(metaData$timeValue),]

#####
# Fic status proportion
##### -------------------------------------------------------------> #####
status.freq<-as.data.frame(table(metaData$status))
colnames(status.freq)<-c("Status","Freq")
status.freq<-status.freq[  with(status.freq, order(-status.freq$Freq, status.freq$Status)), ]

status.freq.p<-ggplot(status.freq, aes(x="", y=Freq, fill=Status ))+
  geom_bar(width = 1, stat = "identity")+
  coord_polar("y", start=0)+ scale_fill_brewer(palette="Set1")+
  theme_minimal()+
  theme(    axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.border = element_blank(),
            panel.grid=element_blank(),
            axis.ticks = element_blank(),
            plot.title=element_text(size=14, face="bold")  )+
  theme(axis.text.x=element_blank())+
  geom_text(aes(label = paste(round(Freq / sum(Freq) * 100, 1), "%")),
            position = position_stack(vjust = 0.5), size=5)+
  labs(subtitle="Status Distribution") +  theme(text = element_text(size=15))
status.freq.p


#####
# Words Histogram
##### -------------------------------------------------------------> #####
#Podfic
podfic.1<-metaData[grep("PODFIC", metaData$title), ]
podfic.2<-metaData[grep("podfic", metaData$title), ]
podfic.3<-metaData[grep("Podfic", metaData$title), ]
words.podfic<-rbind(podfic.1,podfic.2,podfic.3)

metaData.exclud<- metaData[!metaData$work_id %in%  words.podfic$work_id , ]

words.100temp<-metaData.exclud[metaData.exclud$word<=100,]
words.1000temp<-metaData.exclud[metaData.exclud$words > 100 & metaData.exclud$words <=1000,]

#videos
words.video1<-dplyr::filter(words.100temp, grepl("Video",additional.tags))
words.video2<-dplyr::filter(words.100temp, grepl("vid",additional.tags))
words.video3<-words.100temp[words.100temp$work_id==16451231,]
words.video4<-dplyr::filter(words.1000temp, grepl("Fanvid",additional.tags))
words.vid<-rbind(words.video1,words.video2,words.video3,words.video4)
words.vid<- words.vid[!duplicated(words.vid),] #make sure no duplicates

# drabble
words.drabble1<-dplyr::filter(words.100temp, grepl("Drabble",additional.tags))
words.drabble2<-words.100temp[words.100temp$work_id==11633469,]
words.100<-rbind(words.drabble1,words.drabble2)
words.100<- words.100[!duplicated(words.100),] #make sure no duplicates

#art/craft
words.art<-words.100temp[!words.100temp$work_id %in%  words.vid$work_id & !words.100temp$work_id %in%  words.100$work_id  , ]

words.1000<-dplyr::filter(words.1000temp, !grepl("Fanvid",additional.tags))

words.2500<-metaData.exclud[metaData.exclud$words > 1000 & metaData.exclud$words <=2500,]
words.5000<-metaData.exclud[metaData.exclud$words > 2500 & metaData.exclud$words <=5000,]
words.10000<-metaData.exclud[metaData.exclud$words > 5000 & metaData.exclud$words <=10000,]
words.30000<-metaData.exclud[metaData.exclud$words > 10000 & metaData.exclud$words <=30000,]
words.50000<-metaData.exclud[metaData.exclud$words > 30000 & metaData.exclud$words <=50000,]
words.100000<-metaData.exclud[metaData.exclud$words > 50000 & metaData.exclud$words <=100000,]
words.100000plus<-metaData.exclud[metaData.exclud$words > 100000 ,]


wordVector<-c("Podfic", "Art", "Video" , "100 & <","100-1k","1K-2.5k","2.5k-5k","5k-10k","10k-30k","30k-50k","50k-100k",">100k")
wordNums<- c(nrow(words.podfic),nrow(words.art),nrow(words.vid),nrow(words.100),nrow(words.1000),nrow(words.2500),nrow(words.5000),nrow(words.10000),nrow(words.30000),nrow(words.50000),nrow(words.100000),nrow(words.100000plus))

word.freq<-data.frame("Words" = wordVector, "Freq" = wordNums)
word.freq$Words<-factor(word.freq$Words, ordered = TRUE, levels=word.freq$Words)

word.freq.p<- word.freq  %>%
  ggplot( aes(x=Words, y=Freq, label = Freq)) +
  geom_bar(stat="identity", fill="#008080", alpha=.6, width=.4) +
  geom_text(color="black", size=5, position=position_dodge(width=0.9), vjust=-0.25) +
  labs(subtitle="Fanwork/Fanfic Word Count Distribution", x= 'Fanwork/Word Count', y= 'No. of Fanworks/Words') +
  theme_classic() +   theme(text = element_text(size=15)) +
  theme(panel.border = element_blank(), panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line( size=.1, color="grey" ) ,
        axis.line = element_line(colour = "black",size=1),axis.ticks = element_line(colour = "black",size=1), 
        axis.title = element_text(size=15),
        axis.text.x  = element_text(colour = "black",size=15,angle = 60,vjust = 1, hjust = 1),
        axis.text.y  = element_text(colour = "black",size=15))
word.freq.p


nrow(metaData)
sum(wordNums)

# Most words fic
metaData.exclud[metaData.exclud$words == max(metaData.exclud$words),]
words.100[words.100$words == min(words.100$words),]

head(metaData.exclud[order(-metaData.exclud$words),])


#####
# Kudos Histogram
##### -------------------------------------------------------------> #####
metaData$kudos<-as.numeric(metaData$kudos)

kudos.0<-metaData[is.na(metaData$kudos),]
metaData.kudos<- metaData[!is.na(metaData$kudos),]

kudos.10<-metaData.kudos[metaData.kudos$kudos <=10,]
kudos.25<-metaData.kudos[metaData.kudos$kudos > 10 & metaData.kudos$kudos <=25,]
kudos.50<-metaData.kudos[metaData.kudos$kudos > 25 & metaData.kudos$kudos <=50,]
kudos.100<-metaData.kudos[metaData.kudos$kudos > 50 & metaData.kudos$kudos <=100,]
kudos.200<-metaData.kudos[metaData.kudos$kudos > 100 & metaData.kudos$kudos <=200,]
kudos.300<-metaData.kudos[metaData.kudos$kudos > 200 & metaData.kudos$kudos <=300,]
kudos.500<-metaData.kudos[metaData.kudos$kudos > 300 & metaData.kudos$kudos <=500,]
kudos.1000<-metaData.kudos[metaData.kudos$kudos > 500 & metaData.kudos$kudos <=1000,]
kudos.1000plus<-metaData.kudos[metaData.kudos$kudos > 1000 ,]

kudosVector<-c (">10","11-25", "26-50","51-100","101-200","201-300","301-500","501-1k", ">1k")
kudosNums<- c((nrow(kudos.10)+ nrow(kudos.0)),nrow(kudos.25),nrow(kudos.50),nrow(kudos.100),nrow(kudos.200),nrow(kudos.300),nrow(kudos.500),nrow(kudos.1000),nrow(kudos.1000plus))

kudos.freq<-data.frame("Kudos" = kudosVector, "Freq" = kudosNums)
kudos.freq$Kudos<-factor(kudos.freq$Kudos, ordered = TRUE, levels=kudos.freq$Kudos)

kudos.freq.p<- kudos.freq  %>%
  ggplot( aes(x=Kudos, y=Freq, label = Freq)) +
  geom_bar(stat="identity", fill="#008080", alpha=.6, width=.4) +
  geom_text(color="black", size=5, position=position_dodge(width=0.9), vjust=-0.25) +
  labs(subtitle="Kudos Count Distribution", x= 'Kudos Count', y= 'No. of Kudos') +
  theme_classic() +   theme(text = element_text(size=15)) +
  theme(panel.border = element_blank(), panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line( size=.1, color="grey" ) ,
        axis.line = element_line(colour = "black",size=1),axis.ticks = element_line(colour = "black",size=1), 
        axis.title = element_text(size=15),
        axis.text.x  = element_text(colour = "black",size=15,angle = 60,vjust = 1, hjust = 1),
        axis.text.y  = element_text(colour = "black",size=15))
kudos.freq.p

nrow(metaData)
sum(kudosNums)

# Most kudo-ed fic
metaData.kudos[metaData.kudos$kudos == max(metaData.kudos$kudos),]
head(metaData.kudos[order(-metaData.kudos$kudos),])


#####
# Comments Histogram
##### -------------------------------------------------------------> #####
metaData$comments<-as.numeric(metaData$comments)

comments.0<-metaData[is.na(metaData$comments),]
metaData.comments<- metaData[!is.na(metaData$comments),]

comments.5<-metaData.comments[metaData.comments$comments <=5,]
comments.10<-metaData.comments[metaData.comments$comments > 5 & metaData.comments$comments <=10,]
comments.20<-metaData.comments[metaData.comments$comments > 10 & metaData.comments$comments <=20,]
comments.30<-metaData.comments[metaData.comments$comments > 20 & metaData.comments$comments <=30,]
comments.50<-metaData.comments[metaData.comments$comments > 30 & metaData.comments$comments <=50,]
comments.100<-metaData.comments[metaData.comments$comments > 50 & metaData.comments$comments <=100,]
comments.200<-metaData.comments[metaData.comments$comments > 100 & metaData.comments$comments <=200,]
comments.200plus<-metaData.comments[metaData.comments$comments > 200,]

commVector<-c ("0","1-5","6-10", "11-20","21-30","31-50","51-100","101-200",">200")
commNums<- c(nrow(comments.0),nrow(comments.5),nrow(comments.10),nrow(comments.20),nrow(comments.30),nrow(comments.50),nrow(comments.100),nrow(comments.200),nrow(comments.200plus))

comm.freq<-data.frame("Comments" = commVector, "Freq" = commNums)
comm.freq$Comments<-factor(comm.freq$Comments, ordered = TRUE, levels=comm.freq$Comments)

comm.freq.p<- comm.freq  %>%
  ggplot( aes(x=Comments, y=Freq, label = Freq)) +
  geom_bar(stat="identity", fill="#008080", alpha=.6, width=.4) +
  geom_text(color="black", size=5, position=position_dodge(width=0.9), vjust=-0.25) +
  labs(subtitle="Comments Count Distribution", x= 'Comments Count', y= 'No. of Comments') +
  theme_classic() +   theme(text = element_text(size=15)) +
  theme(panel.border = element_blank(), panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line( size=.1, color="grey" ) ,
        axis.line = element_line(colour = "black",size=1),axis.ticks = element_line(colour = "black",size=1), 
        axis.title = element_text(size=15),
        axis.text.x  = element_text(colour = "black",size=15,angle = 60,vjust = 1, hjust = 1),
        axis.text.y  = element_text(colour = "black",size=15))
comm.freq.p


nrow(metaData)
sum(commNums)


# Most comment-ed fic
metaData.comments[metaData.comments$comments == max(metaData.comments$comments),]
head(metaData.comments[order(-metaData.comments$comments),])


#####
# Bookmarks Histogram
##### -------------------------------------------------------------> #####
metaData$bookmarks<-as.numeric(metaData$bookmarks)

book.0<-metaData[is.na(metaData$bookmarks),]
metaData.book<- metaData[!is.na(metaData$bookmarks),]

book.2<-metaData.book[metaData.book$bookmarks <=2,]
book.5<-metaData.book[metaData.book$bookmarks > 2 & metaData.book$bookmarks  <=5,]
book.10<-metaData.book[metaData.book$bookmarks > 5 & metaData.book$bookmarks <=10,]
book.20<-metaData.book[metaData.book$bookmarks > 10 & metaData.book$bookmarks <=20,]
book.30<-metaData.book[metaData.book$bookmarks > 20 & metaData.book$bookmarks <=30,]
book.50<-metaData.book[metaData.book$bookmarks > 30 & metaData.book$bookmarks <=50,]
book.100<-metaData.book[metaData.book$bookmarks > 50 & metaData.book$bookmarks <=100,]
book.100plus<-metaData.book[metaData.book$bookmarks > 100 ,]


bookVector<-c ("0","1-2","3-5","6-10", "11-20","21-30","31-50","51-100",">100")
bookNums<- c(nrow(book.0),nrow(book.2),nrow(book.5),nrow(book.10),nrow(book.20),nrow(book.30),nrow(book.50),nrow(book.100),nrow(book.100plus))

book.freq<-data.frame("Bookmarks" = bookVector, "Freq" = bookNums)
book.freq$Bookmarks<-factor(book.freq$Bookmarks, ordered = TRUE, levels=book.freq$Bookmarks)

book.freq.p<- book.freq  %>%
  ggplot( aes(x=Bookmarks, y=Freq, label = Freq)) +
  geom_bar(stat="identity", fill="#008080", alpha=.6, width=.4) +
  geom_text(color="black", size=5, position=position_dodge(width=0.9), vjust=-0.25) +
  labs(subtitle="Bookmarks Count Distribution", x= 'Bookmarks Count', y= 'No. of Bookmarks') +
  theme_classic() +   theme(text = element_text(size=15)) +
  theme(panel.border = element_blank(), panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line( size=.1, color="grey" ) ,
        axis.line = element_line(colour = "black",size=1),axis.ticks = element_line(colour = "black",size=1), 
        axis.title = element_text(size=15),
        axis.text.x  = element_text(colour = "black",size=15,angle = 60,vjust = 1, hjust = 1),
        axis.text.y  = element_text(colour = "black",size=15))
book.freq.p


nrow(metaData)
sum(bookNums)


# Most bookmarked-ed fic
metaData.book[metaData.book$bookmarks == max(metaData.book$bookmarks),]
metaData.book[order(-metaData.book$bookmarks),][5:9,]


##### 
# Hits Histogram
##### -------------------------------------------------------------> #####
metaData$hits<-as.numeric(metaData$hits)

hits.0<-metaData[is.na(metaData$hits),]
metaData.hits<- metaData[!is.na(metaData$hits),]

hits.100<-metaData.hits[metaData.hits$hits <=100,]
hits.250<-metaData.hits[metaData.hits$hits > 100 & metaData.hits$hits <=250,]
hits.500<-metaData.hits[metaData.hits$hits > 250 & metaData.hits$hits <=500,]
hits.1000<-metaData.hits[metaData.hits$hits > 500 & metaData.hits$hits <=1000,]
hits.2000<-metaData.hits[metaData.hits$hits > 1000 & metaData.hits$hits <=2000,]
hits.5000<-metaData.hits[metaData.hits$hits > 2000 & metaData.hits$hits <=5000,]
hits.10000<-metaData.hits[metaData.hits$hits > 5000 & metaData.hits$hits <=10000,]
hits.10000plus<-metaData.hits[metaData.hits$hits > 10000 ,]

hitsVector<-c ("100 & <","101-250","251-500" ,"501-1k","1k-2k","2k-5k","5k-10k",">10k")
hitsNums<- c((nrow(hits.100)+ nrow(hits.0)),   nrow(hits.250), nrow(hits.500),nrow(hits.1000),nrow(hits.2000),nrow(hits.5000),nrow(hits.10000),nrow(hits.10000plus))

hits.freq<-data.frame("Hits" = hitsVector, "Freq" = hitsNums)
hits.freq$Hits<-factor(hits.freq$Hits, ordered = TRUE, levels=hits.freq$Hits)

hits.freq.p<- hits.freq  %>%
  ggplot( aes(x=Hits, y=Freq, label = Freq)) +
  geom_bar(stat="identity", fill="#008080", alpha=.6, width=.4) +
  geom_text(color="black", size=5, position=position_dodge(width=0.9), vjust=-0.25) +
  labs(subtitle="Hits Count Distribution", x= 'Hits Count', y= 'No. of Hits') +
  theme_classic() +   theme(text = element_text(size=15)) +
  theme(panel.border = element_blank(), panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line( size=.1, color="grey" ) ,
        axis.line = element_line(colour = "black",size=1),axis.ticks = element_line(colour = "black",size=1), 
        axis.title = element_text(size=15),
        axis.text.x  = element_text(colour = "black",size=15,angle = 60,vjust = 1, hjust = 1),
        axis.text.y  = element_text(colour = "black",size=15))
hits.freq.p

nrow(metaData)
sum(hitsNums)

# Most hits-ed fic
metaData.hits[metaData.hits$hits == max(metaData.hits$hits),]
head(metaData.hits[order(-metaData.hits$hits),])



##### 
# Category Histogram
##### -------------------------------------------------------------> #####
category<-strsplit(as.character(metaData$category), ", ")
category<-unlist(category, recursive = FALSE)
category.freq<-as.data.frame(table(category))

colnames(category.freq)<-c("Category","Freq")
category.freq$Percent<- category.freq$Freq/sum(category.freq$Freq)*100
category.freq$Percent<-as.numeric(formatC(category.freq$Percent, digits = 2, format = "f"))
category.freq<-category.freq[  with(category.freq, order(-category.freq$Percent)), ]
category.freq$Category<-factor(category.freq$Category, ordered = TRUE, levels=category.freq$Category)

category.freq.p<- category.freq  %>%
  ggplot( aes(x=Category, y=Percent, label = Percent)) +
  geom_bar(stat="identity", fill="#008080", alpha=.6, width=.4) +
  geom_text(color="black", size=5, position=position_dodge(width=0.9), vjust=-0.25) +
  labs(subtitle="Category Distribution", x= 'Category', y= 'Percentage of Fanworks (%)') +
  theme_classic() +   theme(text = element_text(size=15)) +
  theme(panel.border = element_blank(), panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line( size=.1, color="grey" ) ,
        axis.line = element_line(colour = "black",size=1),axis.ticks = element_line(colour = "black",size=1), 
        axis.title = element_text(size=15),
        axis.text.x  = element_text(colour = "black",size=15,angle = 60,vjust = 1, hjust = 1),
        axis.text.y  = element_text(colour = "black",size=15))
category.freq.p



##### 
# Rating Spread
##### -------------------------------------------------------------> #####
rating.freq<-as.data.frame(table(metaData$rating))
colnames(rating.freq)<-c("Rating","Freq")
rating.freq$Percent<- rating.freq$Freq/sum(rating.freq$Freq)*100
rating.freq$Percent<-as.numeric(formatC(rating.freq$Percent, digits = 2, format = "f"))
rating.freq<-rating.freq[  with(rating.freq, order(-rating.freq$Percent)), ]
rating.freq$Rating<-factor(rating.freq$Rating, ordered = TRUE, levels=rating.freq$Rating)

rating.freq.p<- rating.freq  %>%
  ggplot( aes(x=Rating, y=Percent, label = Percent)) +
  geom_bar(stat="identity", fill="#008080", alpha=.6, width=.4) +
  geom_text(color="black", size=5, position=position_dodge(width=0.9), vjust=-0.25) +
  labs(subtitle="Rating Distribution", x= 'Rating', y= 'Percentage of Fanworks (%)') +
  theme_classic() +   theme(text = element_text(size=15)) +
  theme(panel.border = element_blank(), panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line( size=.1, color="grey" ) ,
        axis.line = element_line(colour = "black",size=1),axis.ticks = element_line(colour = "black",size=1), 
        axis.title = element_text(size=15),
        axis.text.x  = element_text(colour = "black",size=15,angle = 60,vjust = 1, hjust = 1),
        axis.text.y  = element_text(colour = "black",size=15))
rating.freq.p



##### 
# Relationship Spread
##### -------------------------------------------------------------> #####
relationship<-strsplit(as.character(metaData$relationship), ", ")
relationship<-unlist(relationship, recursive = FALSE)

# Jingyan/Linshu retags
relationship[relationship %in% 'Jing Su '] <- 'Lin Shu | Mei Changsu | Su Zhe/Xiao Jingyan'
relationship[relationship %in% 'Su Jing '] <- 'Lin Shu | Mei Changsu | Su Zhe/Xiao Jingyan'
relationship[relationship %in% 'Shu Yan '] <- 'Lin Shu | Mei Changsu | Su Zhe/Xiao Jingyan'
relationship[relationship %in% 'Xiao Jing Yan/Lin Shu'] <-'Lin Shu | Mei Changsu | Su Zhe/Xiao Jingyan'
relationship[relationship %in% 'Lin Shu | Mei Changsu | Su Zhe/Xiao Jing Yan'] <-'Lin Shu | Mei Changsu | Su Zhe/Xiao Jingyan'
relationship[relationship %in% 'hinted Xiao Jingyan/Lin Shu | Mei Changsu | Su Zhe'] <- 'Lin Shu | Mei Changsu | Su Zhe/Xiao Jingyan'
relationship[relationship %in% 'Past Lin Shu | Mei Changsu | Su Zhe/Xiao Jingyan'] <- 'Lin Shu | Mei Changsu | Su Zhe/Xiao Jingyan'
relationship[relationship %in% 'Mei Changsu/Xiao Jingyan'] <- 'Lin Shu | Mei Changsu | Su Zhe/Xiao Jingyan'
relationship[relationship %in% 'Mei Chang Su/Xiao Jingyan'] <- 'Lin Shu | Mei Changsu | Su Zhe/Xiao Jingyan'
relationship[relationship %in% 'Mei Chang Su /Xiao Jing Yan '] <- 'Lin Shu | Mei Changsu | Su Zhe/Xiao Jingyan'
relationship[relationship %in% 'Lin Shu/Xiao Jingyan'] <- 'Lin Shu | Mei Changsu | Su Zhe/Xiao Jingyan'
relationship[relationship %in% 'Lin Shu/Xiao Jing Yan'] <- 'Lin Shu | Mei Changsu | Su Zhe/Xiao Jingyan'
relationship[relationship %in% '(also Jingyan/Lin Shu but what else is new)'] <- 'Lin Shu | Mei Changsu | Su Zhe/Xiao Jingyan'

relationship[relationship %in% 'Lin Shu | Mei Changsu | Su Zhe & Xiao Jing Yan'] <- 'Lin Shu | Mei Changsu | Su Zhe & Xiao Jingyan'
relationship[relationship %in% 'Lin Shu & Xiao Jingyan'] <- 'Lin Shu | Mei Changsu | Su Zhe & Xiao Jingyan'
relationship[relationship %in% 'Lin Shu | Mei Chang Su & Prince Jing | Jingyan'] <- 'Lin Shu | Mei Changsu | Su Zhe & Xiao Jingyan'
relationship[relationship %in% 'Xiao Jingyan & Mei Changsu'] <- 'Lin Shu | Mei Changsu | Su Zhe & Xiao Jingyan'
relationship[relationship %in% 'Mei Changsu & Prince Jingyan'] <- 'Lin Shu | Mei Changsu | Su Zhe & Xiao Jingyan'
relationship[relationship %in% 'Xiao Jingyan & Mei Changsu'] <- 'Lin Shu | Mei Changsu | Su Zhe & Xiao Jingyan'
relationship[relationship %in% 'Mei Changsu & Xiao Jingyan'] <- 'Lin Shu | Mei Changsu | Su Zhe & Xiao Jingyan'

# Nihuang/Linshu retags
relationship[relationship %in% 'Lin Shu/Mu Ni Huang'] <- 'Lin Shu | Mei Changsu | Su Zhe/Mu Nihuang'
relationship[relationship %in% 'Lin Shu/Mu Nihuang'] <- 'Lin Shu | Mei Changsu | Su Zhe/Mu Nihuang'
relationship[relationship %in% 'Lin Shu/Nihuang'] <- 'Lin Shu | Mei Changsu | Su Zhe/Mu Nihuang'
relationship[relationship %in% 'Lin Shu | Mei Chang Su/Nihuang'] <- 'Lin Shu | Mei Changsu | Su Zhe/Mu Nihuang'

relationship[relationship %in% 'Lin Shu | Mei Changsu | Su Zhe & Mu Ni Huang'] <- 'Lin Shu | Mei Changsu | Su Zhe & Mu Nihuang'

# Lin Chen/Linshu retags
relationship[relationship %in% 'Lin Chen/ Lin Shu | Mei Changsu | Su Zhe'] <- 'Lin Chen/Lin Shu | Mei Changsu | Su Zhe'
relationship[relationship %in% 'Lin Shu | Mei Changsu | Su Zhe/Lin Chen'] <- 'Lin Chen/Lin Shu | Mei Changsu | Su Zhe'
relationship[relationship %in% 'Lin Chen/Mei Chang Su | Su Zhe | Lin Su'] <- 'Lin Chen/Lin Shu | Mei Changsu | Su Zhe'
relationship[relationship %in% 'Lin Shu I Mei Changsu I Su Zhe/Lin Chen'] <- 'Lin Chen/Lin Shu | Mei Changsu | Su Zhe'
relationship[relationship %in% 'Lin Chen/Mei Changsu'] <- 'Lin Chen/Lin Shu | Mei Changsu | Su Zhe'
relationship[relationship %in% 'one-sided Lin Shu | Mei Changsu | Su Zhe/Lin Chen'] <- 'Lin Chen/Lin Shu | Mei Changsu | Su Zhe'

relationship[relationship %in% 'Lin Shu | Mei Changsu | Su Zhe & Lin Chen'] <- 'Lin Chen & Lin Shu | Mei Changsu | Su Zhe'

# Jingyan/Zhanying retags
relationship[relationship %in% 'Lie Zhan Ying/Xiao Jing Yan'] <- 'Lie Zhanying/Xiao Jingyan'
relationship[relationship %in% 'Xiao Jingyan/Lie Zhanying'] <- 'Lie Zhanying/Xiao Jingyan'
relationship[relationship %in% 'one-sided Lie Zhanying/Xiao Jingyan'] <- 'Lie Zhanying/Xiao Jingyan'

# Jingrui/Yujin retags
relationship[relationship %in% 'Xiao Jing Rui/Yan Yu Jin'] <-'Xiao Jingrui/Yan Yujin'
relationship[relationship %in% 'Yan Yu Jin/Xiao Jing Rui'] <-'Xiao Jingrui/Yan Yujin'
relationship[relationship %in% 'Yan Yujin/Xiao Jingrui'] <-'Xiao Jingrui/Yan Yujin'
relationship[relationship %in% 'Xiao Jing Rui |Xiao Jing Rui/Yan Yu Jin |Yan Yu Jin'] <-'Xiao Jingrui/Yan Yujin'
relationship[relationship %in% 'Xiao Jingrui/Yan Yu Jin'] <-'Xiao Jingrui/Yan Yujin'

#Linshu/LinChen/Jingyan
relationship[relationship %in% 'Xiao Jingyan/Mei Changsu/Lin Chen'] <- 'Lin Chen/Lin Shu | Mei Changsu | Su Zhe/Xiao Jingyan'
relationship[relationship %in% 'Xiao Jingyan/Lin Shu/Li Chen'] <- 'Lin Chen/Lin Shu | Mei Changsu | Su Zhe/Xiao Jingyan'
relationship[relationship %in% 'Xiao Jingyan/Lin Shu | Mei Changsu | Su Zhe/Lin Chen'] <-'Lin Chen/Lin Shu | Mei Changsu | Su Zhe/Xiao Jingyan'
relationship[relationship %in% 'Lin Chen/Xiao Jing Yan/Lin Shu | Mei Changsu | Su Zhe'] <- 'Lin Chen/Lin Shu | Mei Changsu | Su Zhe/Xiao Jingyan'
relationship[relationship %in% 'Lin Shu | Mei Changsu | Su Zhe/Xiao Jingyan/Lin Chen'] <- 'Lin Chen/Lin Shu | Mei Changsu | Su Zhe/Xiao Jingyan'
relationship[relationship %in% 'Lin Chen/ Lin Shu | Mei Changsu | Su Zhe/Xiao Jingyan'] <- 'Lin Chen/Lin Shu | Mei Changsu | Su Zhe/Xiao Jingyan'


#Linshu/Nihuang/Jingyan
relationship[relationship %in% 'Lin Shu | Mei Changsu | Su Zhe/Xiao Jingyan/Mu Ni Huang'] <- 'Lin Shu | Mei Changsu | Su Zhe/Mu Nihuang/Xiao Jingyan'
relationship[relationship %in% 'Mu Nihuang/Xiao Jingyan/Mei Changsu'] <- 'Lin Shu | Mei Changsu | Su Zhe/Mu Nihuang/Xiao Jingyan'
relationship[relationship %in% 'Mu Nihuang/Lin Shu/Xiao Jingyan'] <- 'Lin Shu | Mei Changsu | Su Zhe/Mu Nihuang/Xiao Jingyan'
relationship[relationship %in% 'Lin Shu/Xiao Jingyan/Mu Nihuang'] <- 'Lin Shu | Mei Changsu | Su Zhe/Mu Nihuang/Xiao Jingyan'


relationship[relationship %in% 'Lin Shu | Mei Changsu | Su Zhe & Xiao Jing Yan & Mu Ni Huang'] <- 'Lin Shu | Mei Changsu | Su Zhe & Mu Nihuang & Xiao Jingyan'
relationship[relationship %in% 'Lin Shu & Xiao Jingyan & Mu Nihuang'] <- 'Lin Shu | Mei Changsu | Su Zhe & Mu Nihuang & Xiao Jingyan'

#Tingsheng/Feiliu
relationship[relationship %in% 'Xiao Tingsheng/Fei Liu'] <- 'Xiao Tingsheng/Fei Liu'
relationship[relationship %in% 'Tingsheng/Fei Liu'] <- 'Xiao Tingsheng/Fei Liu'

#Linchen/Jingyan
relationship[relationship %in% 'Lin Chen/ Xiao Jing Yan'] <- 'Lin Chen/Xiao Jingyan'
relationship[relationship %in% 'Lin Chen / Xiao Jingyan'] <- 'Lin Chen/Xiao Jingyan'

relationship[relationship %in% 'Lin Chen & Xiao Jing Yan'] <- 'Lin Chen & Xiao Jingyan'
relationship[relationship %in% 'Xiao Jingyan & Lin Chen'] <- 'Lin Chen & Xiao Jingyan'

#Jingrui/Linshu
relationship[relationship %in% 'Xiao Jing Rui/Lin Shu'] <- 'Lin Shu | Mei Changsu | Su Zhe/Xiao Jingrui'
relationship[relationship %in% 'Lin Shu | Mei Changsu | Su Zhe/Xiao Jingrui (one-sided)'] <- 'Lin Shu | Mei Changsu | Su Zhe/Xiao Jingrui'
relationship[relationship %in% 'mentioned fem!lin shu/jingrui'] <- 'Lin Shu | Mei Changsu | Su Zhe/Xiao Jingrui'

#Nihuang/Jingyan
relationship[relationship %in% 'Mu Ni Huang/Xiao Jing Yan'] <-'Mu Nihuang/Xiao Jingyan'
relationship[relationship %in% 'Xiao Jingyan/Mu Nihuang'] <-'Mu Nihuang/Xiao Jingyan'
relationship[relationship %in% 'Mu Nihuang/Jingyan'] <-'Mu Nihuang/Xiao Jingyan'

#Linshu/linshu
relationship[relationship %in% 'Lin Shu/Mei Changsu'] <- 'Lin Shu | Mei Changsu | Su Zhe/Lin Shu | Mei Changsu | Su Zhe'
relationship[relationship %in% 'Lin Shu/Mei Changsu - Relationship'] <- 'Lin Shu | Mei Changsu | Su Zhe/Lin Shu | Mei Changsu | Su Zhe'

#Feiliu/changsu
relationship[relationship %in% 'Mei Changsu & Fei Liu'] <- 'Fei Liu & Lin Shu | Mei Changsu | Su Zhe'
relationship[relationship %in% 'Fei Liu & Mei Changsu'] <- 'Fei Liu & Lin Shu | Mei Changsu | Su Zhe'

#others
relationship[relationship %in% 'Lin Chen/Xiao Jingyan'] <-'Lin Chen/Xiao Jingyan'
relationship[relationship %in% 'Lie Zhan Ying/Lin Shu | Mei Changsu | Su Zhe'] <-'Lie Zhanying/Lin Shu | Mei Changsu | Su Zhe'
relationship[relationship %in% 'Xiao Jingyan/Miss Liu'] <- 'Xiao Jingyan/Consort Liu'
relationship[relationship %in% 'One-sided Xiao Jinghuan/Mei Changsu'] <- 'Lin Shu | Mei Changsu | Su Zhe/Xiao Jinghuan'
relationship[relationship %in% 'Qin Banruo/Princess Consort Yu'] <- "Qin Banruo/Xiao Jinghuan's wife"
relationship[relationship %in% 'Lie Zhan Ying/Lin Shu | Mei Changsu | Su Zhe/Xiao Jing Yan'] <- 'Lie Zhanying/Lin Shu | Mei Changsu | Su Zhe/Xiao Jingyan'
relationship[relationship %in% 'Consort Jing/ Consort Chen'] <- 'Consort Jing/Consort Chen'
relationship[relationship %in% 'Mu Ni Huang/Xia Dong'] <- 'Mu Nihuang/Xia Dong'
relationship[relationship %in% 'Gong Yu/Xiao Jingrui/Yan Yujin'] <- 'Gong Yu/Yan Yujin/Xiao Jingrui'
relationship[relationship %in% 'Meng Zhi/Mei Changsu'] <- 'Lin Shu | Mei Changsu | Su Zhe/Meng Zhi'
relationship[relationship %in% 'Mei Changsu/Xiao Jinghuan'] <- 'Lin Shu | Mei Changsu | Su Zhe/Xiao Jinghuan'
relationship[relationship %in% 'Lie Zhan Ying/Xiao Jing Rui'] <- 'Lie Zhanying/Xiao Jingrui'
relationship[relationship %in% 'Lin Shu/Meng Zhi (sort of)'] <- 'Lin Shu | Mei Changsu | Su Zhe/Meng Zhi'
relationship[relationship %in% '(maybe Consort Jing / Lin Xie / Princess Jinyang if you squint)'] <- 'Consort Jing/Grand Princess Jinyang/Lin Xie'
relationship[relationship %in% 'Lin Xie/Princess Jinyang'] <- 'Grand Princess Jinyang/Lin Xie'
relationship[relationship %in% 'Xiao Xuan/Gao Zhan'] <- 'Consort Jing/Eunuch Gao'
relationship[relationship %in% 'Consort Jing/Gao Zhan'] <- 'Consort Jing/Eunuch Gao'
relationship[relationship %in% '(or Consort Jing / Consort Chen if you squint again)'] <- 'Consort Jing/Consort Chen'


relationship[relationship %in% 'Consort Jing & Consort Chen'] <- 'Consort Chen & Consort Jing'
relationship[relationship %in% 'Gong Yu & Mu Ni Huang'] <- 'Gong Yu & Mu Nihuang'
relationship[relationship %in% 'Xiao Jingrui & Mei Changsu'] <- 'Lin Shu | Mei Changsu | Su Zhe & Xiao Jingrui'
relationship[relationship %in% 'Xiao Jinghuan & Mei Changsu'] <- 'Lin Shu | Mei Changsu | Su Zhe & Xiao Jinghuan'
relationship[relationship %in% 'Meng Zhi & Lin Shu | Mei Changsu | Su Zhe'] <- 'Lin Shu | Mei Changsu | Su Zhe & Meng Zhi'



################################ To shorten the x labels
relationship[relationship %in% 'Lin Shu | Mei Changsu | Su Zhe/Xiao Jingyan'] <- 'Lin Shu / Xiao Jingyan'
##
relationship[relationship %in% 'Lin Chen/Lin Shu | Mei Changsu | Su Zhe/Xiao Jingyan'] <- 'Lin Chen / Lin Shu / Xiao Jingyan'
##
relationship[relationship %in%  'Lin Shu | Mei Changsu | Su Zhe/Mu Nihuang/Xiao Jingyan'] <-  'Lin Shu / Mu Nihuang / Xiao Jingyan'
##
relationship[relationship %in% 'Lin Chen/Lin Shu | Mei Changsu | Su Zhe'] <- 'Lin Chen / Lin Shu'
##
relationship[relationship %in% 'Lin Shu | Mei Changsu | Su Zhe/Xiao Jinghuan'] <- 'Lin Shu / Xiao Jinghuan'
##
relationship[relationship %in% 'Lin Shu | Mei Changsu | Su Zhe/Meng Zhi'] <- 'Lin Shu / Meng Zhi'
##
relationship[relationship %in% 'Lin Shu | Mei Changsu | Su Zhe/Mu Nihuang'] <- 'Lin Shu / Mu Nihuang'

relationship[relationship %in% 'Lin Chen/Xiao Jingyan'] <- 'Lin Chen / Xiao Jingyan'
##
relationship[relationship %in% 'Lie Zhanying/Xiao Jingyan'] <- 'Lie Zhanying / Xiao Jingyan'
##
relationship[relationship %in% 'Xiao Jingrui/Yan Yujin'] <- 'Xiao Jingrui / Yan Yujin'

################################ To shorten the x labels
relationship[relationship %in% 'Lin Shu | Mei Changsu | Su Zhe & Xiao Jingyan'] <- 'Lin Shu & Xiao Jingyan'
##
relationship[relationship %in% 'Lin Chen & Lin Shu | Mei Changsu | Su Zhe'] <- 'Lin Chen & Lin Shu '
##
relationship[relationship %in%  'Fei Liu & Lin Shu | Mei Changsu | Su Zhe'] <-  'Fei Liu & Lin Shu'
##
relationship[relationship %in% 'Lin Shu | Mei Changsu | Su Zhe & Mu Nihuang'] <- 'Lin Shu & Mu Nihuang'
##
relationship[relationship %in% 'Lin Shu | Mei Changsu | Su Zhe & Meng Zhi'] <- 'Lin Shu & Meng Zhi'
##
relationship[relationship %in% 'Lin Shu | Mei Changsu | Su Zhe & Xiao Jingrui'] <- 'Lin Shu & Xiao Jingrui'


# plots
relationship.freq<-as.data.frame(table(relationship))
colnames(relationship.freq)<-c("Relationship","Freq")
relationship.freq<-relationship.freq[  with(relationship.freq, order(-relationship.freq$Freq)), ]
relationship.freq

# Top ten romantic pairing
relation.rom<-relationship.freq[str_detect(relationship.freq$Relationship, "/"),]
rom.10<-relation.rom[1:10,]
rom.10$Percent<- rom.10$Freq/nrow(metaData)*100
rom.10$Percent<-as.numeric(formatC(rom.10$Percent, digits = 2, format = "f"))
rom.10<-rom.10[  with(rom.10, order(-rom.10$Percent)), ]
rom.10$Relationship<-factor(rom.10$Relationship, ordered = TRUE, levels=rom.10$Relationship)

rom.freq.p<- rom.10  %>%
  ggplot( aes(x=Relationship, y=Percent, label = Percent)) +
  geom_bar(stat="identity", fill="#008080", alpha=.6, width=.4) +
  geom_text(color="black", size=5, position=position_dodge(width=0.9), vjust=-0.25) +
  labs(subtitle="Romantic Pairing Distribution", x= 'Pairing', y= 'Percentage of Fanworks (%)') +
  theme_classic() +   theme(text = element_text(size=15)) +
  theme(panel.border = element_blank(), panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line( size=.1, color="grey" ) ,
        axis.line = element_line(colour = "black",size=1),axis.ticks = element_line(colour = "black",size=1), 
        axis.title = element_text(size=15),
        axis.text.x  = element_text(colour = "black",size=15,angle = 70,vjust = 1, hjust = 1),
        axis.text.y  = element_text(colour = "black",size=15))
#  scale_x_discrete(guide = guide_axis(n.dodge=10))
rom.freq.p



# Top ten gen pairing
relation.gen<-relationship.freq[str_detect(relationship.freq$Relationship, "&"),]
gen.10<-relation.gen[1:10,]
gen.10$Percent<- gen.10$Freq/nrow(metaData)*100
gen.10$Percent<-as.numeric(formatC(gen.10$Percent, digits = 2, format = "f"))
gen.10<-gen.10[  with(gen.10, order(-gen.10$Percent)), ]
gen.10$Relationship<-factor(gen.10$Relationship, ordered = TRUE, levels=gen.10$Relationship)

gen.freq.p<- gen.10  %>%
  ggplot( aes(x=Relationship, y=Percent, label = Percent)) +
  geom_bar(stat="identity", fill="#008080", alpha=.6, width=.4) +
  geom_text(color="black", size=5, position=position_dodge(width=0.9), vjust=-0.25) +
  labs(subtitle="Gen Pairing Distribution", x= 'Pairing', y= 'Percentage of Fanworks (%)') +
  theme_classic() +   theme(text = element_text(size=15)) +
  theme(panel.border = element_blank(), panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line( size=.1, color="grey" ) ,
        axis.line = element_line(colour = "black",size=1),axis.ticks = element_line(colour = "black",size=1), 
        axis.title = element_text(size=15),
        axis.text.x  = element_text(colour = "black",size=15,angle = 70,vjust = 1, hjust = 1),
        axis.text.y  = element_text(colour = "black",size=15))
#  scale_x_discrete(guide = guide_axis(n.dodge=10))
gen.freq.p




##### 
# Character Spread
##### -------------------------------------------------------------> #####
chara<-strsplit(as.character(metaData$character), ", ")
chara<-unlist(chara, recursive = FALSE)

#retags
# Jingyan
chara[chara %in% 'Xiao Jing Yan'] <- 'Xiao Jingyan'
chara[chara %in% 'Prince Jing | Jingyan'] <- 'Xiao Jingyan'
chara[chara %in% 'prince jingyan'] <- 'Xiao Jingyan'
chara[chara %in% 'Xiao Jing Yan '] <- 'Xiao Jingyan'
chara[chara %in% 'Prince Jing'] <- 'Xiao Jingyan'
chara[chara %in% 'Jingyan'] <- 'Xiao Jingyan'

# Gao Zhan
chara[chara %in% 'Gao'] <- 'Eunuch Gao'
chara[chara %in% 'Gao Zhan'] <- 'Eunuch Gao'

# Xiao Xuan
chara[chara %in% 'The Emperor'] <- 'Xiao Xuan'
chara[chara %in% 'Emperor of Liang'] <- 'Xiao Xuan'
chara[chara %in% 'Emperor Xiao Xuan'] <- 'Xiao Xuan'
chara[chara %in% 'Emperor of Da Liang'] <- 'Xiao Xuan'

# Nihuang
chara[chara %in% 'Mu Ni Huang'] <- 'Mu Nihuang'
chara[chara %in% 'Nihuang'] <- 'Mu Nihuang'

# Yujin
chara[chara %in% 'Yan Yu Jin'] <- 'Yan Yujin'
chara[chara %in% 'Yan Yu Jin |Yan Yu Jin'] <- 'Yan Yujin'
chara[chara %in% 'Yujin'] <- 'Yan Yujin'

# Zhanying
chara[chara %in% 'Lie Zhan Ying'] <- 'Lie Zhanying'
chara[chara %in% 'Lie Zhanying (novel)'] <- 'Lie Zhanying'
chara[chara %in% 'Li Zhanying'] <- 'Lie Zhanying'
chara[chara %in% 'Zhanying'] <- 'Lie Zhanying'

# Jingrui
chara[chara  %in% 'Xiao Jing Rui'] <- 'Xiao Jingrui'
chara[chara  %in% 'Xiao Jinrui'] <- 'Xiao Jingrui'
chara[chara  %in% 'Jingrui'] <- 'Xiao Jingrui'
chara[chara  %in% 'Xiao Jing Rui |Xiao Jing Rui'] <- 'Xiao Jingrui'

# Lin Chen
chara[chara  %in% 'Lin Chen (Nirvana in Fire)'] <- 'Lin Chen'
chara[chara  %in% 'Li Chen'] <- 'Lin Chen'
chara[chara  %in% 'Lin Chen '] <- 'Lin Chen'
chara[chara  %in% 'Li Len'] <- 'Lin Chen'

# Lin Shu
chara[chara  %in% 'Lin Shu'] <- 'Lin Shu | Mei Changsu | Su Zhe'
chara[chara  %in% 'Mei Changsu'] <- 'Lin Shu | Mei Changsu | Su Zhe'
chara[chara  %in% 'Mei Chang Su'] <- 'Lin Shu | Mei Changsu | Su Zhe'
chara[chara  %in% 'Mei Changsu | Su Zhe | Lin Shu'] <- 'Lin Shu | Mei Changsu | Su Zhe'
chara[chara  %in% 'Mei Chansu'] <- 'Lin Shu | Mei Changsu | Su Zhe'
chara[chara  %in% 'Lin Shu | Mei Chang Su'] <- 'Lin Shu | Mei Changsu | Su Zhe'
chara[chara  %in% 'Lin Shu | Mei Changsu | Su Zhe (mentioned)'] <- 'Lin Shu | Mei Changsu | Su Zhe'
chara[chara  %in% 'Mei Chang Su '] <- 'Lin Shu | Mei Changsu | Su Zhe'
chara[chara  %in% 'Lin Shu '] <- 'Lin Shu | Mei Changsu | Su Zhe'
chara[chara  %in% 'LFem!LinShu'] <- 'Lin Shu | Mei Changsu | Su Zhe'

# Jingyu
chara[chara  %in% 'Xiao Jingyu (Prince Qi)'] <- 'Xiao Jingyu | Prince Qi'
chara[chara  %in% 'Xiao Jing Yu | Prince Qi'] <- 'Xiao Jingyu | Prince Qi'
chara[chara  %in% 'Xiao Jingyu (mentioned)'] <- 'Xiao Jingyu | Prince Qi'
chara[chara  %in% 'Prince Qi'] <- 'Xiao Jingyu | Prince Qi'
chara[chara  %in% 'Xiao Jingyu'] <- 'Xiao Jingyu | Prince Qi'
chara[chara  %in% 'Xiao Jing Yu'] <- 'Xiao Jingyu | Prince Qi'

# Tingsheng
chara[chara  %in% 'Tingsheng'] <- 'Xiao Tingsheng'
chara[chara  %in% 'Xiao Ting Sheng'] <- 'Xiao Tingsheng'
chara[chara  %in% 'Ting Sheng'] <- 'Xiao Tingsheng'

# Jinyang
chara[chara  %in% 'Xiao Jingyang'] <- 'Grand Princess Jinyang'
chara[chara  %in% 'Xiao Jinyang'] <- 'Grand Princess Jinyang'
chara[chara  %in% 'Lin Jinyang'] <- 'Grand Princess Jinyang'
chara[chara  %in% 'Princess Jinyang'] <- 'Grand Princess Jinyang'
chara[chara  %in% 'Crown Princess Jinyang'] <- 'Grand Princess Jinyang'

# Yan Que
chara[chara  %in% 'Marquis Yan'] <- 'Yan Que'
chara[chara  %in% 'Marquis Yan Que'] <- 'Yan Que'

# Liu Shan
chara[chara %in% 'Princess Liu'] <- 'Princess Consort Liu'
chara[chara %in% 'Lady Liu (Nirvana in Fire)'] <- 'Princess Consort Liu'
chara[chara %in% "Jingyan's wife"] <- 'Princess Consort Liu'
chara[chara %in% "Jingyan's Consort"] <- 'Princess Consort Liu'
chara[chara %in% "Lady Liu"] <- 'Princess Consort Liu'
chara[chara %in% "Jingyan's Consort"] <- 'Princess Consort Liu'
chara[chara %in% "Empress Liu"] <- 'Princess Consort Liu'
chara[chara %in% "Miss Liu"] <- 'Princess Consort Liu'
chara[chara %in% "Consort Liu"] <- 'Princess Consort Liu'

# Jingyi
chara[chara %in% "Jing Fei  | Consort Jing"] <- 'Consort Jing'
chara[chara %in% "Noble Consort Jing"] <- 'Consort Jing'
chara[chara %in% "Lin Jingyi"] <- 'Consort Jing'
chara[chara %in% "Consort Jing | Lin Jingyi"] <- 'Consort Jing'
chara[chara %in% "Lin Jingyi (Consort Jing)"] <- 'Consort Jing'
chara[chara %in% "Consort Jing (Nirvana in Fire)"] <- 'Consort Jing'

# Meng Zhi
chara[chara %in% "General Meng"] <- 'Meng Zhi'
chara[chara %in% "General Meng Zhi"] <- 'Meng Zhi'

# Fei Liu
chara[chara %in% "Feiliu"] <- 'Fei Liu'
chara[chara %in% "Feil Liu"] <- 'Fei Liu'

# Tai Nai Nai
chara[chara  %in% 'Tainainai'] <- 'Tai Nai Nai'
chara[chara  %in% 'Tai-nainai'] <- 'Tai Nai Nai'
chara[chara  %in% 'Great Grandmother'] <- 'Tai Nai Nai'

# Nian nian
chara[chara  %in% 'Southern Chu Princess Yuwen Nian'] <- 'Yu Wen Nian | Nian-nian'
chara[chara  %in% 'Yuwen Nian'] <- 'Yu Wen Nian | Nian-nian'

# Yueyao
chara[chara  %in% 'Lin Yueyao'] <- 'Consort Chen'
chara[chara  %in% 'Lin Yueyao (Consort Chen)'] <- 'Consort Chen'
chara[chara  %in% 'Noble Consort Chen'] <- 'Consort Chen'
chara[chara  %in% 'Lin Yueyao | Consort Chen'] <- 'Consort Chen'

# Liyang
chara[chara  %in% 'Li Yang'] <- 'Grand Princess Liyang'
chara[chara  %in% 'Princess Supreme Liyang'] <- 'Grand Princess Liyang'
chara[chara  %in% 'Princess Liyang'] <- 'Grand Princess Liyang'
chara[chara  %in% 'Liyang'] <- 'Grand Princess Liyang'

# Yan
chara[chara  %in% 'Dr. Yan (Nirvana in Fire)'] <- 'Physician Yan'
chara[chara  %in% 'Physician Yan (Nirvana in Fire)'] <- 'Physician Yan'

# Jinghuan
chara[chara  %in% 'Prince Yu'] <- 'Xiao Jinghuan'
chara[chara  %in% 'Xiao Jing Huan'] <- 'Xiao Jinghuan'

# Mu Qing
chara[chara %in% "Mu Qing (Nirvana in Fire)"] <- 'Mu Qing'
chara[chara %in% "Female Muqing"] <- 'Mu Qing'
chara[chara %in% "Prince Mu"] <- 'Mu Qing'

# Foya
chara[chara  %in% 'Foya - Character'] <- 'Foya'
chara[chara  %in% 'Foya (Nirvana in Fire)'] <- 'Foya'

# Empress Yan
chara[chara %in% "Empress Yan - Character"] <- 'Empress Yan'
chara[chara %in% "Empress Yan (Nirvana in Fire)"] <- 'Empress Yan'


# Others
chara[chara  %in% 'Grand Empress Dowager (Nirvana in Fire)'] <- 'Grand Empress Dowager'
chara[chara %in% "Noble Consort Yue"] <- 'Consort Yue'
chara[chara  %in% 'Qin Ban Ruo'] <- 'Qin Banruo'
chara[chara  %in% 'Xiao Jing Xuan'] <- 'Xiao Jingxuan'
chara[chara %in% "Li Gang (Nirvana in Fire)"] <- 'Li Gang'
chara[chara  %in% 'Xie Yu (Nirvana in Fire)'] <- 'Xie Yu'
chara[chara %in% "Nie Fung"] <- 'Nie Feng'
chara[chara %in% "Nie Dou"] <- 'Nie Duo'
chara[chara %in% "Auntie Ji"] <- 'Aunt Ji'

## To shorten x label
chara[chara %in% 'Lin Shu | Mei Changsu | Su Zhe'] <- 'Lin Shu'


chara.freq<-as.data.frame(table(chara))
colnames(chara.freq)<-c("Character","Freq")
chara.freq<-chara.freq[  with(chara.freq, order(-chara.freq$Freq)), ]
chara.freq

chara.10<-chara.freq[1:10,]
chara.10$Percent<- chara.10$Freq/nrow(metaData)*100
chara.10$Percent<-as.numeric(formatC(chara.10$Percent, digits = 2, format = "f"))
chara.10<-chara.10[  with(chara.10, order(-chara.10$Percent)), ]
chara.10$Character<-factor(chara.10$Character, ordered = TRUE, levels=chara.10$Character)

chara.freq.p<- chara.10  %>%
  ggplot( aes(x=Character, y=Percent, label = Percent)) +
  geom_bar(stat="identity", fill="#008080", alpha=.6, width=.4) +
  geom_text(color="black", size=5, position=position_dodge(width=0.9), vjust=-0.25) +
  labs(subtitle="Character Tag Distribution", x= 'Character', y= 'Percentage of Fanworks (%)') +
  theme_classic() +   theme(text = element_text(size=15)) +
  theme(panel.border = element_blank(), panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line( size=.1, color="grey" ) ,
        axis.line = element_line(colour = "black",size=1),axis.ticks = element_line(colour = "black",size=1), 
        axis.title = element_text(size=15),
        axis.text.x  = element_text(colour = "black",size=15,angle = 70,vjust = 1, hjust = 1),
        axis.text.y  = element_text(colour = "black",size=15))
#  scale_x_discrete(guide = guide_axis(n.dodge=10))
chara.freq.p



##### 
# Tags Spread
##### -------------------------------------------------------------> #####

tags<-strsplit(as.character(metaData$additional.tags), ", ")
tags<-unlist(tags, recursive = FALSE)

tags.freq<-as.data.frame(table(tolower(tags)))
colnames(tags.freq)<-c("Tag","Freq") 
tags.freq<-tags.freq[  with(tags.freq, order(-tags.freq$Freq)), ] 

tags.10<-tags.freq[1:10,]
tags.10$Percent<- tags.10$Freq/nrow(metaData)*100
tags.10$Percent<-as.numeric(formatC(tags.10$Percent, digits = 2, format = "f"))
tags.10<-tags.10[  with(tags.10, order(-tags.10$Percent)), ]
tags.10$Tag<-factor(tags.10$Tag, ordered = TRUE, levels=tags.10$Tag)

tags.freq.p<- tags.10  %>%
  ggplot( aes(x=Tag, y=Percent, label = Percent)) +
  geom_bar(stat="identity", fill="#008080", alpha=.6, width=.4) +
  geom_text(color="black", size=5, position=position_dodge(width=0.9), vjust=-0.25) +
  labs(subtitle="Additional Tags Distribution", x= 'Tag', y= 'Percentage (%)') +
  theme_classic() +   theme(text = element_text(size=15)) +
  theme(panel.border = element_blank(), panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line( size=.1, color="grey" ) ,
        axis.line = element_line(colour = "black",size=1),axis.ticks = element_line(colour = "black",size=1), 
        axis.title = element_text(size=15),
        axis.text.x  = element_text(colour = "black",size=15,angle = 70,vjust = 1, hjust = 1),
        axis.text.y  = element_text(colour = "black",size=15))
#  scale_x_discrete(guide = guide_axis(n.dodge=10))
tags.freq.p

#####

##### -------------------------------------------------------------> #####
# Figures
year.freq.p
date.freq.p
status.freq.p
word.freq.p
kudos.freq.p
comm.freq.p
book.freq.p
hits.freq.p
category.freq.p
rating.freq.p
rom.freq.p
gen.freq.p
chara.freq.p
tags.freq.p