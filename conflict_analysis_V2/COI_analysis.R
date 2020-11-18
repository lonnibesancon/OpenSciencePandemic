########################################
# Analysis of conflicts of interests
# Date:07/07/2020
# Author: Clemence Leyrat
########################################

library(tidyverse)
library(ggplot2)
library(tableone)
library(aod)
library(ICC)
library(sqldf)
library(gridExtra)

#Change working directory
setwd("OpenSciencePandemic/conflict_analysis_V2/")



#Import
t0days<-read.csv("0 day.csv", sep=",", header=T, stringsAsFactors = FALSE)
t0days$time<-0
summary(t0days)

#Import
t1days<-read.csv("1 day.csv", sep=",", header=T, stringsAsFactors = FALSE)
t1days$time<-1 
summary(t1days)

t16days<-read.csv("16 days.csv", sep=",", header=T, stringsAsFactors = FALSE)
t16days$time<-16
summary(t16days)


t20days<-read.csv("20 days.csv", sep=",", header=T, stringsAsFactors = FALSE)
t20days$time<-20
summary(t20days)


review0<-rbind(t0days,t1days,t16days, t20days)
table(review0$time)

#341 different journals published in a day or less
journals<-unique(review0$URL.of.Editorial.Board[review0$time==0|review0$time==1])

review0$Received<-as.Date(review0$Received, format="%d/%m/%Y")


review<-review0[review0$Received<="2020-06-11",]
review$time[review$time==1]<-0

#Exclusion of records with missing data (because they were duplicates)
review<-review[is.na(review$EIC.is.author)==F,]
summary(review)

table(review$time)
table(review$Binary.type)

table(review$Type.by.me)
review$Type<-ifelse(review$Type.by.me=="Review","Review",review$Binary.type) 
table(review$Type)

#Binary indicator for any conflict of interest
review$any_COI<-ifelse(review$EIC.is.author>0|review$Associate.Editor.is.author>0|review$Editorial.Board.member.is.author>0,1,0)

#Total number of editors as authors
review$Total<-review$EIC.is.author+review$Associate.Editor.is.author+review$Editorial.Board.member.is.author

#Binary indicator for COI (EIC)
review$EIC_COI<-ifelse(review$EIC.is.author>0,1,0)
#Binary indicator for COI (AEs)
review$AE_COI<-ifelse(review$Associate.Editor.is.author>0,1,0)
#Binary indicator for COI (other)
review$other_COI<-ifelse(review$Editorial.Board.member.is.author>0,1,0)

table(review$any_COI, review$time)
table(review$Type)
table(review$any_COI, review$Type, review$time)

prop.table(table(review$any_COI,review$Type),2)
prop.table(table(review$EIC_COI,review$Type),2)
prop.table(table(review$AE_COI,review$Type),2)
prop.table(table(review$other_COI,review$Type),2)

unique<-unique(review$URL.of.Editorial.Board)
tj<-data.frame("journal"=unique,"ID"=1:length(unique))
review<-merge(review,tj,by.x="URL.of.Editorial.Board", by.y="journal")
review$obs<-1

tab_icc<-cbind(aggregate(review$any_COI, by=list(review$ID),sum, na.rm=T),
aggregate(review$obs, by=list(review$ID),sum, na.rm=T)[2])
names(tab_icc)<-c("journal","p","n")

ICCest(ID, any_COI, data = review)


  ###############################################
  # Focus on articles and editorials separately
  ###############################################
  

  articles<-review[review$Type=="Article",]
  #Proportion of articles with COIs
  
  tab<-as.data.frame(aggregate(list(articles$any_COI,articles$EIC_COI,
                                    articles$AE_COI,
                                    articles$other_COI), 
                                    by=list(articles$time),mean, na.rm=T))
  names(tab)<-c("COI","Any editorial COI","Editor-in-Chief","Associate Editors","Editors")
  table(articles$time, articles$any_COI)
  
  rev<-review[review$Type=="Review" | review$Type=="Editorial",]
  #Proportion of articles with COIs
  
  tab3<-as.data.frame(aggregate(list(rev$any_COI,rev$EIC_COI,
                                    rev$AE_COI,
                                    rev$other_COI), 
                               by=list(rev$time),mean, na.rm=T))
  names(tab3)<-c("COI","Any editorial COI","Editor-in-Chief","Associate Editors","Editors")
  table(rev$time)
  table(rev$time, rev$any_COI)
  
  
  
  #Creating a dataframe to plot the data easily
  tab_articles<-data.frame("Time"=rep(c("Same day","16 days", "20 days"),4),
                           "p"=as.vector(as.matrix(tab[,2:5])),
                           "COI"=rep(c("Any editorial COI","Editor-in-Chief","Associate Editors","Editors"),each=3))
  
  #Reordering the categories for the graph
  tab_articles$COI <- factor(tab_articles$COI,levels = c("Any editorial COI", "Editor-in-Chief", 
                                                         "Associate Editors", "Editors"))
 
  tab_articles$Time <- factor(tab_articles$Time,levels = c("Same day","16 days", "20 days"))
  
  #Creating a dataframe to plot the data easily
  tab_rev<-data.frame("Time"=rep(c("Same day","16 days", "20 days"),4),
                           "p"=as.vector(as.matrix(tab3[,2:5])),
                           "COI"=rep(c("Any editorial COI","Editor-in-Chief","Associate Editors","Editors"),each=3))
  
  #Reordering the categories for the graph
  tab_rev$COI <- factor(tab_rev$COI,levels = c("Any editorial COI", "Editor-in-Chief", 
                                                         "Associate Editors", "Editors"))
  
  tab_rev$Time <- factor(tab_articles$Time,levels = c("Same day","16 days", "20 days"))
  
 
  p1<-ggplot(data=tab_articles, aes(x=COI, y=p*100, fill=Time)) +
    geom_bar(stat="identity", color="black", position=position_dodge())+
    theme_minimal() +
    theme(
      plot.title=element_text(size=18,face="bold"),
      legend.title = element_text(color = "black", size = 16, face="bold"),
      legend.text = element_text(color = "black", size = 14),
      axis.text=element_text(size=14),
      axis.title=element_text(size=16,face="bold"),
      axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
      axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))
    )+
    labs(title="   Research articles", x = "Type of conflict of interest", y= "Percentage of manuscripts")+
    scale_fill_manual(values=c('#ADD8E6','#4682B4','#0000CD','#191970'),name = "Time to acceptance", 
                      labels = c("1 day or less (n=224)", "16 days (n=78)" , "20 days (n=102)"))+
    geom_text(aes(label =sprintf('%.1f',p*100)), 
              size = 5, 
              color = "black",
              position = position_dodge(width = 0.9),
              vjust = -1)+
    coord_cartesian( ylim = c(0, 100))
  

  
  p3<-ggplot(data=tab_rev, aes(x=COI, y=p*100, fill=Time)) +
    geom_bar(stat="identity", color="black", position=position_dodge())+
    theme_minimal() +
    theme(
      plot.title=element_text(size=18,face="bold"),
      legend.title = element_text(color = "black", size = 16, face="bold"),
      legend.text = element_text(color = "black", size = 14),
      axis.text=element_text(size=14),
      axis.title=element_text(size=16,face="bold"),
      axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
      axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))
    )+
    labs(title="   Reviews, editorials, viewpoints and letters", x = "Type of conflict of interest", y= "Percentage of manuscripts")+
    scale_fill_manual(values=c('#ADD8E6','#4682B4','#0000CD','#191970'),name = "Time to acceptance", 
                      labels = c("1 day or less (n=444)", "16 days (n=75)" , "20 days (n=71)"))+
    geom_text(aes(label =sprintf('%.1f',p*100)), 
              size = 5, 
              color = "black",
              position = position_dodge(width = 0.9),
              vjust = -1)+
    coord_cartesian( ylim = c(0, 100))
  
  
  pdf("COI_time_binary.pdf",  width = 12, # The width of the plot in inches
      height = 8)
  grid.arrange(p1,p3, nrow = 2)
  dev.off()
  
  table(articles$time)
  table(rev$time)
  table(editorials$time)
  