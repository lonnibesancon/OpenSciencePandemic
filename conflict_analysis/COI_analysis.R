########################################
# Analysis of conflicts of interests
# Date:07/07/2020
# Author: Clémence Leyrat
# Input: 0 day list - ReviewTime_0day.csv
#        1 day list - ReviewTime_1day.csv
# Outputs: COIs.tiff
#          COI.csv
########################################

library(tidyverse)
library(ggplot2)
library(tableone)
library(aod)
library(ICC)
library(sqldf)

#Change working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##############################
# Arxiv-COVID
###############################

#Import
t0days<-read.csv("0 day list - ReviewTime_0day.csv", sep=",", header=T)
t0days$time<-0
summary(t0days)

t1days<-read.csv("1 day list - ReviewTime_1day.csv", sep=",", header=T)
t1days$time<-1
summary(t1days)

#Combine the two datasets
t1days<-t1days %>% 
  rename(Comment = Comment..paper.type.)

review<-rbind(t0days,t1days)

#Exclusion of records with missing data (because they were duplicates)
review<-review[is.na(review$EIC.is.author)==F,]
summary(review)

table(review$time)


########################
#Recoding
########################

#Grouping the type of papers
substr_article="rticle|eview|Case|eport|Guideline|riginal|ecommendation|Research|Consensus|CONSENSUS|ractice|trial|How|Core"
substr_short="Brief|Short|Rapid|Commentary|Response|ommunication|tatement|Correspondence|Clarification|Discussion"
substr_edito="ditor|Letter|Opinion|Viewpoint|Perspective|Point of view"
review$Type<-ifelse(grepl(substr_article,review$Comment),"Article", "")
review$Type<-ifelse(grepl(substr_short,review$Comment),"Short paper", review$Type)
review$Type<-ifelse(grepl(substr_edito,review$Comment),"Editorials", review$Type)


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

table(review$any_COI)
table(review$Type)
table(review$any_COI, review$Type)

prop.table(table(review$any_COI,review$Type),2)
prop.table(table(review$EIC_COI,review$Type),2)
prop.table(table(review$AE_COI,review$Type),2)
prop.table(table(review$other_COI,review$Type),2)

unique<-unique(review$URL.of.Editorial.Board)
tj<-data.frame("journal"=unique,"ID"=1:length(unique))
review<-merge(review,tj,by.x="URL.of.Editorial.Board", by.y="journal")
review$obs<-1

#Estimation of the ICC
tab_icc<-cbind(aggregate(review$any_COI, by=list(review$ID),sum, na.rm=T),
aggregate(review$obs, by=list(review$ID),sum, na.rm=T)[2])
names(tab_icc)<-c("journal","p","n")

iccbin(n, p, data = tab_icc, method = "A")
ICCest(ID, any_COI, data = review)


#Proportion of COIs
tab_prop<-as.data.frame(aggregate(list(review$any_COI,review$EIC_COI,review$AE_COI,
                                       review$other_COI), 
                                  by=list(review$Type),mean, na.rm=T))
names(tab_prop)<-c("COI","Any COI","Editor-in-Chief","Associate Editors","Editors")

#Creating a dataframe to plot the data easily
tab_analysis<-data.frame("Type"=rep(c("Articles","Editorials","Short papers"),4),
                         "p"=as.vector(as.matrix(tab_prop[,2:5])),
                         "COI"=rep(c("Any COI","Editor-in-Chief","Associate Editors","Editors"),each=3))

#Reordering the categories for the graph
tab_analysis$COI <- factor(tab_analysis$COI,levels = c("Any COI", "Editor-in-Chief", 
                                                         "Associate Editors", "Editors"))
tab_analysis$Type <- factor(tab_analysis$Type,levels = c("Articles", "Short papers", 
                                                       "Editorials"))

#Barplot comparing type of CoIs by type of paper
tiff("COIs.tiff", res = 300,  width = 12, height = 6, units = 'in')
ggplot(data=tab_analysis, aes(x=COI, y=p*100, fill=Type)) +
  geom_bar(stat="identity", color="black", position=position_dodge())+
  theme_minimal() +
  theme(
    legend.title = element_text(color = "black", size = 16, face="bold"),
    legend.text = element_text(color = "black", size = 14),
    axis.text=element_text(size=14),
    axis.title=element_text(size=16,face="bold"),
    axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
    axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0))
  )+
  labs(x = "Type of conflict of interest", y= "Percentage of manuscripts")+
  scale_fill_manual(values=c('#ADD8E6','#4682B4','#00008B'),name = "Type of manuscript", 
                    labels = c("Article (n=503)", "Short paper (n=74)","Editorial (n=123)"))+
  geom_text(aes(label =sprintf('%.1f',p*100)), 
            size = 5, 
            color = "black",
            position = position_dodge(width = 0.9),
            vjust = -1)+
  coord_cartesian( ylim = c(0, 100))
dev.off()


#############################################
# Focus on journals with at least 3 papers 
#############################################

  tab_article<-review[review$Type=="Article",]
  tab_journal<-sqldf("select  ID, count(*) as m, sum(any_COI) as y from tab_article group by ID")
  tab_journal3<-tab_journal[tab_journal$m>=3,]
  tab_journal2<-tab_journal[tab_journal$m>=2,]
  

  tab_journal2$p<-tab_journal2$y/tab_journal2$m
  table(tab_journal2$p)
  tab_journal2$cat<-0
  tab_journal2$cat[tab_journal2$p>0 & tab_journal2$p<=0.5]<-1
  tab_journal2$cat[tab_journal2$p>0.5]<-2
  
  t2<-data.frame(table(tab_journal2$cat))
  vect_paper<-tab_journal2$ID[tab_journal2$cat==2]


  tab_badCOIs<-review[review$ID %in% vect_paper,]
  write.csv(unique(tab_badCOIs$URL.of.Editorial.Board), "COI.csv")