########################################
# Analysis of Altmetric scores
# Date:07/07/2020
# Author: Clémence Leyrat
# Input: altmetricArxivCOVID.tsv
#        altmetricRetractionWatch.tsv
#        altmetricMedBiorxiv.tsv
# Outputs: Altmetric_rxiv.tiff
#          Violin_plot_tot.tiff
#          Violin_plot_news.tiff
########################################

library(tidyverse)
library(ggplot2)
library(tableone)

#Change working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


###############################
# Retraction watch
###############################

  #Import
  retract<-read.csv("output/altmetricRetractionWatch.tsv", sep="\t", header=T)
  summary(retract)

  retract<-retract %>% 
  rename(doi = DOI)

  retract$source<-"RetractionWatch"
  
  t1<-retract[,-c(1,3)]
  
  
  
###############################
# Arxiv-COVID
###############################

  #Import
  arx_cov<-read.csv("output/altmetricArxivCOVID.tsv", sep="\t", header=T)
  summary(arx_cov)
  
  arx_cov$source<-"Arxiv_covid"
  t2<-arx_cov[,-c(1:11,13:14)]
  
  
###############################
# MedRxiv+Biorxiv-COVID
###############################

  #Import
  medrx_cov<-read.csv("output/altmetricMedBiorxiv.tsv", sep="\t", header=T, quote="", na.strings = c(NA,""))
  summary(medrx_cov)
  medrx_cov$TotalShares<-as.numeric(as.character(medrx_cov$TotalShares))
  medrx_cov$source<-ifelse(grepl('biorxiv',medrx_cov$link, fixed = TRUE), "Biorxiv", "MedRxiv")
  t3<-medrx_cov[!(is.na(medrx_cov$TotalShares)),-c(1,3:5)]
  summary(t3)

  #Combining the 3 datasets
  tab_scores<-rbind(t1,t2,t3) 
  summary(tab_scores)
  tab_scores$Score<-as.numeric(as.character(tab_scores$Score))

  write.csv(tab_scores,"tab_scores.csv",row.names=F,quote=F)


  #Creating a binary indicator for sharing
  tab_scores$shared<-ifelse(tab_scores$Score>-1,1,0)
  

  #Shares Gautret et al.
  tab_scores[tab_scores$PageURL=="http://www.altmetric.com/details.php?citation_id=77944909",]

  ##############################
  #Recoding
  ##############################
  
  #replacing -1 with 0
  names(tab_scores)
  tab_scores[,c(2,4:16)][tab_scores[,c(2,4:16)] == -1 ] <- 0
  tab_scores2<-tab_scores
  tab_scores2[,c(2,4:16)][tab_scores2[,c(2,4:16)] >1 ] <- 1
  
  #Combining categories for the analysis
  tab_scores2$OtherShares<-ifelse(tab_scores2$RedditShares+tab_scores2$WikipediaShares+tab_scores2$ResearchHighlightShares+
                                    tab_scores2$VideoShares>0,1,0)
  
  
  # Median number of total cites in the media by topic
  cites<-function(var,name){
    tmedian<-as.data.frame(cbind(c("ArXiv","BioRxiv","MedRXiv","RetractionWatch"),
                                 as.matrix(aggregate(var, by=list(tab_scores$source),quantile, na.rm=T)[2])))
    names(tmedian)<-c("Source","Min", "Q1", "Median", "Q3", "Max")
    tmedian$Type<-name
    return(tmedian)
  }
  
  tab_median<-rbind(cites(tab_scores$Score,"Score"),
                    cites(tab_scores$TotalShares,"Total"),
                    cites(tab_scores$TweeterShares,"Twitter"),
                    cites(tab_scores$FacebookShares,"Facebook"),
                    cites(tab_scores$RedditShares,"Reddit"),
                    cites(tab_scores$BlogShares,"Blog"),
                    cites(tab_scores$NewsShare,"News"),
                    cites(tab_scores$WikipediaShares,"Wikipedia"),
                    cites(tab_scores$ResearchHighlightShares,"ResearchHighlight"),
                    cites(tab_scores$VideoShares,"Video"),
                    cites(tab_scores$PoliciesShare,"Policies"),
                    cites(tab_scores$PeerReviewShares,"PeerReview"))
 
  write.csv(tab_median, "mediams.csv",row.names=F)
  
  tiff("Violin_plot_tot.tiff", res = 300,  width = 10, height = 6, units = 'in')
  ggplot(tab_scores[tab_scores$TotalShares>0,], aes(x=source, y=log(TotalShares))) +
    geom_violin(trim=F, fill=c('#ADD8E6'), color="black")+
    geom_boxplot(width=0.05,outlier.shape = NA) + theme_minimal()+
    theme(
      legend.title = element_text(color = "black", size = 16, face="bold"),
      legend.text = element_text(color = "black", size = 14),
      axis.text=element_text(size=14),
      axis.title=element_text(size=16,face="bold"),
      axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
      axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)) )+
    labs(x = "Research topic", y= "Number of total citations (log scale)")+
    scale_x_discrete(labels= c("ArXiv","BioRxiv","MedRXiv","RetractionWatch"))+
    coord_cartesian( ylim = c(0, 15))
  dev.off()
  
  tiff("Violin_plot_news.tiff", res = 300,  width = 10, height = 6, units = 'in')
  ggplot(tab_scores[tab_scores$NewsShare>0,], aes(x=source, y=log(NewsShare))) +
    geom_violin(trim=F, fill=c('#ADD8E6'), color="black")+
    geom_boxplot(width=0.05,outlier.shape = NA) + theme_minimal()+
    theme(
      legend.title = element_text(color = "black", size = 16, face="bold"),
      legend.text = element_text(color = "black", size = 14),
      axis.text=element_text(size=14),
      axis.title=element_text(size=16,face="bold"),
      axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
      axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)) )+
    labs(x = "Research topic", y= "Number of citations in the news medias (log scale)")+
    scale_x_discrete(labels= c("ArXiv","BioRxiv","MedRXiv","RetractionWatch"))+
    coord_cartesian( ylim = c(0, 15))
  dev.off()
  
  
names(tab_scores)
table1<-CreateContTable(vars=c("Score","TotalShares","TweeterShares","FacebookShares","RedditShares",
                               "BlogShares","NewsShare","WikipediaShares","ResearchHighlightShares",
                      "VideoShares","PoliciesShare","PinterestShares","WeiboShares",            
                      "PeerReviewShares"), strata="source", data=tab_scores,
                      funcNames = c("n", "miss", "median", "p25", "p75", "min","max"))
summary(table1)

table(tab_scores2$NewsShare, tab_scores2$source)
#Proportion of articles shared in each media
tab_prop<-as.data.frame(aggregate(list(tab_scores2$TotalShares,tab_scores2$TweeterShares,tab_scores2$OtherShares,
                                       tab_scores2$FacebookShares,tab_scores2$RedditShares,tab_scores2$BlogShares,tab_scores2$NewsShare,
                                       tab_scores2$WikipediaShares,tab_scores2$ResearchHighlightShares,
                                       tab_scores2$VideoShares,tab_scores2$PoliciesShare,tab_scores2$PeerReviewShares), 
                                  by=list(tab_scores2$source),mean, na.rm=T))
names(tab_prop)<-c("source","TotalShares","TweeterShares","OtherShares","FacebookShares","RedditShares",
                   "BlogShares","NewsShare","WikipediaShares","ResearchHighlightShares",
                   "VideoShares","PoliciesShare","PeerReviewShares")

#Creating a dataframe to plot the data easily
tab_analysis<-data.frame("source"=rep(c("ArXiv","BioRxiv","MedRXiv","RetractionWatch"),11),
                         "p"=as.vector(as.matrix(tab_prop[,2:12])),
                         "Type"=rep(c("Total","Twitter","Other","Facebook","Reddit",
                                      "Blogs","News","Wikipedia","ResearchHighlight",
                                      "Video","Policies"),each=4))

#Reordering the categories for the graph
tab_analysis$Type <- factor(tab_analysis$Type,levels = c("Twitter","Facebook","Blogs",
                                                         "News","Policies", "Other","Total",
                                                         "Reddit","Wikipedia","ResearchHighlight","Video"))

#Barplot comparing  papers by source
tiff("Altmetric_rxiv.tiff", res = 200,  width = 16, height = 8, units = 'in')
ggplot(data=tab_analysis[!(tab_analysis$Type %in% c("Total","Reddit","Wikipedia","ResearchHighlight",
                                                    "Video")),], aes(x=Type, y=p*100, fill=source)) +
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
  labs(x = "Type of media", y= "Percentage of preprints shared at least once")+
  scale_fill_manual(values=c('#ADD8E6','#87CEFA','#4682B4','#00008B'),name = "Source", labels = c("ArXiv","BioRxiv","MedRXiv","RetractionWatch"))+
  geom_text(aes(label =sprintf('%.1f',p*100)), 
            size = 5, 
            color = "black",
            position = position_dodge(width = 0.9),
            vjust = -1)+
  coord_cartesian( ylim = c(0, 100))
dev.off()
