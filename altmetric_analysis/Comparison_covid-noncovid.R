########################################
# Analysis of Altmetric scores
# Date:07/07/2020
# Author: Clémence Leyrat
# Input: altmetricArxivCOVID.tsv
#        altmetricsArxivNonCOVID.tsv
# Outputs: Violin_plot.tiff
#          Altmetric_comp.tiff
########################################

library(tidyverse)
library(ggplot2)
library(tableone)
library(expss)
library(reporttools)

#Change working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


##############################
# Arxiv-COVID
###############################

#Import
  arx_cov<-read.csv("output/altmetricArxivCOVID.tsv", sep="\t", header=T)
  arx_cov$source<-"COVID"
  summary(arx_cov)
  
  arx_noncov<-read.csv("output/altmetricsArxivNonCOVID.tsv", sep="\t", header=T, quote = "")
  arx_noncov$source<-"non-COVID"
  summary(arx_noncov)
  
  #Combine the two datasets
  arx<-rbind(arx_cov,arx_noncov)

  
##########################
# Recoding
##########################

  #Creating a binary indicator for sharing
  arx$shared<-ifelse(arx$Score>-1,1,0)
  
  #replacing -1 with 0
  names(arx)
  arx[,c(15,17:29)][arx[,c(15,17:29)] == -1 ] <- 0
  
  #New dataset with only binary variables (e.g shared on Twitter, y/n)
  arx2<-arx
  arx2[,c(15,17:29)][arx2[,c(15,17:29)] >1 ] <- 1

  #Combining categories for the analysis
  arx2$OtherShares<-ifelse(arx2$RedditShares+arx2$WikipediaShares+arx2$ResearchHighlightShares+
                             arx2$VideoShares>0,1,0)
  
  
##########################
# Analysis
##########################

  # Median number of total cites in the media by topic
  cites<-function(var,name){
    tmedian<-as.data.frame(cbind(c("COVID","Non-COVID"),
                                 as.matrix(aggregate(var, by=list(arx$source),quantile, na.rm=T)[2])))
    names(tmedian)<-c("Source","Min", "Q1", "Median", "Q3", "Max")
    tmedian$Type<-name
    return(tmedian)
  }
  
  tab_median<-rbind(cites(arx$Score,"Score"),
                    cites(arx$TotalShares,"Total"),
                    cites(arx$TweeterShares,"Twitter"),
                    cites(arx$FacebookShares,"Facebook"),
                    cites(arx$RedditShares,"Reddit"),
                    cites(arx$BlogShares,"Blog"),
                    cites(arx$NewsShare,"News"),
                    cites(arx$WikipediaShares,"Wikipedia"),
                    cites(arx$ResearchHighlightShares,"ResearchHighlight"),
                    cites(arx$VideoShares,"Video"),
                    cites(arx$PoliciesShare,"Policies"),
                    cites(arx$PeerReviewShares,"PeerReview"))
  
 #write.csv(cbind(tab_median[tab_median$Source=="COVID",],tab_median[tab_median$Source=="Non-COVID",]),
 #          "stats.csv",row.names=F)
  
  arx %>% 
    tab_cells(Score, TotalShares) %>%
    tab_cols(total(label = "#Total| |"), source) %>% 
    tab_stat_fun('Median' = median, 'Min' = min, 'Valid N' = w_n, method = list) %>%
    tab_pivot()
  

  
  
  summary(arx$TotalShares)
  tiff("Violin_plot.tiff", res = 300,  width = 10, height = 6, units = 'in')
  ggplot(arx[arx$TotalShares>0,], aes(x=source, y=log(TotalShares))) +
    geom_violin(trim=T, fill=c('#ADD8E6'), color="black")+
    geom_boxplot(width=0.05,outlier.shape = NA) + theme_minimal()+
    theme(
      legend.title = element_text(color = "black", size = 16, face="bold"),
      legend.text = element_text(color = "black", size = 14),
      axis.text=element_text(size=14),
      axis.title=element_text(size=16,face="bold"),
      axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
      axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)) )+
    labs(x = "Research topic", y= "Number of citations (log scale)")+
    scale_x_discrete(labels= c("COVID-19", "Other"))
    dev.off()

    summary(arx$NewsShare)
    #Violin plot for the number of citations in the news
    ggplot(arx[arx$NewsShare>0,], aes(x=source, y=log(NewsShare))) +
      geom_violin(trim=T, fill=c('#ADD8E6'), color="black")+
      geom_boxplot(width=0.05,outlier.shape = NA) + theme_minimal()+
      theme(
        legend.title = element_text(color = "black", size = 16, face="bold"),
        legend.text = element_text(color = "black", size = 14),
        axis.text=element_text(size=14),
        axis.title=element_text(size=16,face="bold"),
        axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)),
        axis.title.x = element_text(margin = margin(t = 20, r = 0, b = 0, l = 0)))+
      labs(x = "Research topic", y= "Number of citations (log scale)")+
      scale_x_discrete(labels= c("COVID-19", "Other"))
    
  
  #Proportion of papers shared at least once in any type of media
  prop.table(table(arx$source,arx$shared),1)
  names(arx)


  
  #Proportion of articles shared in each media
  table(arx2$source,arx2$NewsShare)
  tab_prop<-as.data.frame(aggregate(list(arx2$TotalShares,arx2$TweeterShares,arx2$OtherShares,
                                         arx2$FacebookShares,arx2$RedditShares,arx2$BlogShares,arx2$NewsShare,
                                         arx2$WikipediaShares,arx2$ResearchHighlightShares,
                                         arx2$VideoShares,arx2$PoliciesShare,arx2$PeerReviewShares), 
                                        by=list(arx2$source),mean, na.rm=T))
  names(tab_prop)<-c("source","TotalShares","TweeterShares","OtherShares","FacebookShares","RedditShares",
                     "BlogShares","NewsShare","WikipediaShares","ResearchHighlightShares",
                     "VideoShares","PoliciesShare","PeerReviewShares")

  #Creating a dataframe to plot the data easily
  tab_analysis<-data.frame("source"=rep(c("COVID","Non-COVID"),11),
                           "p"=as.vector(as.matrix(tab_prop[,2:12])),
                           "Type"=rep(c("Total","Twitter","Other","Facebook","Reddit",
                                        "Blogs","News","Wikipedia","ResearchHighlight",
                                        "Video","Policies"),each=2))

  #Reordering the categories for the graph
  tab_analysis$Type <- factor(tab_analysis$Type,levels = c("Twitter","Facebook","Blogs",
                                         "News","Policies", "Other","Total",
                                         "Reddit","Wikipedia","ResearchHighlight","Video"))

  #Barplot comparing COVID/Non-COVID papers
  tiff("Altmetric_comp.tiff", res = 300,  width = 10, height = 6, units = 'in')
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
    scale_fill_manual(values=c('#ADD8E6','#4682B4'),name = "Research topic", labels = c("COVID-19 (n=1,462)", "Other (n=80,786)"))+
    geom_text(aes(label =sprintf('%.1f',p*100)), 
               size = 5, 
               color = "black",
               position = position_dodge(width = 0.9),
               vjust = -1)+
     coord_cartesian( ylim = c(0, 100))
  dev.off()




