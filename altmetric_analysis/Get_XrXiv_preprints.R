########################################
    # Get COVID preprints from Arxiv
    # Date:07/07/2020
    # Author: Clémence Leyrat
    # Input: none
    # Outputs: preprints_arxiv_COVID.csv
    # preprints_arxiv_nonCOVID.csv
    #  preprints_medbiorxiv.csv
########################################

#Change working directory (current folder if using RStudio)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#load packages
  library(aRxiv) #Used to get info from Arxiv
  library(XML) #Dependency
  library(rjson) #Convert json to list
  library(stringdist) #To compute the edit distance


 #########################################################################
 # Note: we looked at papers submitted between 01/01/2020 and 30/06/2020
 #########################################################################


##########
# ArXiv
##########

#1-COVID papers

#First check the number of COVID records with this query:
  arxiv_count(query = ('submittedDate:[20200101 TO 20200630] AND (ti:"COVID" OR abs:"COVID" OR abs:"SARS-CoV-2" OR ti:"SARS-CoV-2" OR ti:"coronavirus" OR abs:"coronavirus")'))
  #n=1462

#Now extracting complete info for these preprints
  tab_arxiv_cov<-arxiv_search(query = ('submittedDate:[20200101 TO 20200630] AND (ti:"COVID" OR abs:"COVID" OR abs:"SARS-CoV-2" OR ti:"SARS-CoV-2" OR ti:"coronavirus" OR abs:"coronavirus")'),
             output_format = "data.frame", sep = "|", limit=2000, batchsize=1000, force=T)

#Save it in .csv (removing the abstracts to avoid problems with the export)
  write.csv(tab_arxiv_cov[,-5],"input/preprints_arxiv_COVID.csv", quote=T, row.names=F)
  
  
#2-Non-COVID papers 
  #First check the number of non COVID records in the same time period
  arxiv_count(query = ('submittedDate:[20200101 TO 20200630] AND NOT (ti:"COVID" OR abs:"COVID" OR abs:"SARS-CoV-2" OR ti:"SARS-CoV-2" OR ti:"coronavirus" OR abs:"coronavirus")'))
  #n=83819

  #Now extracting complete info for these preprints (by month)
  #Note: there is a problem with papers submitted on 13th April
  tab_jan_noncov<-arxiv_search(query = ('submittedDate:[20200101 TO 20200131] AND NOT (ti:"COVID" OR abs:"COVID" OR abs:"SARS-CoV-2" OR ti:"SARS-CoV-2" OR ti:"coronavirus" OR abs:"coronavirus")'),
                          output_format = "data.frame", sep = "|", limit=20000, batchsize=800, force=T)
  tab_feb_noncov<-arxiv_search(query = ('submittedDate:[20200201 TO 20200229] AND NOT (ti:"COVID" OR abs:"COVID" OR abs:"SARS-CoV-2" OR ti:"SARS-CoV-2" OR ti:"coronavirus" OR abs:"coronavirus")'),
                               output_format = "data.frame", sep = "|", limit=50000, batchsize=1000, force=T)
  tab_mar_noncov<-arxiv_search(query = ('submittedDate:[20200301 TO 20200331] AND NOT (ti:"COVID" OR abs:"COVID" OR abs:"SARS-CoV-2" OR ti:"SARS-CoV-2" OR ti:"coronavirus" OR abs:"coronavirus")'),
                               output_format = "data.frame", sep = "|", limit=20000, batchsize=2000, force=T)
  tab_apr_noncov<-arxiv_search(query = ('submittedDate:[20200401 TO 20200412] AND NOT (ti:"COVID" OR abs:"COVID" OR abs:"SARS-CoV-2" OR ti:"SARS-CoV-2" OR ti:"coronavirus" OR abs:"coronavirus")'),
                               output_format = "data.frame", sep = "|", limit=20000, batchsize=2000, force=T)
  tab_apr2_noncov<-arxiv_search(query = ('submittedDate:[20200414 TO 20200430] AND NOT (ti:"COVID" OR abs:"COVID" OR abs:"SARS-CoV-2" OR ti:"SARS-CoV-2" OR ti:"coronavirus" OR abs:"coronavirus")'),
                               output_format = "data.frame", sep = "|", limit=10000, batchsize=2000, force=T)
  tab_may_noncov<-arxiv_search(query = ('submittedDate:[20200501 TO 20200531] AND NOT (ti:"COVID" OR abs:"COVID" OR abs:"SARS-CoV-2" OR ti:"SARS-CoV-2" OR ti:"coronavirus" OR abs:"coronavirus")'),
                               output_format = "data.frame", sep = "|", limit=50000, batchsize=1000, force=T)
  tab_jun_noncov<-arxiv_search(query = ('submittedDate:[20200601 TO 20200630] AND NOT (ti:"COVID" OR abs:"COVID" OR abs:"SARS-CoV-2" OR ti:"SARS-CoV-2" OR ti:"coronavirus" OR abs:"coronavirus")'),
                               output_format = "data.frame", sep = "|", limit=50000, batchsize=1000, force=T)

  tab_arxiv_noncov<-rbind(tab_jan_noncov,tab_feb_noncov,tab_mar_noncov,tab_apr_noncov,tab_apr2_noncov,tab_may_noncov,tab_jun_noncov)
  
  #Save it in .csv (removing the abstracts to avoid problems with the export)
  write.csv(tab_arxiv_noncov[,-5],"input/preprints_arxiv_nonCOVID.csv", quote=T, row.names=F)
  
  
#######################
# BioRxiv and MedRxiv
#######################
  
  #Access the covid database here:https://connect.medrxiv.org/relate/content/181
  base<-fromJSON(file="https://connect.medrxiv.org/relate/collection_json.php?grp=181", method = "C", unexpected.escape = "error", simplify = TRUE )
  
  #Loop to extract the relevant information
  b<-base[[3]]
  res<-NULL
  for (i in 1:length(b)){
    print(i)
    if(b[[i]]$rel_num_authors>=1 &){
    resi<-t(c(b[[i]]$rel_title, b[[i]]$rel_doi, b[[i]]$rel_link, b[[i]]$rel_authors[[1]]$author_name, b[[i]]$rel_date, b[[i]]$rel_site))
    }
    if(b[[i]]$rel_num_authors==0){
      resi<-t(c(b[[i]]$rel_title, b[[i]]$rel_doi, b[[i]]$rel_link, NA, b[[i]]$rel_date, b[[i]]$rel_site))
    }
    res<-rbind(res,resi)
  }
  
  #Convert to dataframe and name the variables
  tab_med_biorxiv<-as.data.frame(res)
  names(tab_med_biorxiv)<-c("title","doi","link","first_au","datesubmitted","site")
  
  #Extract the date and select papers published between 01/01/2020 and 30/06/2020
  tab_med_biorxiv$year<-as.numeric(as.character(matrix(unlist(strsplit(tab_med_biorxiv$datesubmitted, "-")),ncol=3,byrow=T)[,1]))
  tab_med_biorxiv$month<-as.numeric(as.character(matrix(unlist(strsplit(tab_med_biorxiv$datesubmitted, "-")),ncol=3,byrow=T)[,2]))
  tab_med_biorxiv$day<-as.numeric(as.character(matrix(unlist(strsplit(tab_med_biorxiv$datesubmitted, "-")),ncol=3,byrow=T)[,3]))

  tab_med_biorxiv<-tab_med_biorxiv[tab_med_biorxiv$year==2020 & tab_med_biorxiv$month<7,]

  #Save the file
  write.csv(tab_med_biorxiv[,-c(6:9)],"input/preprints_medbiorxiv_dup.csv", quote=T, row.names=F)
  
  

##########################
 #Check for duplicates
##########################
  

  #Check for duplicates within the bio+medrxiv
  a<-which(duplicated(tab_med_biorxiv$title))
  tit<-tab_med_biorxiv$title[a]
  d<-tab_med_biorxiv[tab_med_biorxiv$title %in% tit,]
  #Linton is a mistake (wrong title in the data extraction process so we keep it)
  
  #Exclusion of the two duplicates
  tab_med_biorxiv<-tab_med_biorxiv[-a[1:2],]
  
  #Comparing bio+medrxiv to arxiv
  duplicate<-merge(tab_arxiv,tab_med_biorxiv, by="title")
  doi_dup<-duplicate$doi.y
  
  #remove the duplicates from tab_med_biorxiv
  tab_med_biorxiv2<-tab_med_biorxiv[-which(tab_med_biorxiv$doi %in% doi_dup),]
  

  #Calculate the edit distance between the titles in the two dataframes
  mat <- stringdistmatrix(tab_arxiv$title, tab_med_biorxiv2$title, method = "lcs")
  
  #For each title in arxiv, pick the closest in bioarxiv 
  min_dist<-NULL
  title_arxiv<-NA
  title_biorxiv<-NA
  for (i in 1:nrow(tab_arxiv)){
    v<-mat[i,]
    min_dist<-c(min_dist,min(v))
    coord<-which(v == min(v), arr.ind = TRUE)[1]
    title_arxiv[i]<-tab_arxiv$title[i]
    title_biorxiv[i]<-tab_med_biorxiv2$title[coord]
  }
  
  #Create a new dataframe with the "closest" title and its distance
  tab_dist<-data.frame("title_arxiv"=title_arxiv,"title_biorxiv"=title_biorxiv,"dist"=min_dist)

  #We can consider duplicates title with an edit distance<15 (empirical threshold)
  duplicate2<-tab_dist[tab_dist$dist<15,]

  
  #Remove the duplicates from tab_med_biorxiv
  title_dup<-duplicate2$title_biorxiv
  tab_med_biorxiv3<-tab_med_biorxiv2[-which(tab_med_biorxiv2$title %in% title_dup),]
  
  #Save it in .csv
  write.csv(tab_med_biorxiv3[,-c(6:9)],"input/preprints_medbiorxiv_covid.csv", quote=T, row.names=F)

    
