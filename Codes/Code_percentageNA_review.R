rm(list = ls(all=TRUE))
ls()
getwd()


#Chargement des librairies 

library('grid')
library('dplyr')
library('lubridate')
library('tidyr')
library('timetk')
library('imputeTS')


source("G:/DOSSIERS RENAUD/anon-ms-example/French_version/MDS_Article/Codes/statsNA.R")



Imput <- "G:/DOSSIERS RENAUD/anon-ms-example/French_version/MDS_Article/Tables/Data_full/"
setwd(Imput)
fichiers<- list.files (Imput)
Data<-NULL

Sites<-c("Nalohou")
SitesID<-c("Nal")

n=1
for (file in fichiers){
  cat("______________INITIALISATION_____________\n")
  Data<-NULL
  print(file)
  tmp = readLines(file)
  tmpFile = tempfile()
  on.exit(unlink(tmpFile))
  writeLines(tmp,tmpFile)
  
  file = tmpFile
  Data=read.csv(file,sep=";", header = TRUE, dec=".",strip.white = TRUE)

  Year<-year(ymd_hms(Data$Date))
  Data<-cbind(Year,Data)
  cat("______________Cr?ation des vecteur ann?e_____________\n")
  if((n==1)||(n==2)){
    Year_vect<-seq(from=2006,to=2018,by=1)
  }
   year_num=1
  
  path<-paste("G:/DOSSIERS RENAUD/anon-ms-example/French_version/MDS_Article/",sep="")
  nrow<-ncol(Data[,5:length(Data)])
  Final=NULL
  cat("______________Stat par ann?e_____________\n")
  for (i in 1:length(Year_vect)) {
    Year_stat<-filter(Data,Year==Year_vect[i])
    X<-colnames(Year_stat)
    Df <- data.frame(
      Variable = character(nrow), Year = numeric(nrow),Percentage=numeric(nrow)
    )
    Df_row=1
    for (j in 5:length(Data)) {
      List_stat<-statsNA(Year_stat[,j],bins = 12,print_only = FALSE)
      Df$Variable[Df_row]=X[j]
      Df$Year[Df_row]=Year_vect[i]
      Df$Percentage[Df_row]=round(List_stat$percentage_NAs[[1]],2)
      Df_row=Df_row+1
      
      
    }
    
    if(is.null(Final)){
      Final<-Df
    }
    else{
      Final<-rbind(Final,Df)
    }
  }
  cat("______________Sauvegade de stat_____________\n")
  write.table(Final,file=paste("G:/MDS_Article/Tables/Nalohou","_Var_NA_stat_","Metric.csv",sep = ""),row.names = T,sep = ";")
  }
