rm(list = ls(all=TRUE))
ls()
getwd()


#Chargement des librairies 
library('ggplot2')
library('ggpubr')
library('grid')
library('dplyr')
library('lubridate')
library('tidyr')
library('timetk')
library('imputeTS')

path<-paste("G:/DOSSIERS RENAUD/anon-ms-example/French_version/MDS_Article/",sep="")
source("G:/DOSSIERS RENAUD/anon-ms-example/French_version/MDS_Article/Codes/statsNA.R")

Sites<-c("Nalohou")

Data_Nal<-read.table(file=paste(path,"Tables/NA_stat/",Sites[1],"_Var_NA_stat_","Metric.csv",sep = ""),sep=";", header = TRUE, na.strings = "NA", skip=0, dec=".",strip.white = TRUE)


theme_bluewhite<- function (base_size = 11, base_family = "") {
  theme_bw() %+replace% 
    theme(
      panel.grid.major  = element_line(color = "white"),
      panel.background = element_rect(fill = "white"),
      panel.border = element_rect(color = "white", fill = NA),
      axis.line = element_line(color = "grey"),
      axis.ticks = element_line(color = "black"),
      axis.text = element_text(family = "serif",color = "black",size = 12,face = "bold"),
      axis.title = element_text(family = "serif",color = "black",size = 12,face = "bold"),
      legend.title = element_blank(),
        # element_text(family = "serif",color = "black",size = 15,face = "bold"),
      plot.title = element_text(family = "serif",color = "black",size = 15,face = "bold"),
      strip.text = element_text(family = "serif",color = "black",size = 11,face = "bold"),
      legend.text = element_text(family = "serif",color = "black",size = 11,face = "bold")
    )
}




Q2<-ggplot(data=Data_Nal,mapping=aes(x=Year,y=Percentage,fill=Variable))+
  ylim(0,100)+
  geom_bar(stat = "identity",position = "dodge")+
  scale_x_continuous(breaks = seq(from=2006,to=2018,by=1))+
  scale_fill_manual(values=c("#0000FF","#FFFF00","#FF00FF","#808000","#008080"))+
  # scale_y_continuous(breaks = seq(from=0,to=100,by=10))+
  ggtitle(Sites[2])+theme_bluewhite()


Q_2
# ggsave(paste(path,"Figure_2.svg",sep=""),plot=Q2,width = 14,height = 12)

