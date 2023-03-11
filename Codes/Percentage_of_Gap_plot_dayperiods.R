rm(list = ls(all=TRUE))
ls()
getwd()


#Chargement des librairies 

library('grid')
library('dplyr')
library('lubridate')
library('tidyr')
library('timetk')
library('ggplot2')
library('ggpubr')


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



path<-paste("G:/DOSSIERS RENAUD/anon-ms-example/Station_Base/")

#Choix du sites
source("G:/DOSSIERS RENAUD/anon-ms-example/Station_Base/day_and_nigth2_demis.R")

Sites<-c("Nalohou")
SitesID<-c("Nal")


n<- 1
path_out<-paste(path,Sites[n],"/Table/NA_stat/",sep = "")


Data=read.csv(paste("G:/DOSSIERS RENAUD/anon-ms-example/Station_Base/",Sites[n],"/Data_full.csv",sep=""),sep=";", header = TRUE, na.strings = "NA", skip=0, dec=".",strip.white = TRUE)
Data <- Data[,-1]
Data[,"DateUTC"]<- ymd_hms(Data$Date)


  Data[,"Day_or_nigth_1.2"]<-day_and_nigth(1.60457,9.74484,Data)
  
  




Data$Year <-as.character(year(Data$DateUTC))  
Data$Month <- month(Data$DateUTC)
Data$Day <- day(Data$DateUTC)

Variable <- c('Swin','Tair','Tsol_1_10','HR','VPD')


for (i in 1:length(Variable)) {
  Flag <- c()
  for (j in 1:nrow(Data)) {
    if(!is.na(Data[j,Variable[i]])){
      Flag[j] <- "0"  
    }
    if(is.na(Data[j,Variable[i]])){
      if(Data$Day_or_nigth_1.2[j]==0){
        Flag[j] <- "Night"
      }
      if(Data$Day_or_nigth_1.2[j]==1){
        Flag[j] <- "Before_Noon"
      }
      if(Data$Day_or_nigth_1.2[j]==2){
        Flag[j] <- "After_Noon"
      }
      if(Data$Day_or_nigth_1.2[j]==3){
        Flag[j] <- "After_Noon"
      }
    }
    
    
  }
  Data[,paste('Flag',Variable[i],sep = "_")] <- Flag
}


Indice<-which(is.na(Data$Flag_Swin)==TRUE )

count_by_group <- function(data, var1, var2) {
  require(dplyr)
  result <- data %>%
    group_by(var2,var1) %>%
    summarise(n=n())
  return(result)
}

elements_non_communs <- function(A, B) {
  non_communs <- B[!B %in% A]
  return(non_communs)
}

tri_tableau <- function(tableau, col1, col2) {
  tableau_trie <- tableau[order(tableau[, col1], tableau[, col2]), ]
  return(tableau_trie)
}

Data_flagswin <- filter(subset(Data,select = c("Year","Flag_Swin")),Flag_Swin==c("Night","Before_Noon","After_Noon"))
Data_flagtair <- filter(subset(Data,select = c("Year","Flag_Tair")),Flag_Tair==c("Night","Before_Noon","After_Noon"))
Data_flagtsol <- filter(subset(Data,select = c("Year","Flag_Tsol_1_10")),Flag_Tsol_1_10==c("Night","Before_Noon","After_Noon"))
Data_flaghr <- filter(subset(Data,select = c("Year","Flag_HR")),Flag_HR==c("Night","Before_Noon","After_Noon"))
Data_flagvpd <- filter(subset(Data,select = c("Year","Flag_VPD")),Flag_VPD==c("Night","Before_Noon","After_Noon"))

####Regroupage par année
result_swin <- Data_flagswin %>%
  group_by(Year) %>%
  count(Flag_Swin) %>% 
  mutate(percent = round(n/sum(n)*100,2) )

A <- unique(result_swin$Year)
B <- unique(Data$Year)

No_NA_year <-elements_non_communs(A,B) 

nb <- length(No_NA_year)

m <- length(rep(c("Night","Before_Noon","After_Noon"),nb))
result_no_na <- data.frame(Year=rep(No_NA_year,each=3),Flag_Swin=rep(c("Night","Before_Noon","After_Noon"),nb),n=rep(0,m),percent=rep(0,m))

result_swin <- rbind(result_swin,result_no_na)

result_swin$Year <- as.numeric(result_swin$Year)

result_swin<- tri_tableau(result_swin,1,2)

result_swin$Year <- as.character(result_swin$Year)



result_Tair <- Data_flagtair %>%
  group_by(Year) %>%
  count(Flag_Tair)%>% 
  mutate(percent = round(n/sum(n)*100,2) )

A <- unique(result_Tair$Year)
B <- unique(Data$Year)

No_NA_year <-elements_non_communs(A,B) 

nb <- length(No_NA_year)

m <- length(rep(c("Night","Before_Noon","After_Noon"),nb))
result_no_na <- data.frame(Year=rep(No_NA_year,each=3),Flag_Tair=rep(c("Night","Before_Noon","After_Noon"),nb),n=rep(0,m),percent=rep(0,m))

result_Tair <- rbind(result_Tair,result_no_na)

result_Tair$Year <- as.numeric(result_Tair$Year)
# result_Tair$Flag_Tair <- as.numeric(result_Tair$Flag_Tair)

result_Tair<- tri_tableau(result_Tair,1,2)

result_Tair$Year <- as.character(result_Tair$Year)
# result_Tair$Flag_Tair <- as.character(result_Tair$Flag_Tair)


result_Tsol_1_10 <- Data_flagtsol %>%
  group_by(Year) %>%
  count(Flag_Tsol_1_10)%>% 
  mutate(percent = round(n/sum(n)*100,2) )

A <- unique(result_Tsol_1_10$Year)
B <- unique(Data$Year)

No_NA_year <-elements_non_communs(A,B) 

nb <- length(No_NA_year)

m <- length(rep(c("Night","Before_Noon","After_Noon"),nb))
result_no_na <- data.frame(Year=rep(No_NA_year,each=3),Flag_Tsol_1_10=rep(c("Night","Before_Noon","After_Noon"),nb),n=rep(0,m),percent=rep(0,m))

result_Tsol_1_10 <- rbind(result_Tsol_1_10,result_no_na)

result_Tsol_1_10$Year <- as.numeric(result_Tsol_1_10$Year)
# result_Tsol_1_10$Flag_Tsol_1_10 <- as.numeric(result_Tsol_1_10$Flag_Tsol_1_10)

result_Tsol_1_10<- tri_tableau(result_Tsol_1_10,1,2)

result_Tsol_1_10$Year <- as.character(result_Tsol_1_10$Year)
# result_Tsol_1_10$Flag_Tsol_1_10 <- as.character(result_Tsol_1_10$Flag_Tsol_1_10)




result_HR <- Data_flaghr %>%
  group_by(Year) %>%
  count(Flag_HR)%>% 
  mutate(percent = round(n/sum(n)*100,2) )

A <- unique(result_HR$Year)
B <- unique(Data$Year)

No_NA_year <-elements_non_communs(A,B) 

nb <- length(No_NA_year)

m <- length(rep(c("Night","Before_Noon","After_Noon"),nb))
result_no_na <- data.frame(Year=rep(No_NA_year,each=3),Flag_HR=rep(c("Night","Before_Noon","After_Noon"),nb),n=rep(0,m),percent=rep(0,m))

result_HR <- rbind(result_HR,result_no_na)

result_HR$Year <- as.numeric(result_HR$Year)
# result_HR$Flag_HR <- as.numeric(result_HR$Flag_HR)

result_HR<- tri_tableau(result_HR,1,2)

result_HR$Year <- as.character(result_HR$Year)
# result_HR$Flag_HR <- as.character(result_HR$Flag_HR)







result_VPD <- Data_flagvpd %>%
  group_by(Year) %>%
  count(Flag_VPD)%>% 
  mutate(percent = round(n/sum(n)*100,2) )

A <- unique(result_VPD$Year)
B <- unique(Data$Year)

No_NA_year <-elements_non_communs(A,B) 

nb <- length(No_NA_year)

m <- length(rep(c("Night","Before_Noon","After_Noon"),nb))
result_no_na <- data.frame(Year=rep(No_NA_year,each=3),Flag_VPD=rep(c("Night","Before_Noon","After_Noon"),nb),n=rep(0,m),percent=rep(0,m))

result_VPD <- rbind(result_VPD,result_no_na)

result_VPD$Year <- as.numeric(result_VPD$Year)
# result_VPD$Flag_VPD <- as.numeric(result_VPD$Flag_VPD)

result_VPD<- tri_tableau(result_VPD,1,2)

result_VPD$Year <- as.character(result_VPD$Year)
# result_VPD$Flag_VPD <- as.character(result_VPD$Flag_VPD)



write.table(result_swin,file=paste(path_out,Sites[n],"_gap_stat_Swin_by_year",".csv",sep = ""),row.names = FALSE,sep = ";")
write.table(result_Tair,file=paste(path_out,Sites[n],"_gap_stat_Tair_by_year",".csv",sep = ""),row.names = FALSE,sep = ";")
write.table(result_Tsol_1_10,file=paste(path_out,Sites[n],"_gap_stat_Tsol_by_year",".csv",sep = ""),row.names = FALSE,sep = ";")
write.table(result_HR,file=paste(path_out,Sites[n],"_gap_stat_HR_by_year",".csv",sep = ""),row.names = FALSE,sep = ";")
write.table(result_VPD,file=paste(path_out,Sites[n],"gap_stat_VPD_by_year",".csv",sep = ""),row.names = FALSE,sep = ";")



if(!require("tidyverse")) install.packages("tidyverse")
library(ggplot2)


p1<- ggplot(result_swin, mapping = aes(x =Year , y = percent, fill = Flag_Swin)) + 
  geom_bar(position= "stack", stat = "identity")+  
  scale_fill_manual(values=c('#F45E5A', '#5086FF', '#17B12B'))+
  geom_text(aes(label = percent),
            position = position_stack(vjust = .5),angle = 90)+
  ggtitle(Variable[1])+
  theme_bluewhite()

p2<- ggplot(result_Tair, mapping = aes(x =Year , y = percent, fill = Flag_Tair)) + 
  geom_bar(position= "stack", stat = "identity")+  
  scale_fill_manual(values=c('#F45E5A', '#5086FF', '#17B12B'))+
  geom_text(aes(label = percent),
            position = position_stack(vjust = .5),angle = 90)+
  ggtitle(Variable[2])+
  theme_bluewhite()



p3<- ggplot(result_Tsol_1_10, mapping = aes(x =Year , y = percent, fill = Flag_Tsol_1_10)) + 
  geom_bar(position= "stack", stat = "identity")+  
  scale_fill_manual(values=c('#F45E5A', '#5086FF', '#17B12B'))+
  geom_text(aes(label = percent),
            position = position_stack(vjust = .5),angle = 90)+
  ggtitle(Variable[3])+
  theme_bluewhite()



p4<- ggplot(result_HR, mapping = aes(x =Year , y = percent, fill = Flag_HR )) + 
  geom_bar(position= "stack", stat = "identity")+  
  scale_fill_manual(values=c('#F45E5A', '#5086FF', '#17B12B'))+
  geom_text(aes(label = percent),
            position = position_stack(vjust = .5),angle = 90)+
  ggtitle(Variable[4])+
  theme_bluewhite()



p5<- ggplot(result_VPD, mapping = aes(x =Year , y = percent, fill = Flag_VPD)) + 
  geom_bar(position= "stack", stat = "identity")+  
  scale_fill_manual(values=c('#F45E5A', '#5086FF', '#17B12B'))+
  geom_text(aes(label = percent),
            position = position_stack(vjust = .5),angle = 90)+
  ggtitle(Variable[5])+
  theme_bluewhite()


Qi<-ggarrange(p2,p4,p1,p5,p3,nrow = 2,ncol=3,common.legend = TRUE, legend="bottom")

Qi_M<-annotate_figure(Qi, top = text_grob(paste("Percentage of gap in year by  variable per site for ",Sites[n],sep=""), 
                                          color = "black", face = "bold", size = 12))







