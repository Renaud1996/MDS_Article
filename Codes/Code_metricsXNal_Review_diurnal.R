library(lubridate)
library(REddyProc)
library(dplyr)
library(stringr)
library(stringi)
library(beepr)
library(Metrics)
library(EntropyExplorer)

Sites<-c("Nalohou")
SitesID<-c("Nal")

n<- 1

path<-paste("G:/DOSSIERS RENAUD/anon-ms-example/French_version/MDS_Article/")
setwd(path)
source("G:/DOSSIERS RENAUD/anon-ms-example/French_version/MDS_Article/Codes/day_and_nigth2_demis.R")



Data_cyclediune_test<-read.csv(paste(path,"Tables/MDS_Test/Cycle_diurne_test/Day_of_test/Cycle_diurne_moyen_test.csv",sep=""),sep=";", header = TRUE, na.strings = "NA", skip=0, dec=".",strip.white = TRUE)
row.names(Data_cyclediune_test)<-NULL
Variable_names<-colnames(Data_cyclediune_test)


Data_test<-read.csv(paste(path,"Tables/MDS_Test/Year_of_test/Data_test.csv",sep=""),sep=";", header = TRUE, na.strings = "NA", skip=0, dec=".",strip.white = TRUE)
DateUTC<-ymd_hms(Data_test$Date)
Data_test<-cbind(DateUTC,subset(Data_test,select =c("Date_only","heure",Variable_names)))

Day_or_nigth_1.2<-day_and_nigth(1.60,9.74,Data_test)
Localisation <- "(9.74°N,1.60°E)"

Data_test<-cbind(Day_or_nigth_1.2,Data_test)


Variable_names1<-str_replace(Variable_names, pattern = "Tsol_1_10", replacement = "Tsoil")
setwd(paste(path,"Tables/MDS_Test/Cycle_diurne_test/Output_of_test/",sep = ""))


simulation<-c(1:4)
simulation<-as.character(simulation)

month_num<-c(1:12)
Month_name<-as.character(month(month_num,label = TRUE,abbr = FALSE))
Month_nameID<-c("01","02","03","04","05","06","07","08","09","10","11","12")




Noon<-c("Nigth","Befor_mid","After_mid")
NoonID<-c("NiT","BeN","AfN")

N<-12

X<-NULL


X<-read.csv(paste(path,"Tables/MDS_Test/Cycle_diurne_test/Output_of_test/",Sites[n],"_MDS_test_Metric_diurnal.csv",sep = ""),sep=";", header = TRUE, na.strings = "NA", skip=0, dec=".",strip.white = TRUE)
rownames(X) <- Variable_names1
Variable<-rownames(X)



###########################################################################################################
# load packages
library(ggplot2) # ggplot() for plotting
library(dplyr) # data reformatting
library(tidyr) # data reformatting
library(stringr) # string manipulation
library(ggsci)
library(gridExtra)
library(ggthemes)
library(ggpubr)
X_percent<-c(25,50,100)
X_percent_suffix<-as.character(X_percent)
set.seed(0852)#Pour rendre reproductif ? l'avenir

p1<-NULL
p2<-NULL
p3<-NULL
p4<-NULL
p5<-NULL
y_scale<-c(paste(NoonID[1],"_",Month_nameID,sep = ""),paste(NoonID[2],"_",Month_nameID,sep = ""),paste(NoonID[3],"_",Month_nameID,sep = ""))
Metrics_colname=c("Variable",paste(NoonID[1],"_",Month_nameID,sep = ""),paste(NoonID[2],"_",Month_nameID,sep = ""),paste(NoonID[3],"_",Month_nameID,sep = ""))





theme_bluewhite<- function (base_size = 11, base_family = "") {
  theme_bw() %+replace% 
    theme(
      panel.grid.major  = element_line(color = "grey",size = 0.5),
      panel.background = element_rect(fill = "white"),
      panel.border = element_rect(color = "grey", fill = NA),
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






p1<-NULL
p2<-NULL
p3<-NULL
p4<-NULL
p5<-NULL







for (i in 1:length(X_percent_suffix) ) {
  Data_metrics<-cbind(Variable,subset(X,select =str_subset(colnames(X),paste("PBE_",X_percent_suffix[i],"_",sep="")) ))
  colnames(Data_metrics)=Metrics_colname
  Data_metrics2 <-Data_metrics %>%
    # convert data to long format
    gather(key="state",value="value",-Variable) %>%
    # rename columns
    setNames(c("Variable","state","value")) %>%
    # convert value to numeric (also converts '-' to NA, gives a warning)
    mutate(value=as.numeric(value))
  
  # assign text colour
  textcol <- "black"
  
  
  if(i==1){
    # vi<-c(-0.10,-0.001,0.09,0.107)
    Data_metrics3 <- Data_metrics2 %>%
      # convert state to factor and reverse order of levels
      mutate(state=factor(state,levels=rev(sort(unique(state))))) %>%
      # create a new variable from count
      mutate(countfactor=cut(value,breaks=c(-50,-25,-15,-10,10,15,25,50),
                             labels=c("Bad","Satisfactory","Good","Very_Good","Good","Satisfactory","Bad") ))
    # Data_metrics3$countfactor <- as.character(Data_metrics3$countfactor)
    
    Data_metrics3$state <- factor(Data_metrics3$state,levels =c(paste(NoonID[1],"_",X1,sep = ""),paste(NoonID[3],"_",X1,sep = ""),paste(NoonID[2],"_",X1,sep = "")) )
    
    Data_metrics3$countfactor <- factor(Data_metrics3$countfactor,levels = c("Very_Good","Good","Satisfactory","Bad"))
    
    
    for (r in 1:length(Data_metrics3$countfactor)) {
      if(is.na(Data_metrics3$countfactor[r])){
        Data_metrics3$countfactor[r] <- "Bad"
      }
      
    }
    
    
    # further modified ggplot
    p <- ggplot(Data_metrics3,aes(x=Variable,y=state,fill=countfactor,width=0.6,height=1))+
      #add border white colour of line thickness 0.25
      geom_tile(colour="#FFFFFF",size=1)+
      #remove x and y axis labels
      guides(fill=guide_legend(title="Intrvals"))+
      #labs(x="",y="",title=paste("Percentage of bias for",X_percent_suffix[i],"% of gap injected"))+
      labs(x="",y="")+
      #remove extra space
      scale_y_discrete(expand=c(0,0))+
      #define new breaks on x-axis
      scale_x_discrete(expand=c(0,0),
                       breaks=Variable)+
      scale_fill_manual(values=c("#008000","#8CEF74","#FF6600","#550000"),na.value = "grey")+
      #coord_fixed()+
      theme_grey(base_size=10)+
      ggtitle(paste("PBIAS ",X_percent_suffix[i],"%",sep=""))+
      theme_bluewhite()
  }
  if(i!=1){
    # vi<-c(-0.5,-0.001,0.09,0.426)
    Data_metrics3 <- Data_metrics2 %>%
      # convert state to factor and reverse order of levels
      mutate(state=factor(state,levels=rev(sort(unique(state))))) %>%
      # create a new variable from count
      mutate(countfactor=cut(value,breaks=c(-50,-25,-15,-10,10,15,25,50),
                             labels=c("Bad","Satisfactory","Good","Very_Good","Good","Satisfactory","Bad") ))
    # Data_metrics3$countfactor <- as.character(Data_metrics3$countfactor)
    
    Data_metrics3$state <- factor(Data_metrics3$state,levels =c(paste(NoonID[1],"_",X1,sep = ""),paste(NoonID[3],"_",X1,sep = ""),paste(NoonID[2],"_",X1,sep = "")) )
    
    Data_metrics3$countfactor <- factor(Data_metrics3$countfactor,levels = c("Very_Good","Good","Satisfactory","Bad"))
    
    
    for (r in 1:length(Data_metrics3$countfactor)) {
      if(is.na(Data_metrics3$countfactor[r])){
        Data_metrics3$countfactor[r] <- "Bad"
      }
      
    }
    
    
    p <- ggplot(Data_metrics3,aes(x=Variable,y=state,fill=countfactor,width=0.6,height=1))+
      #add border white colour of line thickness 0.25
      geom_tile(colour="#FFFFFF",size=1)+
      #remove x and y axis labels
      guides(fill=guide_legend(title="Intrvals"))+
      #labs(x="",y="",title=paste("Percentage of bias for",X_percent_suffix[i],"% of gap injected"))+
      labs(x="",y="")+
      #remove extra space
      scale_y_discrete(expand=c(0,0))+
      #define new breaks on x-axis
      scale_x_discrete(expand=c(0,0),
                       breaks=Variable)+
      # labels=c("Underestimate","Good","Overestimate")
      scale_fill_manual(values=c("#008000","#8CEF74","#FF6600","#550000"),na.value = "grey")+
      # scale_fill_discrete()+
      #coord_fixed()+
      #coord_fixed()+
      theme_grey(base_size=10)+
      ggtitle(paste("PBIAS ",X_percent_suffix[i],"%",sep=""))+
      theme_bluewhite()
  }
  # further modified ggplot
  
  
  if(i==1){
    p1<-p
  }
  if(i==2){
    p2<-p
  }
  if(i==3){
    p3<-p
  }
  if(i==4){
    p4<-p
  }
  if(i==5){
    p5<-p
  }
}

Q_i_PBE<-ggarrange(p1,p2,p3,nrow =1 ,ncol=3,common.legend = TRUE, legend="right",legend.grob =get_legend(p3) )

# Q_i_PBE_Nal<-annotate_figure(Q_i_PBE_Nal, top = text_grob(paste(Sites[n],sep=" "), 
#                                                 color = "black", face = "bold", size = 14))




p1 <- NULL
p2 <- NULL
p3 <- NULL
p4 <- NULL
p5 <- NULL

for (i in 1:length(X_percent_suffix) ) {
  
  
  Data_metrics<-cbind(Variable,subset(X,select =str_subset(colnames(X),paste("RSR_",X_percent_suffix[i],"_",sep="")) ))
  colnames(Data_metrics)=Metrics_colname
  Data_metrics2 <-Data_metrics %>%
    # convert data to long format
    gather(key="state",value="value",-Variable) %>%
    # rename columns
    setNames(c("Variable","state","value")) %>%
    # convert value to numeric (also converts '-' to NA, gives a warning)
    mutate(value=as.numeric(value))
  
  
  Data_metrics3 <- Data_metrics2 %>%
    # convert state to factor and reverse order of levels
    mutate(state=factor(state,levels=rev(sort(unique(state))))) %>%
    # create a new variable from count
    mutate(countfactor=cut(value,breaks=c(0,0.5,0.6,0.7,max(Data_metrics2$value)),
                           labels=c("Very_Good","Good","Satisfactory","Bad") ))
  
  # Data_metrics3$countfactor <- as.character(Data_metrics3$countfactor)
  
  # for (r in 1:length(Data_metrics3$countfactor)) {
  #   if(is.na(Data_metrics3$countfactor[r])){
  #     Data_metrics3$countfactor[r] <- "Bad"
  #   }
  #   
  # }
  
  Data_metrics3$state <- factor(Data_metrics3$state,levels =c(paste(NoonID[1],"_",X1,sep = ""),paste(NoonID[3],"_",X1,sep = ""),paste(NoonID[2],"_",X1,sep = "")) )
  
  Data_metrics3$countfactor <- factor(Data_metrics3$countfactor,levels = c("Very_Good","Good","Satisfactory","Bad"))
  
  
  # assign text colour
  textcol <- "black"
  
  # further modified ggplot
  p <- ggplot(Data_metrics3,aes(x=Variable,y=state,fill=countfactor,width=0.6,height=1))+
    #add border white colour of line thickness 0.25
    geom_tile(colour="#FFFFFF",size=1)+
    #remove x and y axis labels
    guides(fill=guide_legend(title="Intrvals"))+
    #labs(x="",y="",title=)+  #remove extra space
    labs(x="",y="")+  #remove extra space
    scale_y_discrete(expand=c(0,0))+
    #define new breaks on x-axis
    scale_x_discrete(expand=c(0,0),
                     breaks=Variable)+
    # labels=c("Unsatisfactory","Satisfactory","Good")
    scale_fill_manual(values=c("#008000","#8CEF74","#FF6600","#550000"),na.value = "grey")+
    #coord_fixed()+
    theme_grey(base_size=10)+
    ggtitle(paste("RSR ",X_percent_suffix[i],"%",sep=""))+
    theme_bluewhite()  
  if(i==1){
    p1<-p
  }
  if(i==2){
    p2<-p
  }
  if(i==3){
    p3<-p
  }
  if(i==4){
    p4<-p
  }
  if(i==5){
    p5<-p
  }
  
  
}


Q_i_RSR<-ggarrange(p1,p2,p3,nrow =1,ncol=3,common.legend = TRUE, legend="right")


p1 <- NULL
p2 <- NULL
p3 <- NULL
p4 <- NULL
p5 <- NULL

for (i in 1:length(X_percent_suffix) ) {
  Data_metrics<-cbind(Variable,subset(X,select =str_subset(colnames(X),paste("DSE_",X_percent_suffix[i],"_",sep="")) ))
  colnames(Data_metrics)=Metrics_colname
  Data_metrics2 <-Data_metrics %>%
    # convert data to long format
    gather(key="state",value="value",-Variable) %>%
    # rename columns
    setNames(c("Variable","state","value")) %>%
    # convert value to numeric (also converts '-' to NA, gives a warning)
    mutate(value=as.numeric(value))
  
  vi<-c(1e-12,1e-06,1e-05,3.2e-05)
  Data_metrics3 <- Data_metrics2 %>%
    # convert state to factor and reverse order of levels
    mutate(state=factor(state,levels=rev(sort(unique(state))))) %>%
    # create a new variable from count
    mutate(countfactor=cut(value,breaks=c(vi,max(value,na.rm=T)),
    )) %>%
    # change level order
    mutate(countfactor=factor(as.character(countfactor),levels=rev(levels(countfactor))))
  
  Data_metrics3$state <- factor(Data_metrics3$state,levels =c(paste(NoonID[1],"_",X1,sep = ""),paste(NoonID[3],"_",X1,sep = ""),paste(NoonID[2],"_",X1,sep = "")) )
  
  
  # assign text colour
  textcol <- "black"
  
  # further modified ggplot
  p <- ggplot(Data_metrics3,aes(x=Variable,y=state,fill=countfactor,width=0.6,height=1))+
    #add border white colour of line thickness 0.25
    geom_tile(colour="#FFFFFF",size=1)+
    #remove x and y axis labels
    guides(fill=guide_legend(title="Intrvals"))+
    #labs(x="",y="",title=paste("Differential Shanon Entropy  for",X_percent_suffix[i],"% of gap injected"))+
    labs(x="",y="")+
    #remove extra space
    scale_y_discrete(expand=c(0,0),breaks=y_scale)+
    #define new breaks on x-axis
    scale_x_discrete(expand=c(0,0),
                     breaks=Variable  )+
    # labels=c("Less Homogene","Homogene","Very Homogene")
    scale_fill_manual(values=c("#550000","#FF6600","#8CEF74","#008000"),na.value = "grey")+
    #scale_fill_manual(values=c("#7C878EFF","#000000","#C0C0C0","#5C88DAFF","#CC0C00FF","#708090","#000000"),na.value = "grey")+
    #coord_fixed()+
    ggtitle(paste("DSE ",X_percent_suffix[i],"%",sep=""))+
    theme_bluewhite()
  if(i==1){
    p1<-p
  }
  if(i==2){
    p2<-p
  }
  if(i==3){
    p3<-p
  }
  if(i==4){
    p4<-p
  }
  if(i==5){
    p5<-p
  }
  
  
  
  
}
Q_i_DSE<-ggarrange(p1,p2,p3,nrow = 1,ncol=3,common.legend = TRUE, legend="right",legend.grob =get_legend(p3))

Q_i<-ggarrange(Q_i_PBE,Q_i_RSR,Q_i_DSE,nrow = 3,ncol=1,common.legend = FALSE, legend="right" )

Q_i<-annotate_figure(Q_i, top = text_grob(paste(Sites[n],Localisation,sep=" "), 
                                          color = "black", face = "bold", size = 14))
Q_i_Nalohou <- Q_i
ggsave(paste(path,Sites[n],"/Graphe/Metrics_test_plot/Cycle_diurne_test/",Sites[n],"_Heatmap_","MetricsXXXX_diurnal.svg",sep=""),plot=Q_i,width = 15,height = 14)
