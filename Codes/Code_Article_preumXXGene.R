rm(list = ls(all=TRUE))
ls()
getwd()


#Chargement des librairies 

library('grid')
library('dplyr')
library('lubridate')
library('tidyr')
library('timetk')
#Choix du sites


Sites<-c("Nalohou")
SitesID<-c("Nal")

Dates_Begin<-as.character(seq(from=010106,to=010118,by=1))
Dates_end<-as.character(seq(from=311206,to=311218,by=1))

n<- 1

#Chargement des donn?es Brut
if(n==1){
  i<-1
  
  path<-paste("G:/DOSSIERS RENAUD/anon-ms-example/French_version/MDS_Article"
,"/",sep="")  
  
setwd(paste(path,"Tables/Brut",sep = ""))

i<-3
DATA08<-subset(read.table(file=paste(SitesID[n],"_MET_0",Dates_Begin[i],"_",Dates_end[i],"X.csv",sep = "") ,sep=";", header = TRUE, na.strings = "NA", skip=0, dec=".",strip.white = TRUE), select=c("heure","Date","Swin","Tair","Tsol_1_10","HR"))
DATA08$esat<-6.108*exp((17.27*DATA08$Tair)/(DATA08$Tair+237.3))
DATA08$ea<-(DATA08$HR*DATA08$esat)/100
} 



Colonne_names<-c("heure","Date","Swin","Tair","Tsol_1_10","HR","esat","ea")

colnames(DATA08)<-Colonne_names




#Concatenation des frames
Data<-DATA08

#calcul de vpd
VPD<-Data$esat-Data$ea
VPD<-round(VPD,3)
#

if((n==1)||(n==2)){
  Data$Date<-dmy_hm(Data$Date)  
}

Date_only<-lubridate::date(Data$Date)
#Data<-cbind(Date_only,heure,Data[,1:5])
Data<-cbind(Date_only,Data[,1:6])
#Finalisation de la Donn?e
Data<-cbind(Data,VPD)


Variable_ID<-colnames(Data[,4:8])
Variable_names<-c("Short incoming radiation","Air Temperature ","Sol Temperature","Relative Humidity","VPD")

##########################################################################################################
#Save Data file                                                                                         # 
##########################################################################################################
write.table(Data,file=paste("G:/DOSSIERS RENAUD/anon-ms-example/French_version/MDS_Article/Tables/Data_full/Data_full.csv",sep = ""),row.names = FALSE,sep = ";")

Data=read.csv(paste("G:/DOSSIERS RENAUD/anon-ms-example/French_version/MDS_Article/Tables/Data_full/Data_full.csv",sep=""),sep=";", header = TRUE, na.strings = "NA", skip=0, dec=".",strip.white = TRUE)


#Enregistrement de la periode teste
if((n==1)||(n==2)){
    Data_year<-Data[-1,]
    rownames(Data_year) <- NULL
  }
write.table(Data_year,file=paste("G:\DOSSIERS RENAUD\anon-ms-example\French_version\MDS_Article\Tables\MDS_Test\Year_of_test/Data_test.csv",sep = ""),row.names = FALSE,sep = ";")

#Preparation des donn?es format REddyPro
#Pour donn?e test

Data_year$YDay<-yday(Data_year$Date)
Data_year$Day<-day(Data_year$Date)
Data_year$minute<-minute(Data_year$Date)
Data_year$Year<-year(Data_year$Date)
Data_year$Month<-month(Data_year$Date)


#Extraction de donnees requis
DATA_Year<-subset(Data_year, select=c("Year","Month","Day","heure","Swin","Tair","Tsol_1_10","HR","VPD"))

colnames(DATA_Year)=c("Year","Month","Day","Hour","Rg","Tair","Tsoil","Rh","VPD")
#DATA_Year<-DATA_Year[-1,]

XXX<-c("--","--","--","--","Wm-2","degC","degC","%","hPa")
DATA_Year<-rbind(XXX,DATA_Year)

write.table(DATA_Year,file=paste("G:/DOSSIERS RENAUD/anon-ms-example/French_version/MDS_Article/Tables/REddyProc_TableInput","/DATA_test_REddyPro.csv",sep = ""),row.names = FALSE,sep = ";")

##Determination du cycle diurne  mensuelle
library(lubridate)
library(REddyProc)
library(dplyr)
library(stringr)
library(stringi)

#Chargement de la donn?e
path<-paste("G:/DOSSIERS RENAUD/anon-ms-example/French_version/MDS_Article/"
            ,sep="")
if((n==1)||(n==2)){
  Data_test<-subset(read.csv(paste("G:/DOSSIERS RENAUD/anon-ms-example/French_version/MDS_Article/Tables/Data_full/Data_full.csv",sep=""),sep=";", header = TRUE, na.strings = "NA", skip=0, dec=".",strip.white = TRUE),format(as.Date(Date_only),"%Y")==2008)
  
}

Data_test$Date<-ymd_hms(Data_test$Date)
Data_test$Date_only <-lubridate::date(Data_test$Date)
Year<-year(Data_test$Date)
Month<-month(Data_test$Date)
Day<-day(Data_test$Date)
Data_test<-cbind(Year,Month,Day,Data_test)

Data_test1<-subset(Data_test, select=c("Year","Month","heure","Swin","Tair","Tsol_1_10","HR","VPD"))

#Cycle diurne moyen mensuel
Data_mean<-aggregate(Data_test1, by=list(Data_test1$heure,Data_test1$Month), FUN=mean)
Data_date<-Data_mean[,3:5]
Data_mean<-Data_mean[,-1:-5]
Data_mean<-round(Data_mean[,order(names(Data_mean))],digits = 3)  
Data_mean<-cbind(Data_date,Data_mean)

write.table(Data_mean,file=paste(path,"Tables/Cycle_diurne_test/Day_of_test/Cycle_diurne_moyen_mens.csv",sep = ""),row.names = FALSE,sep = ";")

Data_test1<-subset(Data_test, select=c("Date_only","Month","Day","heure","HR","Swin","Tair","Tsol_1_10","VPD"))

Variable_names<-colnames(Data_test1[,5:9])

#Cycle diurne moyen Test 
Month_list<-list()#Pour la r?cup?ration des cycle diurne test par mois(taille: nombre de mois)
for (i in 1:12) {#Parcours des Mois 
  Month_i_test<-filter(Data_test1,Month==i)#Extraction des donn?es du Mois i
  Month_i_test_mean<-filter(Data_mean,Month==i)#Extration du cycle diurne moyen mensuel correspondant au mois i 
  
  date1<-as.POSIXct(Month_i_test$Date_only[1])
  date2<-as.POSIXct(Month_i_test$Date_only[length(Month_i_test$Date_only)])
  
  x<-difftime(date1,date2,units = "days")
  n_day<- (-as.numeric(x))+1#Recup?rer le nombre de jours dans le mois i
  Variable_date<-c()#Recup?rer les dates test correspondantes pour chaque variable(taille : le nombre de variables)
  for (k in 1:length(Variable_names)) {#Parcourt des variables
    cpte<-1
    #Variable_vector<-c()
    Variable_distance<-c()#Recuperation de la distance de chaque jours du mois i par rapport au cycle diurne moyen mensuel i pour une variable en position k(taille :le nombre de jours n_day)
    for (j in 1:n_day) {#Parcourt des jours j de Month_i_test pour la variable en positions k
      Day_Month_i_test<-filter(Month_i_test,Day==j)#Recupp?ration d'un jours j
      X<-dist(rbind(Day_Month_i_test[,Variable_names[k]],Month_i_test_mean[,Variable_names[k]])) #Calcul de la distance entre le jours j et le cycle diurne moyen mensuel i pour la variable k 
      Variable_distance[j]<-as.numeric(X)#Stokage de la distance ? la position j   
      
    }
    Jours<-which(Variable_distance==min(Variable_distance),arr.ind = T)#R?cup?rer l'indice de la distance la plus petite dans Variable distance
    Indice<-which(Month_i_test$Day==Jours)[1]#R?cuperation de l'indice du jour test correspondant dans Month_i_test
    Date<-Month_i_test$Date_only[Indice]
    Variable_date[k]<-format.Date(Date,"%Y-%m-%d")#Stockage de la date correspondant  au jours test ? la position k
    cat(k,"____________________________\n")
    
  }
  
  Month_list[[i]]<-Variable_date#Stockage de Variable_date pour le mois in dans Month_list
  rm(Variable_date,Month_i_test,Month_i_test_mean)#Clean up
}

month_num<-c(1:12)
Month_name<-as.character(month(month_num,label = TRUE,abbr = FALSE))
# Pour avoir la matrice des cycle diurne test par mois pour chaque variables d'interet.
#Formatage de la liste Month_list en data frame et stockage
auxi<-max(sapply(Month_list,length))
res<-sapply(Month_list,function(u) c(u,rep(NA,auxi-length(u))))
res<-as.data.frame(t(res))
colnames(res)=Variable_names
rownames(res)=as.character(month(month_num,label = TRUE,abbr = FALSE))
res<-as.data.frame(res)

write.table(res,file=paste(path,"/Tables/Cycle_diurne_test/Day_of_test/Cycle_diurne_moyen_test.csv",sep = ""),row.names = TRUE,sep = ";")

#######################################################################################
#Evaluation du MDS
#######################################################################################
#Chargement des librairies
rm(list = ls(all=TRUE))
ls()
getwd()

library(lubridate)
library(REddyProc)
library(dplyr)
library(stringr)
library(stringi)
library(beepr)
#Choix du sites
Sites<-c("Nalohou")
SitesID<-c("Nal")



n<- 1
path<-paste("G:/DOSSIERS RENAUD/anon-ms-example/French_version/MDS_Article/")
setwd(path)
source("G:/DOSSIERS RENAUD/anon-ms-example/French_version/MDS_Article/Codes/day_and_nigth2_demis.R")

#Chargement de la table pour le cycle diurne test
Data_cyclediune_test<-read.csv(paste(path,"/Tables/Cycle_diurne_test/Day_of_test/Cycle_diurne_moyen_test.csv",sep=""),sep=";", header = TRUE, na.strings = "NA", skip=0, dec=".",strip.white = TRUE)
row.names(Data_cyclediune_test)<-NULL
Variable_names<-colnames(Data_cyclediune_test)





Data_test<-read.csv(paste(path,"Tables/MDS_Test/Year_of_test/Data_test.csv",sep=""),sep=";", header = TRUE, na.strings = "NA", skip=0, dec=".",strip.white = TRUE)
DateUTC<-ymd_hms(Data_test$Date)
Data_test<-cbind(DateUTC,subset(Data_test,select =c("Date_only","heure",Variable_names)))


if(n==1){
  Day_or_nigth_1.2<-day_and_nigth(1.60457,9.74484,Data_test)
  
  
}

Data_test1<-cbind(Day_or_nigth_1.2,Data_test)
Data_test<-cbind(Day_or_nigth_1.2,Data_test)

setwd(paste(path,"Tables/MDS_Test/Cycle_diurne_test/Output_of_test/",sep = ""))#Chemin pour le prochain r?pertoire

Noon<-c("Nigth","Befor_mid","After_mid")#Vecteur  de stockage des noms des diff?rent p?riode d'unn jours test
month_num<-c(1:12)
Month_name<-as.character(month(month_num,label = TRUE,abbr = FALSE))#Nom des mois
N<-12
set.seed(0852)#Pour rendre reproductif ? l'avenir

for (m in 1:length(Noon)) {#Injection des gap par p?riode d'un jours test
  
  path1<-paste(path,"Tables/MDS_Test/Cycle_diurne_test/Output_of_test/",Noon[m],"/",sep = "")#Chemin pour atteindre le dossier pour les output de la p?riode correspondante
  
  X_percent<-c(50,60,70)
  X_percent_suffix<-as.character(X_percent)#Chargement des diff?rent pourcentage
  Percent_file<-paste("Percent_",X_percent_suffix,sep="")#Nom des dossiers pour chaque pourcentage d'injection de gap par p?riode de la journ?e
  
  pb_a<-txtProgressBar(min = 1,max = length(X_percent),style = 3)# barre de progression de la compilation
  for (t in 1:length(X_percent)) {#Injection de gap de pourcentage en position t
    
    for (i in 1:N) {#Choix du mois(jours test) d'injection du %t
      Cycle_diurne_month_test<-Data_cyclediune_test[i,]# Recuperation de la ligne i de la data_frame contenant les cycle diurne test par variable pour chaque mois 
      
      Indice_list<-list()#Liste pour r?cupp?rer les indices des demis des jours test correspondant pour chaque variable(5 vecteurs dans la list)
      
      for(l in 1:length(Cycle_diurne_month_test)){#R?cuperation des indices par variable et stockage
        A<-as.character(Cycle_diurne_month_test[l]) 
        Indice<-which(Data_test$Date_only==A & Data_test$Day_or_nigth_1.2==m-1)
        Indice_list[[l]]<-Indice
      }
      #Formatage en data frame
      auxi<-max(sapply(Indice_list,length))
      res<-sapply(Indice_list,function(u) c(u,rep(NA,auxi-length(u))))
      res<-as.data.frame(res)
      colnames(res)=Variable_names
      Indice_list<-as.data.frame(res)
      
      simulation<-c(1:4)#Nombre de simulation ? effectuer
      simulation<-as.character(simulation)
      
      XVal<-Data_test1#Data sans les valeurs de la logne 1(00:00)
      
      
      for (j in 1:length(simulation)) {#Parcourt des simulations
        data_tenpercentnumber_day_1<-(length(Indice_list$HR)*X_percent[t])/100#Calcul du nombre d'indice correspondant a %t
        
        del<-list()#Liste pour la r?cup?ration des indice s?lection?e al?atoirement pour chaque variable pour la simulation j et pour le jour i(nligne:data_tpercentnumber_day_1,ncolone:nombre de variable)     
        
        for (s in 1:length(Cycle_diurne_month_test)) {#R?cuperation des indices par variable et stockage
          del[[s]]<-sort(sample(Indice_list[,s],data_tenpercentnumber_day_1,replace = FALSE))
        }
        #Formatage en data frame  
        auxi<-max(sapply(del,length))
        res<-sapply(del,function(u) c(u,rep(NA,auxi-length(u))))
        res<-as.data.frame(res)
        colnames(res)=Variable_names
        del<-as.data.frame(res)
        
        
        #############################################
        # Artificial Gap filling#
        #############################################
        
        
        
        ### Loading the required packages ==============================================
        
        # You might need to install the packages first. In that case use:
        # install.packages("packagename")
        library(openeddy)
        library(REddyProc)
        
        
        
        siteyear <- SitesID[n] # used as output filename and as 'ID.s' in sEddyProc$new()
        Tstamp <- format(Sys.time(), "%Y-%m-%d") # Timestamp of the computation
        path_out <- paste(path1,"Output_by_gap_percentage/",Percent_file[t],"/",sep="")
        
        pkg_version <- packageVersion("REddyProc")
        
        
        

        if(n==1){
          lat <- 9.74# edit site latitude
          long <- 1.60 # edit site longtitude
          input <- "G:/DOSSIERS RENAUD/anon-ms-example/French_version/MDS_Article/Tables/REddyProc_TableInput/DATA_test_REddyPro.csv"
          
        }
        
        tz <- 1 # timezone
        # Include meteo variables that will be plotted, gap-filled and exported
        meteo <- c("Rg", "Tair", 'VPD')
        # Show precheck plots in the console? Default: plot_to_console <- TRUE
        plot_to_console <- TRUE
       # used when plotting to console (single year allowed)
        year <- 2008
        # Save plots as "png" (default) or "pdf"; NEEvsUStar plots are fixed to "pdf"
        plot_as <- "png" 
        
        ### Data preparation =============================================================
        
        # Load data with one header and one unit row from (tab-delimited) text file
        EddyData.F <- read_eddy(input, sep = ";")
        
        Eddy_var_name<-c("Rh","Rg","Tair","Tsoil","VPD")
        
        for (s in 1:length(Cycle_diurne_month_test)) {
          at_gap<-del[,s]
          
          at_gap<-as.numeric(na.omit(at_gap))  
          Q<-Eddy_var_name[s]
          EddyData.F[at_gap,Q]<-NA
        }
        
        # Add time stamp in POSIX time format
        EddyDataWithPosix.F <- fConvertTimeToPosix(
          EddyData.F, 'YMDH', Year = 'Year', Day = 'Day', Hour = 'Hour',Month = 'Month')
        
        # Initalize R5 reference class sEddyProc for processing of eddy data
        # with all variables needed for processing later
        variables <- c("Rg","Tair","Tsoil","Rh","VPD")
        EddyProc.C <- sEddyProc$new(siteyear, EddyDataWithPosix.F, variables)
        EddyProc.C$sSetLocationInfo(lat, long, tz)  # site location info
        
        # See the content
        str(EddyProc.C)
        EddyProc.C$sPrintFrames(NumRows.i = 6L)
        
        # Meteo must be gap-filled even when without gaps to run the partitioning 
        for (met_var in variables) {#MDS Gap_filling
          EddyProc.C$sMDSGapFill(met_var, FillAll = TRUE,suffix = paste(X_percent_suffix[t],"_",simulation[j], sep = "") )
        }
        
        Gapvalfill<-EddyProc.C$sExportResults()
        FP_vars <- c(paste(variables,"_",X_percent_suffix[t],"_",simulation[j],"_f", sep = ""))
        FP_varsd <- c(paste(variables,"_",X_percent_suffix[t],"_",simulation[j],"_fsd", sep = ""))
        V_gap_101<-subset(Gapvalfill, select=FP_vars)#Recup?ration des colonne des variable gapfiller
        V_gap_101sd<-subset(Gapvalfill, select=FP_varsd)
        variables1<-c("Swin","Tair","Tsoil","HR","VPD")
        colnames(V_gap_101)=c(paste(variables1,"_",X_percent_suffix[t],"_",simulation[j],"_f", sep = ""))#Refoematage qu nom de base
        colnames(V_gap_101sd)=c(paste(variables1,"_",X_percent_suffix[t],"_",simulation[j],"_fsd", sep = ""))
        
        XVal<-cbind(XVal,V_gap_101,V_gap_101sd)#Concatenation avec XVal
        remove(V_gap_101,del,Gapvalfill)  
      }
      write.table(XVal,file=paste(path_out,Sites[n],"_artificial_gap_",Month_name[i],"_",X_percent_suffix[t],".csv",sep = ""),row.names = FALSE,sep = ";")#Sauvegade dans la direction
      remove(XVal)
      
      
      if(i==12){#test
        beep(2) 
      }
    }
    
    if(t==length(X_percent)){#test
      beep(3)
    }
    
    Sys.sleep(60)
    setTxtProgressBar(pb_a,t)
    
    
    
  }
  
}


#######################################################################################
#Calcule des moyenne des simulations
#######################################################################################

library(lubridate)
library(REddyProc)
library(dplyr)
library(stringr)
library(stringi)
library(beepr)


#Choix du sites
Sites<-c("Nalohou")
SitesID<-c("Nal")



n<-1  
path<-paste("G:/DOSSIERS RENAUD/anon-ms-example/French_version/MDS_Article/")
setwd(path)
source("G:/DOSSIERS RENAUD/anon-ms-example/French_version/MDS_Article/Codes/day_and_nigth2_demis.R")



Data_cyclediune_test<-read.csv(paste(path,"Tables/MDS_Test/Cycle_diurne_test/Day_of_test/Cycle_diurne_moyen_test.csv",sep=""),sep=";", header = TRUE, na.strings = "NA", skip=0, dec=".",strip.white = TRUE)
row.names(Data_cyclediune_test)<-NULL
Variable_names<-colnames(Data_cyclediune_test)


Data_test<-read.csv(paste(path,"Tables/MDS_Test/Year_of_test/Data_test.csv",sep=""),sep=";", header = TRUE, na.strings = "NA", skip=0, dec=".",strip.white = TRUE)
DateUTC<-ymd_hms(Data_test$Date)
Data_test<-cbind(DateUTC,subset(Data_test,select =c("Date_only","heure",Variable_names)))


if(n==1){
  Day_or_nigth_1.2<-day_and_nigth(1.60457,9.74484,Data_test)
  
  
}

Data_test<-cbind(Day_or_nigth_1.2,Data_test)


Variable_names<-str_replace(Variable_names, pattern = "Tsol_1_10", replacement = "Tsoil")
setwd(paste(path,"Tables/MDS_Test/Cycle_diurne_test/Output_of_test/",sep = ""))


simulation<-c(1:4)
simulation<-as.character(simulation)

month_num<-c(1:12)
Month_name<-as.character(month(month_num,label = TRUE,abbr = FALSE))

Noon<-c("Nigth","Befor_mid","After_mid")

N<-12

for (m in 1:length(Noon)) {
  
  path1<-paste(path,"/Tables/MDS_Test/Cycle_diurne_test/Output_of_test/",Noon[m],"/",sep = "")
  
  
  X_percent<-c(50,60,70)
  X_percent_suffix<-as.character(X_percent)
  Percent_file<-paste("Percent_",X_percent_suffix,sep="")
  
  pb_a<-txtProgressBar(min = 1,max = length(X_percent),style = 3)
  
  for (t in 1:length(X_percent)) {
    
    for (i in 1:N) {
      
      Output<-list()
      Output_Error<-list()
      path_in <- paste(path1,"Output_by_gap_percentage/",Percent_file[t],"/",sep="")
      path_out <- paste(path1,"Output_by_PecentSimulation_mean/",Percent_file[t],"/",sep="")
      
      XVal<-Data_test
      X<-read.csv(paste(path_in,Sites[n],"_artificial_gap_",Month_name[i],"_",X_percent_suffix[t],".csv",sep = ""),sep = ";")
      
      for (k in 1:length(Variable_names)) {
        FP_vars<-paste(Variable_names[k],"_",X_percent_suffix[t],"_",simulation,"_f",sep="")
        X_sub<-subset(X, select=FP_vars)
        X_sub_vect<-c()
        Error_vec<-c()
        for (p in 1:nrow(X_sub)) {
          vect<-unname(unlist(X_sub[p,]))
          le<-length(simulation)
          X_sub_vect[p]<-mean(vect)
          Error_vec[p]<-round(qt(0.975,df=le-1)*sqrt(var(vect))/sqrt(le),3)  
          
        }
        
        
        Output[[k]]<-X_sub_vect
        Output_Error[[k]]<-Error_vec
        
      }
      auxi<-max(sapply(Output,length))
      res<-sapply(Output,function(u) c(u,rep(NA,auxi-length(u))))
      res<-as.data.frame(res)
      colnames(res)=paste(Variable_names,"_",X_percent_suffix[t],"_f",sep="")
      
      auxi1<-max(sapply(Output_Error,length))
      res1<-sapply(Output_Error,function(u) c(u,rep(NA,auxi-length(u))))
      res1<-as.data.frame(res1)
      colnames(res1)=paste(Variable_names,"_",X_percent_suffix[t],"_error_f",sep="")
      
      
      Output_1<-cbind(XVal,res,res1)
      
      write.table(Output_1,file=paste(path_out,Sites[n],"_artificial_gap_",Month_name[i],"_",X_percent_suffix[t],"_mean_with_error.csv",sep = ""),row.names = FALSE,sep = ";")
      remove(XVal)
      
      
      if(i==12){
        beep(2) 
      }
    }
    if(t==length(X_percent)){
      beep(3)
    }
    
    # Sys.sleep(120)
    setTxtProgressBar(pb_a,t)
    
    
    
  }
  if(m==length(Noon)){
    beep(4)
  }
  
}



#######################################################################################
#Calcule des Metrique
#######################################################################################
rm(list = ls(all=TRUE))
ls()
getwd()

library(lubridate)
library(REddyProc)
library(dplyr)
library(stringr)
library(stringi)
library(beepr)
library(Metrics)
library(EntropyExplorer)
library(hydroGOF)

Sites<-c("Nalohou")
SitesID<-c("Nal")


n<- 1
path<-paste("G:/DOSSIERS RENAUD/anon-ms-example/French_version/MDS_Article/")
setwd(path)
source("G:/DOSSIERS RENAUD/anon-ms-example/French_version/MDS_Article/Codes/day_and_nigth2_demis.R")



Data_cyclediune_test<-read.csv(paste(path,"Tables/MDS_Test/Cycle_diurne_test/Day_of_test/Cycle_diurne_moyen_test.csv",sep=""),sep=";", header = TRUE, na.strings = "NA", skip=0, dec=".",strip.white = TRUE)
row.names(Data_cyclediune_test)<-NULL
Variable_names<-colnames(Data_cyclediune_test)


Data_test<-read.csv(paste(path,"/Tables/MDS_Test/Year_of_test/Data_test.csv",sep=""),sep=";", header = TRUE, na.strings = "NA", skip=0, dec=".",strip.white = TRUE)
DateUTC<-ymd_hms(Data_test$Date)
Data_test<-cbind(DateUTC,subset(Data_test,select =c("Date_only","heure",Variable_names)))

if(n==1){
  Day_or_nigth_1.2<-day_and_nigth(1.60457,9.74484,Data_test)
  
}


# 
#
Data_test<-cbind(Day_or_nigth_1.2,Data_test)


Variable_names1<-str_replace(Variable_names, pattern = "Tsol_1_10", replacement = "Tsoil")
setwd(paste(path,"Tables/MDS_Test/Cycle_diurne_test/Output_of_test/",sep = ""))


simulation<-c(1:4)
simulation<-as.character(simulation)

month_num<-c(1:12)
Month_name<-as.character(month(month_num,label = TRUE,abbr = FALSE))
Month_nameID<-c("01","02","03","04","05","06","07","08","09","10","11","12")


Noon<-c("Nigth","Befor_mid","After_mid")
NoonID<-c("Ni","BeN","AfN")

N<-12

X<-NULL
for (m in 1:length(Noon)) {
  path1<-paste(path,"Tables/MDS_Test/Cycle_diurne_test/Output_of_test/",Noon[m],"/",sep = "")
  path_in<-paste(path1,"Output_by_PecentSimulation_mean/",sep = "")
  X_percent<-c(50,60,70)
  X_percent_suffix<-as.character(X_percent)
  Percent_file<-paste("Percent_",X_percent_suffix,sep="")
  
  for (t in 1:length(X_percent)) {
    
    Output_list_rmse<-list()
    Output_list_mean<-list()
    Output_list_BE<-list()
    Output_list_DSE<-list()
    Output_list_RSR<-list()
    Output_list_PBE<-list()
    
    for (i in 1:N) {
      Data_month<-read.csv(paste(path_in,Percent_file[t],"/",Sites[n],"_artificial_gap_",Month_name[i],"_",X_percent_suffix[t],"_mean_with_error.csv",sep = ""),sep = ";") 
      
      Output_var_rmse<-c()
      Output_var_BE<-c()
      Output_var_PBE<-c()
      Output_var_DSE<-c()
      Output_var_mean<-c()
      Output_var_RSR<-c()
      
      for (k in 1:length(Variable_names)) {
        Date <- Data_cyclediune_test[i,Variable_names[k]]
        Table_month <- filter(Data_month,Date_only==Date,Day_or_nigth_1.2==m-1)
        
        Output_var_rmse[k]<-rmse(Table_month[,Variable_names[k]],Table_month[,paste(Variable_names1[k],"_",X_percent_suffix[t],"_f",sep = "")])
        Output_var_BE[k]<-bias(Table_month[,Variable_names[k]],Table_month[,paste(Variable_names1[k],"_",X_percent_suffix[t],"_f",sep = "")])
        Output_var_mean[k]<-mean(Table_month[,Variable_names[k]])
        Output_var_RSR[k]<-rsr(Table_month[,paste(Variable_names1[k],"_",X_percent_suffix[t],"_f",sep = "")],Table_month[,Variable_names[k]])
        # 
        m1<-t(Table_month[,Variable_names[k]])
        # 
        m2<-t(Table_month[,paste(Variable_names1[k],"_",X_percent_suffix[t],"_f",sep = "")])
        Entropy<-EntropyExplorer(m1,m2,dmetric = "dse",otype = "v",shift = c("auto","auto"))
        # 
        Output_var_DSE[k]<-abs(Entropy[,3]) 
        Output_var_PBE[k]<-(Output_var_BE[k]/Output_var_mean[k])*100
        
      }
       
      Output_list_rmse[[i]]<-Output_var_rmse
      
      Output_list_BE[[i]]<-Output_var_BE
      Output_list_PBE[[i]]<-round(Output_var_PBE,1) 
      Output_list_DSE[[i]]<-Output_var_DSE
      Output_list_RSR[[i]]<-round(Output_var_RSR,2)
    }
    
    auxi<-max(sapply(Output_list_rmse,length))
    res<-sapply(Output_list_rmse,function(u) c(u,rep(NA,auxi-length(u))))
    res<-as.data.frame(res)
    colnames(res)=paste("RMSE_",X_percent_suffix[t],"_",NoonID[m],"_",Month_nameID,sep = "")         
    rownames(res)=Variable_names
    Output_frame_rmse<-res
    
    auxi<-max(sapply(Output_list_BE,length))
    res<-sapply(Output_list_BE,function(u) c(u,rep(NA,auxi-length(u))))
    res<-as.data.frame(res)
    colnames(res)=paste("BE_",X_percent_suffix[t],"_",NoonID[m],"_",Month_nameID,sep = "")         
    rownames(res)=Variable_names
    Output_frame_BE<-res
    
    auxi<-max(sapply(Output_list_PBE,length))
    res<-sapply(Output_list_PBE,function(u) c(u,rep(NA,auxi-length(u))))
    res<-as.data.frame(res)
    colnames(res)=paste("PBE_",X_percent_suffix[t],"_",NoonID[m],"_",Month_nameID,sep = "")         
    rownames(res)=Variable_names
    Output_frame_PBE<-res
    
    
    auxi<-max(sapply(Output_list_DSE,length))
    res<-sapply(Output_list_DSE,function(u) c(u,rep(NA,auxi-length(u))))
    res<-as.data.frame(res)
    colnames(res)=paste("DSE_",X_percent_suffix[t],"_",NoonID[m],"_",Month_nameID,sep = "")         
    rownames(res)=Variable_names
    Output_frame_DSE<-res
    
    
    auxi<-max(sapply(Output_list_RSR,length))
    res<-sapply(Output_list_RSR,function(u) c(u,rep(NA,auxi-length(u))))
    res<-as.data.frame(res)
    colnames(res)=paste("RSR_",X_percent_suffix[t],"_",NoonID[m],"_",Month_nameID,sep = "")         
    rownames(res)=Variable_names
    Output_frame_RSR<-res
    
    
    
    
    
    
    
    if(is.null(X)){
      X<-cbind(Output_frame_PBE,Output_frame_DSE,Output_frame_RSR)
      
    }
    else{
      X<-cbind(X,Output_frame_PBE,Output_frame_DSE,Output_frame_RSR)
    }
    
    
    
  }
  if(m==length(Noon)){
    beep(8)
  }
}

write.table(X,file=paste(path,"Tables/MDS_Test/Cycle_diurne_test/Output_of_test/",Sites[n],"_MDS_test_Metric_diurnal.csv",sep = ""),row.names = T,sep = ";")

#######################################################################################
#Evaluation extreme MDS
#######################################################################################

library(lubridate)
library(REddyProc)
library(dplyr)
library(stringr)
library(stringi)
library(beepr)


#Recherche des dates teste pour les p?riodes extremes 
Sites<-c("Nalohou")
SitesID<-c("Nal")

n<- 1

path<-paste("G:/DOSSIERS RENAUD/anon-ms-example/French_version/MDS_Article/","Brut/",sep="")
Dates_Begin<-as.character(010108)
Dates_end<-as.character(311208)
setwd(path)
DATA08<-subset(read.table(file=paste(SitesID[n],"_MET_0",Dates_Begin,"_",Dates_end,"X.csv",sep = "") ,sep=";", header = TRUE, na.strings = "NA", skip=0, dec=".",strip.white = TRUE), select=c("Rain"))

for (i in 1:length(DATA08$Rain)) {
  
  if(!is.na(DATA08$Rain[i])){
    if(DATA08$Rain[i]<0){
      DATA08$Rain[i]<-NA
    }
    
    
    
  }
  
}

path<-paste("G:/DOSSIERS RENAUD/anon-ms-example/French_version/MDS_Article/")
setwd(path)
source("G:/DOSSIERS RENAUD/anon-ms-example/French_version/MDS_Article/Codes/day_and_nigth2_demis.R")

Data_cyclediune_test<-read.csv(paste(path,"Tables/MDS_Test/Cycle_diurne_test/Day_of_test/Cycle_diurne_moyen_test.csv",sep=""),sep=";", header = TRUE, na.strings = "NA", skip=0, dec=".",strip.white = TRUE)
row.names(Data_cyclediune_test)<-NULL
Variable_names<-colnames(Data_cyclediune_test)



Data_test<-read.csv(paste(path,"Tables/MDS_Test/Year_of_test/Data_test.csv",sep=""),sep=";", header = TRUE, na.strings = "NA", skip=0, dec=".",strip.white = TRUE)
DateUTC<-ymd(Data_test$Date_only)
Mois<-month(DateUTC)


Data_test<-cbind(DateUTC,Mois,subset(Data_test,select =Variable_names),DATA08)

Saisons<-c()
x<-as.Date(as.Date("2008-05-01"):as.Date("2008-10-19"),origin="1970-01-01")
y<-as.Date(as.Date("2007-12-20"):as.Date("2008-02-23"),origin="1970-01-01")
z<-as.Date(as.Date("2008-12-27"):as.Date("2009-01-26"),origin="1970-01-01")
for (i in 1:length(Data_test$DateUTC)) {
  if(is.element(Data_test$DateUTC[i],x)){
    Saisons[i]<-1
  }
  else if(is.element(Data_test$DateUTC[i],y)){
    Saisons[i]<-0
  }
  else if(is.element(Data_test$DateUTC[i],z)){
    Saisons[i]<-0
  }
  else{
    Saisons[i]<-3
  }
}

Data_test<-cbind(Data_test,Saisons)

Data_humide<-filter(Data_test,Saisons==1)
Data_sec<-filter(Data_test,Saisons==0)
Data_transition<-filter(Data_test,Saisons==3)


Data_mean_humide<-aggregate(Data_humide[,-8], by=list(Data_humide$Mois), FUN=mean)
Data_sum_humide<-aggregate(Data_humide$Rain, by=list(Data_humide$Mois), FUN=sum,na.rm=TRUE)
Indice_humide<-which(Data_mean_humide$Tair==min(Data_mean_humide$Tair) ,arr.ind = T)

Data_mean_sec<-aggregate(Data_sec[,-8], by=list(Data_sec$Mois), FUN=mean)
Data_sum_sec<-aggregate(Data_sec$Rain, by=list(Data_sec$Mois), FUN=sum,na.rm=TRUE)
Indice_sec<-which(Data_mean_sec$Tair==max(Data_mean_sec$Tair) ,arr.ind = T)

Mois_humide<-Data_mean_humide$Mois[Indice_humide]
Mois_sec<-Data_mean_sec$Mois[Indice_sec]

Output_month_test<-c(Mois_humide,Mois_sec)
Output_month_test<-as.matrix(Output_month_test)
Output_month_test<-as.data.frame(t(Output_month_test))
colnames(Output_month_test)=c("Wet_Season","Dry_Season")
rownames(Output_month_test)="Month"

path_out<-paste(path,"Tables/MDS_Test/Season_test/Month_of_test/",sep = "")
write.table(Output_month_test,file = paste(path_out,"Monthly_of_season_test.csv",sep = ""),row.names = TRUE,sep = ";")
#############################################################################

#Chargement des librairies
library(lubridate)
library(REddyProc)
library(dplyr)
library(stringr)
library(stringi)
library(beepr)
#Choix du sites
Sites<-c("Nalohou")
SitesID<-c("Nal")


n<- 1

path<-paste("G:/DOSSIERS RENAUD/anon-ms-example/French_version/MDS_Article/")
setwd(path)

#Chargement de la table pour le cycle diurne test
Data_cyclediune_test<-read.csv(paste(path,"Tables/MDS_Test/Cycle_diurne_test/Day_of_test/Cycle_diurne_moyen_test.csv",sep=""),sep=";", header = TRUE, na.strings = "NA", skip=0, dec=".",strip.white = TRUE)
row.names(Data_cyclediune_test)<-NULL
Variable_names<-colnames(Data_cyclediune_test)

#Chargement des mois test
path_out<-paste(path,"/Tables/MDS_Test/Season_test/Month_of_test/",sep = "")
Output_month_test<-read.csv(paste(path_out,"Monthly_of_season_test.csv",sep = ""),sep=";", header = TRUE, na.strings = "NA", skip=0, dec=".",strip.white = TRUE)
row.names(Output_month_test)<-NULL


Data_test<-read.csv(paste(path,"Tables/MDS_Test/Year_of_test/Data_test.csv",sep=""),sep=";", header = TRUE, na.strings = "NA", skip=0, dec=".",strip.white = TRUE)
DateUTC<-ymd_hms(Data_test$Date)
Mois<-month(DateUTC)
Data_test<-cbind(DateUTC,Mois,subset(Data_test,select =c("Date_only","heure",Variable_names)))


setwd(paste(path,"Tables/MDS_Test/Season_test/Output_of_test/",sep = ""))#Chemin pour le prochain r?pertoire

Seas<-c("Wet_Season","Dry_Season")#Vecteur  de stockage des noms des diff?rent p?riode d'unn jours test
set.seed(0852)#Pour rendre reproductif ? l'avenir

for (m in 1:length(Seas)) {
  path1<-paste(path,"Tables/MDS_Test/Season_test/Output_of_test/",Seas[m],"/",sep = "")
  X_percent<-c(50,60,70)
  X_percent_suffix<-as.character(X_percent)#Chargement des diff?rent pourcentage
  Percent_file<-paste("Percent_",X_percent_suffix,sep="")#Nom des dossiers pour chaque pourcentage d'injection de gap par p?riode de la journ?e
  
  pb_a<-txtProgressBar(min = 1,max = length(X_percent),style = 3)# barre de progression de la compilation
  
  for (t in 1:length(X_percent)) {
    
    if(m==1){
      Indices<-which(Data_test$Mois==Output_month_test$Wet_Season,arr.ind = T)
    }
    if(m==2){
      Indices<-which(Data_test$Mois==Output_month_test$Dry_Season,arr.ind = T)
        
    }
    simulation<-c(1:4)#Nombre de simulation ? effectuer
    simulation<-as.character(simulation)
    
    XVal<-Data_test#Data sans les valeurs de la logne 1(00:00)
    for (j in 1:length(simulation)) {
      data_tenpercentnumber_day_1<-(length(Indices)*X_percent[t])/100
      del<-sort(sample(Indices,data_tenpercentnumber_day_1,replace = FALSE))
      
      #############################################
      # Artificial Gap filling#
      #############################################
      #setwd("/media/yaulande/Recreator/anon-ms-example/")
      
      
      ### Loading the required packages ==============================================
      
      # You might need to install the packages first. In that case use:
      # install.packages("packagename")
      library(openeddy)
      library(REddyProc)
      
      
      
      # Set filenames, paths and arguments
      siteyear <- SitesID[n] # used as output filename and as 'ID.s' in sEddyProc$new()
      Tstamp <- format(Sys.time(), "%Y-%m-%d") # Timestamp of the computation
      
      path_out <- paste(path1,"Output_by_gap_percentage/",Percent_file[t],"/",sep="")
      
      pkg_version <- packageVersion("REddyProc")
      
      if(n==1){
        input <- "G:/DOSSIERS RENAUD/anon-ms-example/French_version/MDS_Article/Tables/REddyProc_TableInput/DATA_test_REddyPro.csv"
        lat <- 9.74484# edit site latitude
        long <- 1.60457 # edit site longtitude
          
      }
      
      tz <- 1 # timezone
      # Include meteo variables that will be plotted, gap-filled and exported
      meteo <- c("Rg", "Tair", 'VPD')
      
      plot_to_console <- TRUE
      year <- 2008 # used when plotting to console (single year allowed)
      # Save plots as "png" (default) or "pdf"; NEEvsUStar plots are fixed to "pdf"
      plot_as <- "png" 
      
      ### Data preparation =============================================================
      
      # Load data with one header and one unit row from (tab-delimited) text file
      EddyData.F <- read_eddy(input, sep = ";")
      
      
      
      Eddy_var_name<-c("Rh","Rg","Tair","Tsoil","VPD")
      
      for (s in 1:length(Eddy_var_name)) {#Injection des gap al?atoire %t aux indices de del pour chaque variable et pour le mois i
        Q<-Eddy_var_name[s]
        EddyData.F[del,Q]<-NA
      }
      
      # Add time stamp in POSIX time format
      EddyDataWithPosix.F <- fConvertTimeToPosix(
        EddyData.F, 'YMDH', Year = 'Year', Day = 'Day', Hour = 'Hour',Month = 'Month')
      
      # Initalize R5 reference class sEddyProc for processing of eddy data
      # with all variables needed for processing later
      variables <- c("Rg","Tair","Tsoil","Rh","VPD")
      EddyProc.C <- sEddyProc$new(siteyear, EddyDataWithPosix.F, variables)
      EddyProc.C$sSetLocationInfo(lat, long, tz)  # site location info
      
      # See the content
      str(EddyProc.C)
      EddyProc.C$sPrintFrames(NumRows.i = 6L)
      
      # Meteo must be gap-filled even when without gaps to run the partitioning 
      for (met_var in variables) {#MDS Gap_filling
        EddyProc.C$sMDSGapFill(met_var, FillAll = TRUE,suffix = paste(X_percent_suffix[t],"_",simulation[j], sep = "") )
      }
      
      Gapvalfill<-EddyProc.C$sExportResults()
      FP_vars <- c(paste(variables,"_",X_percent_suffix[t],"_",simulation[j],"_f", sep = ""))
      FP_varsd <- c(paste(variables,"_",X_percent_suffix[t],"_",simulation[j],"_fsd", sep = ""))
      V_gap_101<-subset(Gapvalfill, select=FP_vars)#Recup?ration des colonne des variable gapfiller
      V_gap_101sd<-subset(Gapvalfill, select=FP_varsd)
      variables1<-c("Swin","Tair","Tsoil","HR","VPD")
      colnames(V_gap_101)=c(paste(variables1,"_",X_percent_suffix[t],"_",simulation[j],"_f", sep = ""))#Refoematage qu nom de base
      colnames(V_gap_101sd)=c(paste(variables1,"_",X_percent_suffix[t],"_",simulation[j],"_fsd", sep = ""))
      #Data_gapVal<-cbind(Data_gapVal,V_gap_101)
      XVal<-cbind(XVal,V_gap_101,V_gap_101sd)#Concatenation avec XVal
      
          remove(V_gap_101,V_gap_101sd,del,Gapvalfill)
    }
    write.table(XVal,file=paste(path_out,Sites[n],"_artificial_gap_",X_percent_suffix[t],".csv",sep = ""),row.names = FALSE,sep = ";")#Sauvegade dans la direction
    remove(XVal)
    
    if(t==length(X_percent)){#test
      beep(3)
    }
    
    Sys.sleep(60)
    setTxtProgressBar(pb_a,t)
    
    }
}

#######################################################################################
#Calcule des moyenne des simulations
#######################################################################################

library(lubridate)
library(REddyProc)
library(dplyr)
library(stringr)
library(stringi)
library(beepr)


Sites<-c("Nalohou")
SitesID<-c("Nal")


n<- 1
path<-paste("G:/DOSSIERS RENAUD/anon-ms-example/French_version/MDS_Article/")
setwd(path)


Data_cyclediune_test<-read.csv(paste(path,"Tables/MDS_Test/Cycle_diurne_test/Day_of_test/Cycle_diurne_moyen_test.csv",sep=""),sep=";", header = TRUE, na.strings = "NA", skip=0, dec=".",strip.white = TRUE)
row.names(Data_cyclediune_test)<-NULL
Variable_names<-colnames(Data_cyclediune_test)


Data_test<-read.csv(paste(path,"Tables/MDS_Test/Year_of_test/Data_test.csv",sep=""),sep=";", header = TRUE, na.strings = "NA", skip=0, dec=".",strip.white = TRUE)
DateUTC<-ymd(Data_test$Date_only)
Mois<-month(DateUTC)


Data_test<-cbind(DateUTC,Mois,subset(Data_test,select =Variable_names))



Variable_names<-str_replace(Variable_names, pattern = "Tsol_1_10", replacement = "Tsoil")
setwd(paste(path,"Tables/MDS_Test/Season_test/Output_of_test/",sep = ""))#Chemin pour le prochain r?pertoire


simulation<-c(1:4)
simulation<-as.character(simulation)


Seas<-c("Wet_Season","Dry_Season")#Vecteur  de stockage des noms des diff?rent p?riode d'unn jours test
for (m in 1:length(Seas)) {
  
  path1<-paste(path,"Tables/MDS_Test/Season_test/Output_of_test/",Seas[m],"/",sep = "")
  
  
  X_percent<-c(50,60,70)
  X_percent_suffix<-as.character(X_percent)
  Percent_file<-paste("Percent_",X_percent_suffix,sep="")
  
  pb_a<-txtProgressBar(min = 1,max = length(X_percent),style = 3)
  
  for (t in 1:length(X_percent)) {
    Output<-list()
    Output_Error<-list()
    path_in <- paste(path1,"Output_by_gap_percentage/",Percent_file[t],"/",sep="")
    path_out <- paste(path1,"Output_by_PecentSimulation_mean/",Percent_file[t],"/",sep="")
    
    XVal<-Data_test
    X<-read.csv(paste(path_in,Sites[n],"_artificial_gap_",X_percent_suffix[t],".csv",sep = ""),sep = ";")
    
    for (k in 1:length(Variable_names)) {
      FP_vars<-paste(Variable_names[k],"_",X_percent_suffix[t],"_",simulation,"_f",sep="")
      X_sub<-subset(X, select=FP_vars)
      X_sub_vect<-c()
      Error_vec<-c()
      for (p in 1:nrow(X_sub)) {
        vect<-unname(unlist(X_sub[p,]))
        le<-length(simulation)
        X_sub_vect[p]<-mean(vect)
        Error_vec[p]<-round(qt(0.975,df=le-1)*sqrt(var(vect))/sqrt(le),3)  
        
      }
      
      
      Output[[k]]<-X_sub_vect
      Output_Error[[k]]<-Error_vec
      
    }    
    
  auxi<-max(sapply(Output,length))
  res<-sapply(Output,function(u) c(u,rep(NA,auxi-length(u))))
  res<-as.data.frame(res)
  colnames(res)=paste(Variable_names,"_",X_percent_suffix[t],"_f",sep="")
  
  auxi1<-max(sapply(Output_Error,length))
  res1<-sapply(Output_Error,function(u) c(u,rep(NA,auxi-length(u))))
  res1<-as.data.frame(res1)
  colnames(res1)=paste(Variable_names,"_",X_percent_suffix[t],"_error_f",sep="")
  
  
  Output_1<-cbind(XVal,res,res1)
  
    write.table(Output_1,file=paste(path_out,Sites[n],"_artificial_gap_",X_percent_suffix[t],"_mean.csv",sep = ""),row.names = FALSE,sep = ";")
    remove(XVal)
    
    
    
    if(t==length(X_percent)){
      beep(3)
    }
    
    
    setTxtProgressBar(pb_a,t)
    
    
  }    
  
  if(m==length(Seas)){
    beep(4)
  }
  
}

#######################################################################################
#Calcule des Metrique
#######################################################################################
rm(list = ls(all=TRUE))
ls()
getwd()

library(lubridate)
library(REddyProc)
library(dplyr)
library(stringr)
library(stringi)
library(beepr)
library(Metrics)
library(EntropyExplorer)
library(hydroGOF)

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

if(n==1){
  Day_or_nigth_1.2<-day_and_nigth(1.60457,9.74484,Data_test)
  
}




Season_test<-read.csv(paste(path,"Tables/MDS_Test/Season_test/Month_of_test/Monthly_of_season_test.csv",sep=""),sep=";", header = TRUE, na.strings = "NA", skip=0, dec=".",strip.white = TRUE)



Data_test<-cbind(Day_or_nigth_1.2,Data_test)


Variable_names1<-str_replace(Variable_names, pattern = "Tsol_1_10", replacement = "Tsoil")
setwd(paste(path,"Tables/MDS_Test/Cycle_diurne_test/Output_of_test/",sep = ""))


simulation<-c(1:4)
simulation<-as.character(simulation)

month_num<-c(1:12)
Month_name<-as.character(month(month_num,label = TRUE,abbr = FALSE))
Month_nameID<-c("01","02","03","04","05","06","07","08","09","10","11","12")

Seas<-c("Wet_Season","Dry_Season")
SeasID<-c("Wet","Dry")



N<-12

X<-NULL
for (m in 1:length(Seas)) {
  path1<-paste(path,"Tables/MDS_Test/Season_test/Output_of_test/",Seas[m],"/",sep = "")
  path_in<-paste(path1,"Output_by_PecentSimulation_mean/",sep = "")
  X_percent<-c(50,60,70)
  X_percent_suffix<-as.character(X_percent)
  Percent_file<-paste("Percent_",X_percent_suffix,sep="")
  
  for (t in 1:length(X_percent)) {
    
    Output_list_rmse<-list()
    Output_list_BE<-list()
    Output_list_DSE<-list()
    Output_list_RSR<-list()
    Output_list_PBE<-list()
    
    
    Data_month<-read.csv(paste(path_in,Percent_file[t],"/",Sites[n],"_artificial_gap_",X_percent_suffix[t],"_mean.csv",sep = ""),sep = ";") 
    Data_month$DateUTC<- ymd(Data_month$DateUTC)
    Output_var_rmse<-c()
    Output_var_BE<-c()
    Output_var_PBE<-c()
    Output_var_DSE<-c()
    Output_var_mean<-c()
    Output_var_RSR<-c()
    for (k in 1:length(Variable_names)) {
      Month <- Season_test[1,Seas[m]]
      Table_month <- filter(Data_month,month(DateUTC)==Month)
      
      Output_var_rmse[k]<-rmse(Table_month[,Variable_names[k]],Table_month[,paste(Variable_names1[k],"_",X_percent_suffix[t],"_f",sep = "")])
      Output_var_BE[k]<-bias(Table_month[,Variable_names[k]],Table_month[,paste(Variable_names1[k],"_",X_percent_suffix[t],"_f",sep = "")])
      Output_var_mean[k]<-mean(Table_month[,Variable_names[k]])
      Output_var_RSR[k]<-rsr(Table_month[,paste(Variable_names1[k],"_",X_percent_suffix[t],"_f",sep = "")],Table_month[,Variable_names[k]])
      # 
      m1<-t(Table_month[,Variable_names[k]])
      # 
      m2<-t(Table_month[,paste(Variable_names1[k],"_",X_percent_suffix[t],"_f",sep = "")])
      Entropy<-EntropyExplorer(m1,m2,dmetric = "dse",otype = "v",shift = c("auto","auto"))
      # 
      Output_var_DSE[k]<-abs(Entropy[,3]) 
      Output_var_PBE[k]<-(Output_var_BE[k]/Output_var_mean[k])*100
      
      # 
      
    }
    
    
    Output_list_rmse<-Output_var_rmse
    Output_list_BE<-Output_var_BE
    Output_list_PBE<-round(Output_var_PBE,2)
    Output_list_DSE<-Output_var_DSE
    Output_list_RSR<-round(Output_var_RSR,2)
    
    auxi<-max(sapply(Output_list_rmse,length))
    res<-sapply(Output_list_rmse,function(u) c(u,rep(NA,auxi-length(u))))
    res<-as.data.frame(res)
    colnames(res)=paste("RMSE_",X_percent_suffix[t],"_",SeasID[m],sep = "")         
    rownames(res)=Variable_names
    Output_frame_rmse<-res
    
    
    
    auxi<-max(sapply(Output_list_BE,length))
    res<-sapply(Output_list_BE,function(u) c(u,rep(NA,auxi-length(u))))
    res<-as.data.frame(res)
    colnames(res)=paste("BE_",X_percent_suffix[t],"_",SeasID[m],sep = "")         
    rownames(res)=Variable_names
    Output_frame_BE<-res
    
    auxi<-max(sapply(Output_list_PBE,length))
    res<-sapply(Output_list_PBE,function(u) c(u,rep(NA,auxi-length(u))))
    res<-as.data.frame(res)
    colnames(res)=paste("PBE_",X_percent_suffix[t],"_",SeasID[m],sep = "")         
    rownames(res)=Variable_names
    Output_frame_PBE<-res
    
    
    auxi<-max(sapply(Output_list_DSE,length))
    res<-sapply(Output_list_DSE,function(u) c(u,rep(NA,auxi-length(u))))
    res<-as.data.frame(res)
    colnames(res)=paste("DSE_",X_percent_suffix[t],"_",SeasID[m],sep = "")         
    rownames(res)=Variable_names
    Output_frame_DSE<-res
    
    
    
    
    
    auxi<-max(sapply(Output_list_RSR,length))
    res<-sapply(Output_list_RSR,function(u) c(u,rep(NA,auxi-length(u))))
    res<-as.data.frame(res)
    colnames(res)=paste("RSR_",X_percent_suffix[t],"_",SeasID[m],sep = "")         
    rownames(res)=Variable_names
    Output_frame_RSR<-res
    
    
    if(is.null(X)){
      X<-cbind(Output_frame_PBE,Output_frame_DSE,Output_frame_RSR)
      
    }
    else{
      X<-cbind(X,Output_frame_PBE,Output_frame_DSE,Output_frame_RSR)
    }
    
    
    
  }
  if(m==length(Seas)){
    beep(8)
  }
}

write.table(X,file=paste(path,"Tables/MDS_Test/Season_test/Output_of_test/",Sites[n],"_MDS_test_Metric_Month.csv",sep = ""),row.names = T,sep = ";")





































