#### MAIN FILE FOR SPACE FORTRESS DATA EXTRACTION AND BASIC VISUALITION

#####
#librairies
source("Clean\\Clean_SF.R")
library("stringr")
library("foreach")
library("bestNormalize")
library("SciViews")
library("ggplot2")
library("ggpubr")
library("dplyr")
library("broom")

#####
#DATA CLEANING
path=choose.dir(default = "", caption = "Participants files")#Path with the Raw files
fil=list.files(path=path,recursive = T) # files pattern : "^SpaceFortress-5.1.0(.*).txt$")
path_clean=choose.dir(default = "", caption = "Clean Folder")#Path For the Clean Data
#Clean File with points writing
invisible(lapply(fil,write_file,path=path,path_clean=path_clean)) #launch the cleaning
fil_clean=list.files(path=path_clean,recursive = T) #load the clean files
#DATA Creation
data=read_final_Score(fil_clean) #Create the data 
df_GROUPS=read.table(choose.files(default = "", caption = "GROUP file"),header=TRUE)#Choose the Group txt file
for(str_pseudo in unique(data$Pseudo)){
  data$Treatment[data$Pseudo==str_pseudo]=as.numeric(df_GROUPS$Treatment[df_GROUPS$Pseudo==str_pseudo])
}
data$Pseudo[data$Pseudo=="SL2804"]="SL0804"
df_demographique$identifiant[df_demographique$identifiant=="SL2804"]="SL0804"

#Add Demographic information
df_demographique=read.csv(choose.files(default = "", caption = "Demographic file"),head=TRUE,dec = ",",sep=";")
data_long=demographie_long(data,df_demographique)#LONG FORMAT of the data with dem info
data_wide=demographie(data,df_demographique,ZMean=FALSE) #WIDE FORMAT of the data with dem info
#data_wide=demographie(data_long,df_demographique,ZMean=TRUE) #WIDE FORMAT of the data with dem info WITH ZMEAN : to do after Zmean addition

#OUTLIERS : LM2411 & EC1603 & TB0301
data_long=subset(data_long,Pseudo!="LM2411"&Pseudo!="EC1603"&Pseudo!="TB0301")#outliers on long format
data_wide=subset(data_wide,Pseudo!="LM2411"&Pseudo!="EC1603"&Pseudo!="TB0301")#outliers on wide format

#####
#ZSCORES
data_long$ZScore=scale(data_long$TotalScore)
#1 Yeo johnson transformation
data_long$Flight_YeoJ=yeojohnson(data_long$Flight)$x.t
data_long$Bonus_YeoJ=yeojohnson(data_long$Bonus)$x.t
data_long$Mine_YeoJ=yeojohnson(data_long$Mine,standardize = TRUE)$x.t
data_long$Fortress_YeoJ=yeojohnson(data_long$Fortress,standardize = TRUE)$x.t
#2ZSousScores
data_long$ZFlight=scale(data_long$Flight_YeoJ)
data_long$ZMine=scale(data_long$Mine_YeoJ)
data_long$ZBonus=scale(data_long$Bonus_YeoJ)
data_long$ZFortress=scale(data_long$Fortress_YeoJ)
#3 ZMean ( mean of the Sub ZScores)
data_long$ZMean=rowMeans(subset(data_long,select=c("ZMine","ZFortress","ZBonus","ZFlight")))
