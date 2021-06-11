#### MAIN FILE FOR SPACE FORTRESS DATA EXTRACTION AND BASIC VISUALITION
#####
source("Clean\\Clean_SF.R")
#librairies
library("stringr")
library("foreach")
library("bestNormalize")
library("SciViews")
library("ggplot2")
library("ggpubr")
library("dplyr")
library("broom")

#####
b_Clean=winDialog(type="yesno","Are the raw files already cleaned?")
if(b_Clean=="NO"){
  #DATA CLEANING
  path=choose.dir(default = "", caption = "Choose the participants logs Folder")#Path with the Raw files
  fil=list.files(path=path,recursive = T) # files pattern : "^SpaceFortress-5.1.0(.*).txt$")
  path_clean=choose.dir(default = "", caption = "Choose the Clean Data Folder")#Path For the Clean Data
  #Clean File with points writing
  invisible(lapply(fil,write_file,path=path,path_clean=path_clean)) #launch the cleaning
}else{
  # path_clean=choose.dir(default = "", caption = "Choose the Clean Data Folder")#Path For the Clean Data
  path_clean="E:\\ISAE-2021\\Alldata\\Data_clean\\"
  
}
#CLEAN DATA READING
fil_clean=list.files(path=path_clean,recursive = T) #load the clean files
data=read_final_Score(fil_clean,detailed = FALSE) #Create the data 
#df_GROUPS=read.table(choose.files(default = "", caption = "Select the GROUP.txt file"),header=TRUE)#Choose the Group txt file
df_GROUPS=read.table("E:\\ISAE-2021\\Alldata\\GROUPS.txt",header=TRUE)
for(str_pseudo in unique(data$Pseudo)){
  data$Treatment[data$Pseudo==str_pseudo]=as.numeric(df_GROUPS$Treatment[df_GROUPS$Pseudo==str_pseudo])
}
data$Pseudo[data$Pseudo=="SL2804"]="SL0804"

#Add Demographic information
#df_demographique=read.csv(choose.files(default = "", caption = "Select the Demographic.csv file"),head=TRUE,dec = ",",sep=";")
df_demographique=read.csv("E:\\demographicsbrut.csv",head=TRUE,dec = ",",sep=";")

names(df_demographique)[names(df_demographique)=="Votre.âge"]="Age"
data_long=demographie_long(data,df_demographique)#LONG FORMAT of the data with dem info
data_wide=demographie(data,df_demographique,ZMean=FALSE,Delta=TRUE) #WIDE FORMAT of the data with dem info

#OUTLIERS : LM2411 & EC1603 & TB0301
data_long=subset(data_long,Pseudo!="LM2411"&Pseudo!="EC1603"&Pseudo!="TB0301")#outliers on long format
data_wide=subset(data_wide,Pseudo!="LM2411"&Pseudo!="EC1603"&Pseudo!="TB0301")#outliers on wide format

#####
#ZSCORES
#data_long=ZScores(data_long)
#data_wide=demographie(data_long,df_demographique,ZMean=TRUE) #WIDE FORMAT of the data with dem info WITH ZMEAN : to do after Zmean addition with ZScores(data_long)
#data_wide=subset(data_wide,Pseudo!="LM2411"&Pseudo!="EC1603"&Pseudo!="TB0301")#outliers on wide format

#Women "Outliers" in sham group
data_wide=subset(data_wide,Pseudo!="CP1809"&Pseudo!="MM0301"&Pseudo!="SP0801"&Pseudo!="CH0205") 
data_long=subset(data_long,Pseudo!="CP1809"&Pseudo!="MM0301"&Pseudo!="SP0801"&Pseudo!="CH0205")
#####
#LearningRate
data_wide=LearningRate(data_long,data_wide,ZM=FALSE)
data_wide=LearningRate(data_long,data_wide,TRUE,ZM=FALSE)
#data_wide=LearningRate(data_long,data_wide,ZM=TRUE)
#data_wide=LearningRate(data_long,data_wide,TRUE,ZM=TRUE)

#LOG GameLevel
data_wide$GameLevelLog=log(data_wide$GameLevel+1)
