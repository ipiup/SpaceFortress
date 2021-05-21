#### MAIN FILE FOR SPACE FORTRESS DATA EXTRACTION AND BASIC VISUALITION

#####
#librairies
source("Clean\\Clean_SF.R")
library("stringr")
library("foreach")

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
#Add Demographic information
df_demographique=read.csv(choose.files(default = "", caption = "Demographic file"),head=TRUE,dec = ",",sep=";")
data=demographie_long(data,df_demographique)#LONG FORMAT of the data with dem info
data_wide=demographie(data() ,df_demographique,ZMean=FALSE) #WIDE FORMAT of the data with dem info
data_wide=subset(data_wide,Pseudo!="LM2411")#Pseudo!="EC1603"&&Pseudo!="JT0601")#,select=c(Date,Session,Pseudo,Treatment,TotalScore,Flight,Bonus,Mine,Fortress))

#OUTLIERS
data=subset(data,Pseudo!="LM2411")#Pseudo!="EC1603"&&Pseudo!="JT0601")
data_wide=subset(data_wide,Pseudo!="LM2411")#outliers on wide format

#####
#VISUALISATION
