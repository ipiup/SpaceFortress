#CLEAN_SF MAIN
source("fNIrS\\fNIRS_Clean.R")
library("stringr")
library("foreach")
library("doParallel")
registerDoParallel(cores=4)
library("ggplot2")
library("reshape2")
library("ggpubr")
library("wesanderson")
library("bestNormalize")

#Path choice
path=choose.dir(default = "", caption = "Participants file")#ALL FILES non clean files path
fil=list.files(path=path,recursive = T) # files pattern : "^SpaceFortress-5.1.0(.*).txt$")
path_clean=choose.dir(default = "", caption = "Participants file")#Clean File with points writing
invisible(lapply(fil,write_file))

#Read new clean files
fil_clean=list.files(path=path_clean,recursive = T)

#####
df_data=read_final_Score(fil_clean,path_clean)
#ZSUBSCORES with YEo transformation
df_data$ZScore=scale(df_data$TotalScore)
df_data=ZScores_Yeo(df_data)

write.csv(df_data,"fFNIRSdata.csv")

#####
#APM and ScM
path_ScM_APM="E:\\ISAE-2021\\fNIRS\\fNIRs_APM\\" 
write_APM_ScM(fil_clean) #write
fil_APM_ScM=list.files(path=path_ScM_APM,recursive = T)
df_APM_ScM=read_APM_ScM(fil_APM_ScM)#read

#Add Mean APM to df_data
df_data$MeanAPM=df_APM_ScM$MeanAPM

#PLOTS
for(str_pseudo in unique(df_APM_ScM$Pseudo)){
  ggsave(paste("E:\\ISAE-2021\\fNIRS\\Plots\\",str_pseudo,"_APM_ScM.pdf"),APM_ScM_plot(str_pseudo,df_APM_ScM),width=15,height=8)
}

#Autres trucs
write.csv(df_data,"C:\\Users\\q.chenot\\Google Drive\\Thèse\\MANIPS\\Manip1_NIRSCOUT_SF\\V6_2021\\rawdata_SF&questionnaires\\SF_Scores.csv",row.names = FALSE)

df_data_multi=foreach(i=1:nrow(df_data),.combine=rbind)%do%{
  d=NULL
  if(!(df_data$Bonus[i]==0)&!(df_data$Mine[i]==0)){
    d=df_data[i,]
  }
  d
}
write.csv(df_data_multi,"C:\\Users\\q.chenot\\Google Drive\\Thèse\\MANIPS\\Manip1_NIRSCOUT_SF\\V6_2021\\rawdata_SF&questionnaires\\SF_Scores_Multi.csv",row.names = FALSE,quote=FALSE)

