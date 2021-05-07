#CLEAN_SF MAIN
source("Clean\\Clean_SF.R")
library("stringr")
library("foreach")
#Path choice
path=choose.dir(default = "", caption = "Participants file")#ALL FILES non clean files path
fil=list.files(path=path,recursive = T) # files pattern : "^SpaceFortress-5.1.0(.*).txt$")
path_clean="E:\\ISAE-2021\\Alldata\\Data_Clean_NEW"#CLEANFILES
#Clean File with points writing
invisible(lapply(fil,write_file,path=path,path_clean=path_clean))

#CLEAN FILES : compute scores/min and APM

fil_clean=list.files(path=path_clean,recursive = T)
path_ScM_APM="E:\\ISAE-2021\\Alldata\\ScM_SubScores_APM\\"

#df_APM_ScM=lapply(fil_clean,scores_apm_fct)
#write_APM_ScM(fil_clean) #Matrix: APM and Scores per Minute of Participants, for each session
fil_APM_ScM=list.files(path=path_ScM_APM,recursive = T)
df_APM_ScM=read_APM_ScM(fil_APM_ScM)
df_APM_ScM=subset(df_APM_ScM,Pseudo!="EC1603"&Pseudo!="LM2411")
#df_APM_ScM$RollSD=lapply(df_APM_ScM$ScoresMin,moving_sd_ttr, mov_point=60)

conc_df=concatenate(df_APM_ScM)

df_GROUPS=read.table("E:\\ISAE-2021\\Alldata\\GROUPS.txt",header=TRUE)
for(str_pseudo in unique(df_APM_ScM$Pseudo)){
  df_APM_ScM$Treatment[df_APM_ScM$Pseudo==str_pseudo]=as.numeric(df_GROUPS$Treatment[df_GROUPS$Pseudo==str_pseudo])
}
 for(str_pseudo in unique(df_demographique$identifiant)){
   df_demographique$Treatment[df_demographique$identifiant==str_pseudo]=as.numeric(df_GROUPS$Treatment[df_GROUPS$Pseudo==str_pseudo])
 }

# #Data scores cumul?s
#df_data=read_data_score(fil_clean)
# 
# df_data$SD=sapply(df_data$CumulScore,sd)
# df_data$SDPoint=sapply(df_data$Point,sd)
# df_data$SDFlight=sapply(df_data$FlightPoint,sd)
# df_data$SDBonus=sapply(df_data$BonusPoint,sd)
# df_data$SDMine=sapply(df_data$MinePoint,sd)
# df_data$SDFortress=sapply(df_data$FortressPoint,sd)
# 
#General data
gen_data=subset(df_APM_ScM,select=c(Date,Session,Pseudo,Treatment,TotalScore,Flight,Bonus,Mine,Fortress))
gen_data=subset(gen_data,Pseudo!="EC1603"&Pseudo!="LM2411")

#Add Day on gen_data
gen_data$Day=substring(gen_data$Session,1,3)

#add Zscore
#Zscore on all session all days
z_score=scale(gen_data$TotalScore)
gen_data$Zscore=z_score

gen_data_P2=subset(gen_data,grepl("P2",Session)|Session=="D01P1")

final_df=read_final_Score(fil_clean)
