#Z score on total Score
library("ggplot2")
library("ggpubr")
library("dplyr")
library("tidyr")
library("maditr")
#Zscore on all session all days
z_score=scale(gen_data$TotalScore)
gen_data$Zscore=z_score


#dataframe with J0 (baseline day) J05P2 and J14P2
data_end_score= subset(gen_data,Session=='D01P1'|Session=="D05P2"|Session=="D14P2",select=c("Pseudo","Treatment","Session","Zscore"))
data_end_score$Pseudo=as.factor(data_end_score$Pseudo)
data_end_score$Session=as.factor(data_end_score$Session)
data_end_score$Treatment=as.factor(data_end_score$Treatment)

dt_zscore=dcast(data_end_score,Treatment+Pseudo~Session,value.var ="Zscore")
gen_data_P2_unmelt=dcast(gen_data_P2,Treatment+Pseudo~Session,value.var="Zscore")
colnames(dt_zscore)=c("Group","Pseudo","Day1","Day5","Day14")
write.csv(dt_zscore,"E:\\ISAE-2021\\STATS\\Z-scoresJ1J5J14.csv")#,header=TRUE,sep="\t",quote=FALSE)

#ANOVA ON Zscores J1 J5 and J14 (P2) between groups
library(ez)
ezanova_zscore=ezANOVA(data=data_end_score,dv=Zscore,wid=Pseudo,within=Session,between=Treatment)
capture.output(ezanova_zscore,file="E:\\ISAE-2021\\STATS\\ANOVAJ1J5J14_Zscore.txt")

#ANOVA ON Zscores J1 J5 and J14 between groups
data_short_term= subset(gen_data,Session!='D14P1'&Session!="D14P2",select=c("Pseudo","Treatment","Session","Zscore"))
data_short_term$Pseudo=as.factor(data_short_term$Pseudo)
data_short_term$Session=as.factor(data_short_term$Session)
data_short_term$Treatment=as.factor(data_short_term$Treatment)
ezanova_zscore_alldays=ezANOVA(data=data_short_term,dv=Zscore,wid=Pseudo,within=Session,between=Treatment)
capture.output(ezanova_zscore_alldays,file="E:\\ISAE-2021\\STATS\\ANOVAShortTerm_Zscore.txt")


#Zscore Table
Zscore_mean_sd_table = gen_data_P2%>%
  group_by(Treatment,Day)%>%
  summarize(Mean= round(mean(Zscore),4),SD=round(sd(Zscore),4))

Zscore_table=pivot_wider(Zscore_mean_sd_table, names_from = Day, values_from = c(Mean,SD)) 
Zscore_table
write.csv(Zscore_table,"E:\\ISAE-2021\\STATS\\Zscore_table_P2.csv",quote=FALSE,row.names = FALSE)



#WITH BASELINE DIFFERENCE
gen_data$BaselineZscore=NA
for(str in unique(gen_data$Pseudo)){
  gen_data$BaselineZscore[gen_data$Pseudo==str]=gen_data$Zscore[gen_data$Pseudo==str]-gen_data$Zscore[gen_data$Pseudo==str & gen_data$Day=="D01"]
}
gen_data_P2_Baseline=subset(gen_data,grepl("P2",Session))

#ANOVA ON Zscores J1 J5 and J14 (P2) between groups
library(ez)

gen_data_P2_Baseline_end=subset(gen_data_P2_Baseline,Day=="D02"|Day=="D05"|Day=="D14",select=c("Pseudo","Treatment","Day","BaselineZscore"))

ezanova_zscore=ezANOVA(data=gen_data_P2_Baseline_end,dv=BaselineZscore,wid=Pseudo,within=Day,between=Treatment)
capture.output(ezanova_zscore,file="E:\\ISAE-2021\\STATS\\ANOVAJ1J5J14_BaselineZscore.txt")

#ANOVA ON Zscores J1 J5 and J14 between groups
gen_data_P2_Baseline_short= subset(gen_data_P2_Baseline,Day!='D14'&Day!="D01",select=c("Pseudo","Treatment","Day","BaselineZscore"))

ezanova_zscore_alldays=ezANOVA(data=gen_data_P2_Baseline_short,dv=BaselineZscore,wid=Pseudo,within=Day,between=Treatment)
capture.output(ezanova_zscore_alldays,file="E:\\ISAE-2021\\STATS\\ANOVAShortTerm_BaselineZscore.txt")


###
#Mean of each day (session 2)
mean_sess2=apply(gen_data_P2_unmelt[,c(-1,-2)],2,mean)
summary(lm(formula=mean_sess2~log(1:6)))
plot(mean_sess2,x)
