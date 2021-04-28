library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggsci)
#T-tests
gen_data=subset(df_APM_ScM,select=c(Date,Session,Pseudo,Treatment,TotalScore,Flight,Bonus,Mine,Fortress))

score_names=c("TotalScore","Flight","Bonus","Mine","Fortress")

t_test_AllScore_file="D:\\ISAE-2021\\STATS\\t_test_J1__All_Scores_nooutliers.txt"

gen_data_J1=subset(gen_data,Session=="D01P1")
gen_data_J14P2=subset(gen_data,Session=="D14P2")
for(n in score_names){
  t_test=t.test(gen_data_J1[,n]~gen_data_J1$Treatment)
  t_test$data.name=paste("J1",n,"~ Group")
  capture.output(t_test,file=t_test_AllScore_file,append=TRUE)
}

bxp_TotalScore=ggboxplot(gen_data_J1,x="Treatment",y="TotalScore",color="Treatment",palette="jco",add="jitter")+labs(x="Group",y="TotalScore",title="Total Score")+stat_compare_means(method="t.test",label.x = 1.35, label.y = 0.9*max(gen_data_J1$TotalScore))+rremove("legend")
bxp_Flight=ggboxplot(gen_data_J1,x="Treatment",y="Flight",color="Treatment",palette="jco",add="jitter")+labs(x="Group",y="Flight",title="Flight")+stat_compare_means(method="t.test",label.x = 1.35, label.y = max(gen_data_J1$Flight)-120)+rremove("legend")
bxp_Bonus=ggboxplot(gen_data_J1,x="Treatment",y="Bonus",color="Treatment",palette="jco",add="jitter")+labs(x="Group",y="Bonus",title="Bonus")+stat_compare_means(method="t.test",label.x = 1.35, label.y = 0.9*max(gen_data_J1$Bonus))+rremove("legend")
bxp_Mine=ggboxplot(gen_data_J1,x="Treatment",y="Mine",color="Treatment",palette="jco",add="jitter")+labs(x="Group",y="Mine",title="Mine")+stat_compare_means(method="t.test",label.x = 1.35, label.y = 0.9*max(gen_data_J1$Mine))+rremove("legend")
bxp_Fortress=ggboxplot(gen_data_J1,x="Treatment",y="Fortress",color="Treatment",palette="jco",add="jitter")+labs(x="Group",y="Fortress",title="Fortress")+stat_compare_means(method="t.test",label.x = 1.35, label.y = 0.9*max(gen_data_J1$Fortress))+rremove("legend")

ggarrange(bxp_TotalScore,ggarrange(bxp_Flight,bxp_Bonus,bxp_Mine,bxp_Fortress,ncol=2,nrow=2,labels=c("B","C","D","E")),nrow=2,heights = c(2,5),labels = "A")

# T test on demographics between groups (sham vs stim)
df_demographique <- read.csv("E:\\ISAE-2021\\Alldata\\2021_04_01_RawData_tRNS_study.csv", sep = ";", dec=',',header=TRUE)
df_demographique<-na.omit(df_demographique)

df_demographique$TotalScore=as.numeric(lapply(df_demographique$identifiant,function(x){gen_data_J1$TotalScore[gen_data_J1$Pseudo==x]}))

df_demographique$Score_D14P02=as.numeric(lapply(df_demographique$identifiant,function(x){gen_data_J14P2$TotalScore[gen_data_J1$Pseudo==x]}))
df_demographique=subset(df_demographique,identifiant!="EC1603"&identifiant!="LM2411")
t_test_Dem_file="D:\\ISAE-2021\\STATS\\t_test_Demographics_nooutliers.txt"
for(n in c("NET","sum_JV","Votre.fge","TotalScore","Score_D14P02")){
  t_test=t.test(df_demographique[,n]~df_demographique$Group)
  #t_test=t.test(n ~ Group, data=df_demographique)
  t_test$data.name=paste(n,"~ Group")
  capture.output(t_test,file=t_test_Dem_file,append=TRUE)
  }
chi_test=chisq.test(df_demographique$Votre.genre,df_demographique$Group)
capture.output(chi_test,file=t_test_Dem_file,append=TRUE)

bxp_NET=ggboxplot(df_demographique,x="Treatment",y="NET",color="Treatment",palette="jco",add="jitter")+labs(x="Group",y="NET",title="NET")+stat_compare_means(method="t.test",label.x = 1.35, label.y = 0.9*max(df_demographique$NET))+rremove("legend")
bxp_GameLevel=ggboxplot(df_demographique,x="Treatment",y="sum_JV",color="Treatment",palette="jco",add="jitter")+labs(x="Group",y="GameLevel",title="Game Level")+stat_compare_means(method="t.test",label.x=1.35,label.y=0.7*max(df_demographique$sum_JV))+rremove("legend")
bxp_Age=ggboxplot(df_demographique,x="Treatment",y="Votre.fge",color="Treatment",palette="jco",add="jitter")+labs(x="Group",y="Age",title="Age")+stat_compare_means(method="t.test",label.x = 1.35, label.y = 0.9*max(df_demographique$Votre.fge))+rremove("legend")
bxp_ScoreJ1=ggboxplot(df_demographique,x="Treatment",y="TotalScore",color="Treatment",palette="jco",add="jitter")+labs(x="Group",y="Score D1",title="First Score")+stat_compare_means(method="t.test",label.x = 1.35, label.y = 0.9*max(df_demographique$TotalScore))+rremove("legend")
bxp_ScoreJ6=ggboxplot(df_demographique,x="Treatment",y="Score_D14P02",color="Treatment",palette="jco",add="jitter")+labs(x="Group",y="Score D14P2",title="Last Score")+stat_compare_means(method="t.test",label.x = 1.35, label.y = 0.9*max(df_demographique$Score_D14P02))+rremove("legend")

bxp_Gender=ggplot(df_demographique,aes(as.factor(Treatment),group=as.factor(Votre.genre),fill=as.factor(Votre.genre)))+geom_bar(width=0.25)+theme_classic2()+theme(legend.title = element_blank(),legend.key.size =unit(0.1,"cm"))+labs(x="Gender",y=" ",title="Gender")+scale_fill_jco()
bxp_Gender

ggarrange(bxp_NET,bxp_GameLevel,bxp_ScoreJ1,bxp_ScoreJ6,bxp_Age,bxp_Gender,ncol=2,nrow=3)

