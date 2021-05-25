library(ggplot2)
library(ggpubr)
library(dplyr)
library(rstatix)
library(ggsci)
#FINAL STATS ON 3 GROUPS 

#1.Descriptives STAT
#1.1 Outliers Detection

ggplot(data_long,aes(Session, TotalScore,color=as.factor(Treatment)))+geom_boxplot()+theme_classic2()+labs(color = "Group")

outliers=data_long%>%
  group_by(Session)%>%
  identify_outliers(TotalScore)

##Sont donc extraits : EC1603 et TB0301

#1.2 Données démographiques
data_wide$Genre=as.factor(data_wide$Genre)

bxp_NET=ggboxplot(data_wide,x="Group",y="NET",color="Group",palette="jco",add="jitter")+labs(x="Groups",y="NET",title="NET")+rremove("legend")
bxp_GameLevel=ggboxplot(data_wide,x="Group",y="GameLevel",color="Group",palette="jco",add="jitter")+labs(x="Groups",y="GameLevel",title="Game Level")+rremove("legend")
bxp_Age=ggboxplot(data_wide,x="Group",y="Age",color="Group",palette="jco",add="jitter")+labs(x="Group",y="Age",title="Age")+rremove("legend")
bxp_ScoreJ1=ggboxplot(data_wide,x="Group",y="D01P1",color="Group",palette="jco",add="jitter")+labs(x="Groups",y="Score D1",title="First Score")+rremove("legend")
bxp_ScoreJ6=ggboxplot(data_wide,x="Group",y="D14P2",color="Group",palette="jco",add="jitter")+labs(x="Groups",y="Score D14P2",title="Last Score")+rremove("legend")
bxp_Gender=ggplot(data_wide,aes(as.factor(Group),group=as.factor(Genre),fill=as.factor(Genre)))+geom_bar(width=0.25)+theme_classic2()+theme(legend.title = element_blank(),legend.key.size =unit(0.1,"cm"))+labs(x="Gender",y=" ",title="Gender")+scale_fill_jco()
bxp_Gender

ggarrange(bxp_NET,bxp_GameLevel,bxp_ScoreJ1,bxp_ScoreJ6,bxp_Age,bxp_Gender,ncol=2,nrow=3)
