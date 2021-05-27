#FINAL STATS ON 3 GROUPS 
#####
library(ggplot2)
library(ggpubr)
library(dplyr)
library(rstatix)
library(ggsci)
library(MANOVA.RM)
library(viridis)
library(hrbrthemes)
#####
data_wide$Group=factor(data_wide$Group,levels=c("STIM-SD","STIM-HD","SHAM"))
data_long$Group=factor(data_long$Group,levels=c("STIM-SD","STIM-HD","SHAM"))
#1.Descriptives STAT
#1.1 Outliers Detection
plot_=ggplot(data_long,aes(Session, TotalScore,fill=as.factor(Group)))+geom_boxplot(alpha=0.6,position=position_dodge(0.9))+labs(fill = "Group")+theme_pubr()
set_palette(plot_,"jco")

outliers=data%>%
  group_by(Session)%>%
  identify_outliers(TotalScore)
##rejected outliers LM2411, EC1603 & TB0301

#1.2 Données démographiques
data_wide$Genre=as.factor(data_wide$Genre)

#####
bxp_NET=ggboxplot(data_wide,x="Group",y="NET",color="Group",palette="jco",add="jitter")+labs(x="",y="NET",title="NET")+rremove("legend")+stat_compare_means(method="anova",label.y=19)
bxp_GameLevel=ggboxplot(data_wide,x="Group",y="GameLevel",color="Group",palette="jco",add="jitter")+labs(x="",y="GameLevel",title="Game Level")+rremove("legend")+stat_compare_means(method="anova",label.y=9)
bxp_Age=ggboxplot(data_wide,x="Group",y="Age",color="Group",palette="jco",add="jitter")+labs(x="",y="Age",title="Age")+rremove("legend")+stat_compare_means(method="anova",label.y=35)
bxp_ScoreJ1=ggboxplot(data_wide,x="Group",y="D01P1",color="Group",palette="jco",add="jitter")+labs(x="",y="Score D1",title="First Score")+rremove("legend")+stat_compare_means(method="anova",label.y=5000)
bxp_ScoreJ6=ggboxplot(data_wide,x="Group",y="D14P2",color="Group",palette="jco",add="jitter")+labs(x="",y="Score D14P2",title="Last Score")+rremove("legend")+stat_compare_means(method="anova",label.y=20000)
bxp_Gender=ggplot(data_wide,aes(as.factor(Group),group=as.factor(Genre),fill=as.factor(Genre)))+geom_bar(width=0.25)+theme_classic2()+theme(legend.title = element_blank(),legend.key.size =unit(0.1,"cm"),legend.position =c(0.4,1.1),legend.direction = "horizontal",legend.background = element_rect(fill ="transparent"),)+labs(x="",y=" ",title="Gender")+scale_fill_jco()

ggarrange(bxp_NET,bxp_GameLevel,bxp_ScoreJ1,bxp_ScoreJ6,bxp_Age,bxp_Gender,ncol=2,nrow=3,align="v")
#2.LEARNING RATE
#2.1 D1-D14
data_long$D=1:11
fit_all=lm(ZMean~ln(D),data=data_long) #fit_all_lin=lm(ZMean~D,data=data_long)
plot_lnreg_LT=ggplot(data_long,aes(D,ZMean))+geom_point()+theme_classic2()+annotate("text",label=paste("Equation :","y~ln(x)"),x=3,y=2)+
  stat_smooth(method=lm,formula=y~ln(x))+stat_summary(geom="point",col="red",fun="mean",size=3)+labs(x="Day")+
  scale_x_continuous(breaks=1:11)
eq=paste0("Equation: ZMean= ",round(coef(fit_all)[1],4)," + ",round(coef(fit_all)[2],4),"*ln(D)")

#2.2D1-D5
data_long_shortT=subset(data_long,Session!="D14P2"&Session!="D14P1")
fit_all_ST=lm(ZMean~ln(D),data=data_long_shortT) #fit_all_lin=lm(ZMean~D,data=data_long)
plot_lnreg_ST=ggplot(data_long_shortT,aes(D,ZMean))+geom_point()+theme_classic2()+annotate("text",label=paste("Equation :","y~ln(x)"),x=3,y=2)+
  stat_smooth(method=lm,formula=y~ln(x))+stat_summary(geom="point",col="red",fun="mean",size=3)+labs(x="Day")+
  scale_x_continuous(breaks=1:9)
eq=paste0("Equation: ZMean= ",round(coef(fit_all_ST)[1],4)," + ",round(coef(fit_all_ST)[2],4),"*ln(D)")


#2.3Correlation
cor(data_wide$D01P1ZM,data_wide$LearningRate)
cor(data_wide$D01P1ZM,data_wide$LearningRateST)

ggplot(data_wide,aes(D01P1ZM,LearningRate),add="reg.line")+geom_point()+theme_classic2()+stat_cor(method="pearson")+
  geom_smooth(method='lm',formula=y~x, se = FALSE)+labs(title="Correlation between Learning Rate & Initial ZMean")

ggplot(data_wide,aes(D01P1ZM,LearningRateST),add="reg.line")+geom_point()+theme_classic2()+stat_cor(method="pearson")+
  geom_smooth(method='lm',formula=y~x, se = FALSE)+labs(title="Correlation between Learning Rate & Initial ZMean")

#####
#3.STATS
#Effect of GameLevel on each Session Score : correlation btw each day and GameLevel
GameD01=ggplot(data_wide,aes(D01P1,GameLevel),add="reg.line")+geom_point()+theme_classic2()+stat_cor(method="pearson")+geom_smooth(method='lm',formula=y~x, se = FALSE)
GameD05=ggplot(data_wide,aes(D05P2,GameLevel),add="reg.line")+geom_point()+theme_classic2()+stat_cor(method="pearson")+geom_smooth(method='lm',formula=y~x, se = FALSE)
GameD14=ggplot(data_wide,aes(D14P2,GameLevel),add="reg.line")+geom_point()+theme_classic2()+stat_cor(method="pearson")+geom_smooth(method='lm',formula=y~x, se = FALSE)
figure=ggarrange(GameD01,GameD05,GameD14,ncol=1,nrow=3,labels = c("D01","D05","D14"),label.x=0.05)
figure

#Effect of GameLevel on the learning Rate
GameLRST=ggplot(data_wide,aes(LearningRateST,GameLevel),add="reg.line")+geom_point()+theme_classic2()+stat_cor(method="pearson")+geom_smooth(method='lm',formula=y~x, se = FALSE)
GameLRLT=ggplot(data_wide,aes(LearningRateLT,GameLevel),add="reg.line")+geom_point()+theme_classic2()+stat_cor(method="pearson")+geom_smooth(method='lm',formula=y~x, se = FALSE)
figure=ggarrange(GameLRLT,GameLRST,ncol=1,nrow=2,labels=c("A","B"))
figure

#Effect of Stim on learning Rate Anova on three groups
data_wide$Group=factor(data_wide$Group,levels=c("STIM-SD","STIM-HD","SHAM"))
data_wide%>%
  anova_test(LearningRateST~Group)

LRG_ST=ggplot(data_wide,aes(x=Group,LearningRateST,fill=as.factor(Group)))+geom_boxplot(alpha=0.6,position=position_dodge(0.9))+labs(fill = "Group")+theme_pubr()+rremove("legend")+xlab("Groups")+stat_compare_means(method="anova")
LRG_LT=ggplot(data_wide,aes(x=Group,LearningRateLT,fill=as.factor(Group)))+geom_boxplot(alpha=0.6,position=position_dodge(0.9))+labs(fill = "Group")+theme_pubr()+rremove("legend")+xlab("Groups")+stat_compare_means(method="anova")
LRG_ST=set_palette(LRG_ST,"jco")
LRG_LT=set_palette(LRG_LT,"jco")
figure=ggarrange(LRG_LT,LRG_ST,ncol=1,nrow=2,labels=c("A","B"))
figure


tukST=data_wide%>%
  tukey_hsd(LearningRateST~Group)
ggboxplot(data_wide,x="Group",y="LearningRateST",color="Group")+stat_pvalue_manual(tukST, label = "p.adj.signif", tip.length = 0.01,y.position = c(1.2,1,1.1))+stat_compare_means(method="anova",label.y=0.2) 

tuk=data_wide%>%
  tukey_hsd(LearningRate~Group)
ggboxplot(data_wide,x="Group",y="LearningRate",color="Group")+stat_pvalue_manual(tuk, label = "p.adj.signif", tip.length = 0.01,y.position = c(1.2,1,1.1))+stat_compare_means(method="anova",label.y=0.2)

#MANOVA RM
data_long_P2=subset(data_long,Session=="D01P1"|grepl("P2",Session))
data_long_P2$Session=substring(data_long_P2$Session,1,3)
data_long_P2$Treatment=as.factor(data_long_P2$Treatment)
data_long_P2$Session=as.factor(data_long_P2$Session)
data_long_P2$Pseudo=as.factor(data_long_P2$Pseudo)
data_long_P2$GameLevel=as.factor(data_long_P2$GameLevel)

MANOVA.RM::RM(ZMean~GameLevel*Treatment*Session,data=data_long_P2,subject = "Pseudo")

#ANOVA with Genre as covariable
data_long_LT=subset(data_long,Session=="D05P2"|Session=="D14P2")
data_long_LT$Session=as.factor(data_long_LT$Session)
data_long_LT$Genre=as.factor(data_long_LT$Genre)
data_long_LT$Treatment=as.factor(data_long_LT$Treatment)
data_long_LT$GameLevel=as.factor(data_long_LT$GameLevel)

data_long_LT%>%anova_test(ZMean~Treatment*Session+Genre)
data_long_LT%>%tukey_hsd(ZMean~Treatment*Session+Genre)

summary(manova)

#ANCOVA
ancova=data_wide%>%anova_test(LearningRateST~GameLevel+Group)
get_anova_table(ancova)
