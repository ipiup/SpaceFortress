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
data_wide$Genre=as.factor(data_wide$Genre)
data_wide$GameLevel=as.numeric(data_wide$GameLevel)

data_long$Group=factor(data_long$Group,levels=c("STIM-SD","STIM-HD","SHAM"))
data_long$Treatment=as.factor(data_long$Treatment)

data_long_P2=subset(data_long,Session=="D01P1"|grepl("P2",Session))
data_long_P2$Session=substring(data_long_P2$Session,1,3)
data_long_P2$Treatment=as.factor(data_long_P2$Treatment)
data_long_P2$Session=as.factor(data_long_P2$Session)
data_long_P2$Pseudo=as.factor(data_long_P2$Pseudo)
data_long_P2$GameLevel=as.factor(data_long_P2$GameLevel)
#data_long_P2$ZScore=as.numeric(data_long_P2$ZScore)
data_long_P2_noD14=subset(data_long_P2,Session!="D14")
data_long_P2_noD14$Session=as.factor(data_long_P2_noD14$Session)
data_long_P2_noD14$Treatment=as.factor(data_long_P2_noD14$Treatment)
data_long_P2_noD14$Pseudo=as.factor(data_long_P2_noD14$Pseudo)
#data_long_P2_noD14$ZScore=as.numeric(data_long_P2_noD14$ZScore)

data_long_P2_D14=subset(data_long_P2,Session=="D05"|Session=="D14")
data_long_P2_D14$Group=as.factor(data_long_P2_D14$Group)
data_long_P2_D14$Session=as.factor(data_long_P2_D14$Session)


#1.Descriptives STAT
#1.1 Outliers Detection
plot_=ggplot(data_long,aes(Session, TotalScore,fill=as.factor(Group)))+geom_boxplot(alpha=0.6,position=position_dodge(0.9))+labs(fill = "Group")+theme_pubr()
set_palette(plot_,"jco")

outliers=data%>%
  group_by(Session)%>%
  identify_outliers(TotalScore)
##rejected outliers LM2411, EC1603 & TB0301

#1.2 Données démographiques

#####
bxp_NET=ggboxplot(data_wide,x="Group",y="NET",color="Group",palette="jco",add="jitter")+labs(x="",y="NET",title="NET")+rremove("legend")+stat_compare_means(method="anova",label.y=19)
bxp_GameLevel=ggboxplot(data_wide,x="Group",y="GameLevel",color="Group",palette="jco",add="jitter")+labs(x="",y="GameLevel",title="Game Level")+rremove("legend")+stat_compare_means(method="anova",label.y=9)
bxp_Age=ggboxplot(data_wide,x="Group",y="Age",color="Group",palette="jco",add="jitter")+labs(x="",y="Age",title="Age")+rremove("legend")+stat_compare_means(method="anova",label.y=35)
bxp_ScoreJ1=ggboxplot(data_wide,x="Group",y="D01P1",color="Group",palette="jco",add="jitter")+labs(x="",y="Score D1",title="First Score")+rremove("legend")+stat_compare_means(method="anova",label.y=5000)
bxp_ScoreJ6=ggboxplot(data_wide,x="Group",y="D14P2",color="Group",palette="jco",add="jitter")+labs(x="",y="Score D14P2",title="Last Score")+rremove("legend")+stat_compare_means(method="anova",label.y=20000)
bxp_Gender=ggplot(data_wide,aes(as.factor(Group),group=as.factor(Genre),fill=as.factor(Genre)))+geom_bar(width=0.25)+theme_classic2()+theme(legend.title = element_blank(),legend.key.size =unit(0.1,"cm"),legend.position =c(0.4,1.1),legend.direction = "horizontal",legend.background = element_rect(fill ="transparent"),)+labs(x="",y=" ",title="Gender")+scale_fill_jco()

ggarrange(bxp_NET,bxp_GameLevel,bxp_ScoreJ1,bxp_ScoreJ6,bxp_Age,bxp_Gender,ncol=2,nrow=3,align="v")

#1.3Distributions ON P2
ggdensity(data_long_P2$TotalScore)+geom_histogram(binwidth=150)+xlab("Total Score")
p1=ggdensity(data_long_P2$Flight)+geom_histogram(binwidth = 100)+xlab("Flight Score")
p2=ggdensity(data_long_P2$Bonus)+geom_histogram(binwidth = 100)+xlab("Bonus Score")
p3=ggdensity(data_long_P2$Mine)+geom_histogram(binwidth = 100)+xlab("Mine Score")
p4=ggdensity(data_long_P2$Fortress)+geom_histogram(binwidth = 200)+xlab("Fortress Score")
figure=ggarrange(p1,p2,p3,p4,ncol=2,nrow=2)
figure
#1.4ZSCORES
#Zscore and ZMean cor
# p1=ggdensity(data_long_P2$ZFlight)+geom_histogram(binwidth = 0.1)+xlab("Flight ZScore")
# p2=ggdensity(data_long_P2$ZBonus)+geom_histogram(binwidth = 0.1)+xlab("Bonus ZScore")
# p3=ggdensity(data_long_P2$ZMine)+geom_histogram(binwidth = 0.1)+xlab("Mine ZScore")
# p4=ggdensity(data_long_P2$ZFortress)+geom_histogram(binwidth = 0.1)+xlab("Fortress ZScore")
# figure=ggarrange(p1,p2,p3,p4,ncol=2,nrow=2)
# figure
# Zcor=ggplot(data_long_P2,aes(ZMean,ZScore))+geom_point()+geom_smooth(method="lm")+theme_pubr()+stat_cor(method="spearman")
# Zcor

#2.LEARNING RATE
#2.1 D1-D14
data_long$D=1:11
fit_all=lm(TotalScore~ln(D),data=data_long) #fit_all_lin=lm(TotalScore~D,data=data_long)
eq=paste0("Equation: TotalScore= ",round(coef(fit_all)[1],4)," + ",round(coef(fit_all)[2],4),"*ln(D)")

#2.2D1-D5
data_long_shortT=subset(data_long,Session!="D14P2"&Session!="D14P1")
fit_all_ST=lm(TotalScore~ln(D),data=data_long_shortT) #fit_all_lin=lm(TotalScore~D,data=data_long)
eq=paste0("Equation: TotalScore= ",round(coef(fit_all_ST)[1],4)," + ",round(coef(fit_all_ST)[2],4),"*ln(D)")

#2.3Correlation
cor(data_wide$D01P1ZM,data_wide$LearningRateLT)
cor(data_wide$D01P1ZM,data_wide$LearningRateST)

#PLOTS
plot_lnreg_LT=ggplot(data_long,aes(D,TotalScore))+geom_point()+theme_classic2()+annotate("text",label=paste("Equation :","y~ln(x)"),x=3,y=2)+
  stat_smooth(method=lm,formula=y~ln(x))+stat_summary(geom="point",col="red",fun="mean",size=3)+labs(x="Day")+
  scale_x_continuous(breaks=1:11)

plot_lnreg_ST=ggplot(data_long_shortT,aes(D,TotalScore))+geom_point()+theme_classic2()+annotate("text",label=paste("Equation :","y~ln(x)"),x=3,y=2)+
  stat_smooth(method=lm,formula=y~ln(x))+stat_summary(geom="point",col="red",fun="mean",size=3)+labs(x="Day")+
  scale_x_continuous(breaks=1:9)

p_reg_ST=ggplot(data_wide,aes(D01P1,LearningRateST),add="reg.line")+geom_point()+theme_classic2()+stat_cor(method="spearman")+
  geom_smooth(method='lm',formula=y~x, se = FALSE)+labs(title="Correlation between Learning Rate & Initial Score")+scale_x_continuous(breaks=seq(-3.5,3,0.5))#+scale_x_continuous(breaks=seq(-3.5,0,0.5))

p_reg_LT=ggplot(data_wide,aes(D01P1,LearningRateLT),add="reg.line")+geom_point()+theme_classic2()+stat_cor(method="spearman")+
  geom_smooth(method='lm',formula=y~x, se = FALSE)+labs(title="Correlation between Learning Rate & Initial Score")+scale_x_continuous(breaks=seq(-3.5,3,0.5))

reg_ST=ggarrange(plot_lnreg_ST,p_reg_ST,nrow=2,ncol=1,labels = c("A","B"))
reg_LT=ggarrange(plot_lnreg_LT,p_reg_LT,nrow=2,ncol=1,labels = c("A","B"))
reg_ST
reg_LT

cor(data_wide$D01P1,data_wide$LearningRateLT,method="pearson")

#####
#3.STATS
#Effect of GameLevel on each Session Score : correlation btw each day and GameLevel
GameD01=ggplot(data_wide,aes(GameLevel,D01P1),add="reg.line")+geom_point()+theme_classic2()+stat_cor(method="spearman")+geom_smooth(method='lm',formula=y~x, se = FALSE)
GameD05=ggplot(data_wide,aes(GameLevel,D05P2),add="reg.line")+geom_point()+theme_classic2()+stat_cor(method="spearman")+geom_smooth(method='lm',formula=y~x, se = FALSE)
GameD14=ggplot(data_wide,aes(GameLevel,D14P2),add="reg.line")+geom_point()+theme_classic2()+stat_cor(method="spearman")+geom_smooth(method='lm',formula=y~x, se = FALSE)
figure=ggarrange(GameD01,GameD05,GameD14,ncol=1,nrow=3,labels = c("D01","D05","D14"),label.x=0.05)
figure

#Effect of GameLevel on the learning Rate
GameLRST=ggplot(data_wide,aes(GameLevel,LearningRateST),add="reg.line")+geom_point()+theme_classic2()+stat_cor(method="spearman")+geom_smooth(method='lm',formula=y~x, se = FALSE)
GameLRLT=ggplot(data_wide,aes(GameLevel,LearningRateLT),add="reg.line")+geom_point()+theme_classic2()+stat_cor(method="spearman")+geom_smooth(method='lm',formula=y~x, se = FALSE)
figure=ggarrange(GameLRLT,GameLRST,ncol=1,nrow=2,labels=c("A","B"))
figure


#Effect of Stim on learning Rate Anova on three groups
data_wide_NF%>%
  anova_test(LearningRateLT~Group)

LRG_ST=ggplot(data_wide,aes(x=Group,LearningRateST,fill=as.factor(Group)))+geom_boxplot(alpha=0.6,position=position_dodge(0.9))+labs(fill = "Group")+theme_pubr()+rremove("legend")+xlab("Groups")+stat_compare_means(method="anova",label.y=0.9)
LRG_LT=ggplot(data_wide,aes(x=Group,LearningRateLT,fill=as.factor(Group)))+geom_boxplot(alpha=0.6,position=position_dodge(0.9))+labs(fill = "Group")+theme_pubr()+rremove("legend")+xlab("Groups")+stat_compare_means(method="anova",label.y=0.9)
LRG_ST=set_palette(LRG_ST,"jco")
LRG_LT=set_palette(LRG_LT,"jco")
figure=ggarrange(LRG_LT,LRG_ST,ncol=1,nrow=2,labels=c("A","B"))
figure

tukST=data_wide_NF%>%
  tukey_hsd(LearningRateST~Group)
LRG_ST=ggboxplot(data_wide_NF,x="Group",y="LearningRateST",fill="Group")+stat_pvalue_manual(tukST, label = "p.adj.signif", tip.length = 0.01,y.position = c(1.6,1.4,1.5))+stat_compare_means(method="anova",label.y=0.2)+fill_palette("jco")+rremove("legend")+xlab("")

tukLT=data_wide_NF%>%
  tukey_hsd(LearningRateLT~Group)
data_wide_NF%>%
  group_by(Group)%>%
  summarize(mean(LearningRateLT))
LRG_LT=ggboxplot(data_wide_NF,x="Group",y="LearningRateLT",fill="Group")+stat_pvalue_manual(tukLT, label = "p.adj.signif", tip.length = 0.01,y.position = c(1.6,1.4,1.5))+stat_compare_means(method="anova",label.y=0.2)+fill_palette("jco")+rremove("legend")+xlab("")
figure=ggarrange(LRG_LT,LRG_ST,ncol=1,nrow=2,labels=c("A","B"))
figure


#MANOVA RM
plot_=ggplot(data_long_P2_noD14,aes(Session, TotalScore,fill=Group))+geom_boxplot(alpha=0.6,position=position_dodge(0.9))+labs(fill = "Group")+theme_pubr()
set_palette(plot_,"jco")
Mano=MANOVA.RM::RM(TotalScore~Group*Session,data=data_long_P2_noD14,subject = "Pseudo")
data_long_P2_noD14%>%tukey_hsd(TotalScore~Group*Session)

#Effet on Long term (D5-D14)
plot_=ggplot(data_long_P2_D14,aes(Session, TotalScore,fill=Group))+geom_boxplot(alpha=0.6,position=position_dodge(0.9))+labs(fill = "Group")+theme_pubr()
set_palette(plot_,"jco")

data_long_P2_D14%>%anova_test(TotalScore~Group*Session)
data_long_P2_D14%>%tukey_hsd(TotalScore~Group*Session)


#ANOVA with Genre/GameLevel as covariable
data_long_P2_D14=subset(data_long_P2_D14,Pseudo!="CP1809"&Pseudo!="MM0301"&Pseudo!="SP0801"&Pseudo!="CH0205")
data_long_P2_D14%>%anova_test(TotalScore~Group*Session+GameLevel)
data_long_P2_D14%>%tukey_hsd(TotalScore~Group*Session+GameLevel)



data_long_P2_D14%>%anova_test(TotalScore~Group*Session+GameLevel)
data_long_P2_D14%>%tukey_hsd(TotalScore~Group*Session+GameLevel)


data_long_P2_D14%>%anova_test(TotalScore~Group*Session)
data_long_P2_D14%>%tukey_hsd(TotalScore~Group*Session)

data_wide%>%
  anova_test(DeltaD1D5~Group+GameLevel)

tukdelta=data_wide%>%
  tukey_hsd(DeltaD14D5~Group)


data_wide$DeltaD1D14=data_wide$D14P2-data_wide$D01P1
data_wide$DeltaD1D5=data_wide$D05P2-data_wide$D01P1
data_wide$DeltaD14D5=data_wide$D14P2-data_wide$D05P2


ggboxplot(data_wide,x="Group",y="DeltaD14D5",fill="Group")+theme_classic2()+stat_pvalue_manual(tukdelta, label = "p.adj.signif", tip.length = 0.01,y.position = c(7000,7500,7000))

ggplot(data_long_P2_D14,aes(Group,TotalScore,fill=Group,palette="jco"))+geom_boxplot()+theme_classic2()


#T-test between GameLevel and Gendre
t_gender=t.test(data_wide$GameLevel~data_wide$Genre,paired=FALSE)

ggboxplot(data_wide,x="Genre",y="GameLevel",fill="Genre",add="jitter")+scale_fill_jco()+stat_compare_means(method = "t.test")


#####
#Correlation btw Game Level and D01P1
ggplot(data_wide,aes(GameLevel,D01P1))+geom_point()+theme_classic2()+stat_cor(method="pearson")+geom_smooth(method='lm',formula=y~x, se = FALSE)