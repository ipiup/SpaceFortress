#FINAL STATS ON 3 GROUPS 
#####
library(ggplot2)
library(ggpubr)
library(dplyr)
library(rstatix)
library(ggsci)
#####

#1.Descriptives STAT
#1.1 Outliers Detection

ggplot(data,aes(Session, TotalScore,color=as.factor(Treatment)))+geom_boxplot()+theme_classic2()+labs(color = "Group")

outliers=data%>%
  group_by(Session)%>%
  identify_outliers(TotalScore)

##rejected outliers LM2411, EC1603 & TB0301

#1.2 Données démographiques
data_wide$Genre=as.factor(data_wide$Genre)

bxp_NET=ggboxplot(data_wide,x="Group",y="NET",color="Group",palette="jco",add="jitter")+labs(x="Groups",y="NET",title="NET")+rremove("legend")
bxp_GameLevel=ggboxplot(data_wide,x="Group",y="GameLevel",color="Group",palette="jco",add="jitter")+labs(x="Groups",y="GameLevel",title="Game Level")+rremove("legend")
bxp_Age=ggboxplot(data_wide,x="Group",y="Age",color="Group",palette="jco",add="jitter")+labs(x="Group",y="Age",title="Age")+rremove("legend")
bxp_ScoreJ1=ggboxplot(data_wide,x="Group",y="D01P1",color="Group",palette="jco",add="jitter")+labs(x="Groups",y="Score D1",title="First Score")+rremove("legend")
bxp_ScoreJ6=ggboxplot(data_wide,x="Group",y="D14P2",color="Group",palette="jco",add="jitter")+labs(x="Groups",y="Score D14P2",title="Last Score")+rremove("legend")
bxp_Gender=ggplot(data_wide,aes(as.factor(Group),group=as.factor(Genre),fill=as.factor(Genre)))+geom_bar(width=0.25)+theme_classic2()+theme(legend.title = element_blank(),legend.key.size =unit(0.1,"cm"))+labs(x="Gender",y=" ",title="Gender")+scale_fill_jco()

ggarrange(bxp_NET,bxp_GameLevel,bxp_ScoreJ1,bxp_ScoreJ6,bxp_Age,bxp_Gender,ncol=2,nrow=3)
#####
#LEARNING RATE
#D1-D14
data_long$D=1:11
fit_all=lm(ZMean~ln(D),data=data_long) #fit_all_lin=lm(ZMean~D,data=data_long)
a1=coef(fit_all)[1]
a2=coef(fit_all)[2]
x0=exp(a1)
mu=a2
eq=paste0("Equation: ZMean= ",round(a1,4)," + ",round(a2,4),"*ln(D)")

plot_lnreg=ggplot(data_long,aes(D,ZMean))+geom_point()+theme_classic2()+annotate("text",label=paste("Equation :","y~ln(x)"),x=3,y=2)+
  stat_smooth(method=lm,formula=y~ln(x))+stat_summary(geom="point",col="red",fun="mean",size=3)+labs(x="Day")+
  scale_x_continuous(breaks=1:11)
plot_lnreg
#Akaike Information Criterion
AIC(fit_all)

by_pseudo=data_long%>%
  group_by(Pseudo)

lnreg_pseudo=do(by_pseudo,tidy(lm(ZMean~ln(D),data=.)))
intercept_pseudo=lnreg_pseudo$estimate[lnreg_pseudo$term=="(Intercept)"]
slope_pseudo=lnreg_pseudo$estimate[lnreg_pseudo$term=="ln(D)"]

for(str_pseudo in unique(data_wide$Pseudo)){
  data_wide$LearningRate[data_wide$Pseudo==str_pseudo]=lnreg_pseudo$estimate[lnreg_pseudo$term=="ln(D)"&lnreg_pseudo$Pseudo==str_pseudo]
}

#D1-D5
data_long_shortT=subset(data_long,Session!="D14P2"&Session!="D14P1")
fit_all_ST=lm(ZMean~ln(D),data=data_long_shortT) #fit_all_lin=lm(ZMean~D,data=data_long)
a1=coef(fit_all_ST)[1]
a2=coef(fit_all_ST)[2]
x0=exp(a1)
mu=a2
eqst=paste0("Equation: ZMean= ",round(a1,4)," + ",round(a2,4),"*ln(D)")

by_pseudo_ST=data_long_shortT%>%
  group_by(Pseudo)

lnreg_pseudo_ST=do(by_pseudo_ST,tidy(lm(ZMean~ln(D),data=.)))
intercept_pseudo_ST=lnreg_pseudo_ST$estimate[lnreg_pseudo_ST$term=="(Intercept)"]
slope_pseudo_ST=lnreg_pseudo_ST$estimate[lnreg_pseudo_ST$term=="ln(D)"]

for(str_pseudo in unique(data_wide$Pseudo)){
  data_wide$LearningRateST[data_wide$Pseudo==str_pseudo]=lnreg_pseudo_ST$estimate[lnreg_pseudo_ST$term=="ln(D)"&lnreg_pseudo_ST$Pseudo==str_pseudo]
}

#Correlation
cor(data_wide$D01P1ZM,data_wide$LearningRate)
cor(data_wide$D01P1ZM,data_wide$LearningRateST)

ggplot(data_wide,aes(D01P1ZM,LearningRate),add="reg.line")+geom_point()+theme_classic2()+stat_cor(method="pearson")+
  geom_smooth(method='lm',formula=y~x, se = FALSE)+labs(title="Correlation between Learning Rate & Initial ZMean")

ggplot(data_wide,aes(D01P1ZM,LearningRateST),add="reg.line")+geom_point()+theme_classic2()+stat_cor(method="pearson")+
  geom_smooth(method='lm',formula=y~x, se = FALSE)+labs(title="Correlation between Learning Rate & Initial ZMean")

#####
#STATS
#Effect of GameLevel on each Session Score : correlation btw each day and GameLevel
GameD01=ggplot(data_wide,aes(D01P1,GameLevel),add="reg.line")+geom_point()+theme_classic2()+stat_cor(method="pearson")+geom_smooth(method='lm',formula=y~x, se = FALSE)
GameD05=ggplot(data_wide,aes(D05P2,GameLevel),add="reg.line")+geom_point()+theme_classic2()+stat_cor(method="pearson")+geom_smooth(method='lm',formula=y~x, se = FALSE)
GameD14=ggplot(data_wide,aes(D14P2,GameLevel),add="reg.line")+geom_point()+theme_classic2()+stat_cor(method="pearson")+geom_smooth(method='lm',formula=y~x, se = FALSE)
figure=ggarrange(GameD01,GameD05,GameD14,ncol=1,nrow=3,labels = c("D01","D05","D14"))
figure

#Effect of GameLevel on the learning Rate
GameLRST=ggplot(data_wide,aes(LearningRateST,GameLevel),add="reg.line")+geom_point()+theme_classic2()+stat_cor(method="pearson")+geom_smooth(method='lm',formula=y~x, se = FALSE)
GameLRLT=ggplot(data_wide,aes(LearningRate,GameLevel),add="reg.line")+geom_point()+theme_classic2()+stat_cor(method="pearson")+geom_smooth(method='lm',formula=y~x, se = FALSE)


#Effect of Stim on learning Rate Anova on three groups
data_wide$Group=factor(data_wide$Group,levels=c("SHAM","STIM-SD","STIM-HD"))
data_wide%>%
  anova_test(LearningRate~Group)

ggplot(data_wide,aes(x=Group,LearningRateST,color=Group))+geom_boxplot()+theme_classic2()+rremove("legend")+xlab("Groups")+stat_compare_means(method="anova")
ggplot(data_wide,aes(x=Group,LearningRate,color=Group))+geom_boxplot()+theme_classic2()+rremove("legend")+xlab("Groups")+stat_compare_means(method="anova")

tukST=data_wide%>%
  tukey_hsd(LearningRateST~Group)
ggboxplot(data_wide,x="Group",y="LearningRateST",color="Group")+stat_pvalue_manual(tukST, label = "p.adj.signif", tip.length = 0.01,y.position = c(1.2,1,1.1))+stat_compare_means(method="anova",label.y=0.2) 

tuk=data_wide%>%
  tukey_hsd(LearningRate~Group)
ggboxplot(data_wide,x="Group",y="LearningRate",color="Group")+stat_pvalue_manual(tuk, label = "p.adj.signif", tip.length = 0.01,y.position = c(1.2,1,1.1))+stat_compare_means(method="anova",label.y=0.2)



