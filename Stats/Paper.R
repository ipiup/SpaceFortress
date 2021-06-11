#Paper Plots


data_wide$Group=factor(data_wide$Group,levels=c("SHAM","STIMSD","STIMHD"))

couleurs=c("#868686FF","#0073C2FF","#A73030FF")
couleurs_alpha=c("#86868666","#0073C266","#A7303099")

#Halves plots
#DELTA
ph_DeltaD1D5=data_wide%>%
  emmeans_test(DeltaD1D5~Group,covariate = GameLevelLog,p.adjust.method = "holm")
ph_DeltaD1D14=data_wide%>%
  emmeans_test(DeltaD1D14~Group,covariate = GameLevelLog,p.adjust.method = "holm")
ph_DeltaD14D5=data_wide%>%
  emmeans_test(DeltaD14D5~Group,covariate = GameLevelLog,p.adjust.method = "holm")
##

plot_D14D5=ggplot(data_wide,aes(Group,DeltaD14D5,color=Group,fill=Group))+
  theme_pubr()+scale_fill_manual(values=couleurs_alpha)+
  scale_color_manual(values=couleurs)+
  geom_half_violin(width=0.3, position = position_nudge(x=-0.2,y=0))+geom_jitter(width=0.1)+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=1 ,show.legend = FALSE,geom="errorbar",width=0.1)+
  stat_summary(fun=mean, geom="point",size=4)
  #geom_boxplot(width=0.1,outlier.shape=NA,show.legend = FALSE, position = position_nudge(x=+0.2,y=0))

plot_D14D5=plot_D14D5+
  add_pvalue(ph_DeltaD14D5,y.position=c(7500,8500,8000),
             label = "p = {round(p.adj,3)} {p.adj.signif}", inherit.aes = FALSE,fontface="bold")+theme(axis.title=element_text(size=12,face="bold"),axis.text =element_text(size=12) )+ylab("Performance Retention (Day 14 - Day 5)")+rremove("legend")+scale_x_discrete(labels=c("SHAM","STIM-SD","STIM-HD"))+scale_y_continuous( breaks=seq(-4000,8000,2000))
plot_D14D5  


plot_D14D1=ggplot(data_wide,aes(Group,DeltaD1D14,color=Group,fill=Group))+
  theme_pubr()+scale_fill_manual(values=couleurs_alpha)+
  scale_color_manual(values=couleurs)+
  geom_half_violin(width=0.3, position = position_nudge(x=-0.2,y=0))+geom_jitter(width=0.1)+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=1 ,show.legend = FALSE,geom="errorbar",width=0.1)+
  stat_summary(fun=mean, geom="point",size=4)
#geom_boxplot(width=0.1,outlier.shape=NA,show.legend = FALSE, position = position_nudge(x=+0.2,y=0))

plot_D14D1=plot_D14D1+
  add_pvalue(ph_DeltaD1D14,y.position=c(20000,22000,21000),
             label = "p = {round(p.adj,3)} {p.adj.signif}", inherit.aes = FALSE,fontface="bold")+theme(axis.title=element_text(size=12,face="bold"),axis.text =element_text(size=12) )+ylab("Performance Retention (Day 14 - Day 1)")+rremove("legend")+scale_x_discrete(labels=c("SHAM","STIM-SD","STIM-HD"))+scale_y_continuous( breaks=seq(0,20000,2000))
plot_D14D1

plot_D5D1=ggplot(data_wide,aes(Group,DeltaD1D5,color=Group,fill=Group))+
  theme_pubr()+scale_fill_manual(values=couleurs_alpha)+
  scale_color_manual(values=couleurs)+
  geom_half_violin(width=0.3, position = position_nudge(x=-0.2,y=0))+geom_jitter(width=0.1)+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=1 ,show.legend = FALSE,geom="errorbar",width=0.1)+
  stat_summary(fun=mean, geom="point",size=4)
#geom_boxplot(width=0.1,outlier.shape=NA,show.legend = FALSE, position = position_nudge(x=+0.2,y=0))

plot_D5D1=plot_D5D1+
  add_pvalue(ph_DeltaD1D5,y.position=c(20000,22000,21000),
             label = "p = {round(p.adj,3)} {p.adj.signif}", inherit.aes = FALSE,fontface="bold")+theme(axis.title=element_text(size=12,face="bold"),axis.text =element_text(size=12) )+ylab("Performance Retention (Day 5 - Day 1)")+rremove("legend")+scale_x_discrete(labels=c("SHAM","STIM-SD","STIM-HD"))+scale_y_continuous( breaks=seq(0,20000,2000))
plot_D5D1

#####
#SOUS SCORES (pas de covariable)
#FORTRESS
ph_DeltaD14D5_Fortress=data_wide%>%
  emmeans_test(DeltaD14D5Fortress~Group,p.adjust.method = "holm")

plot_D14D5_Fortress=ggplot(data_wide,aes(Group,DeltaD14D5Fortress,color=Group,fill=Group))+
  theme_pubr()+scale_fill_manual(values=couleurs_alpha)+
  scale_color_manual(values=couleurs)+
  geom_half_violin(width=0.3, position = position_nudge(x=-0.2,y=0))+geom_jitter(width=0.1)+
  geom_boxplot(width=0.1,outlier.shape=NA,show.legend = FALSE, position = position_nudge(x=+0.2,y=0))+
  stat_summary(fun.data = "mean_sdl", fun.args = list(mult = 1),size=1.3 ,show.legend = FALSE)

plot_D14D5_Fortress=plot_D14D5_Fortress+
  add_pvalue(ph_DeltaD14D5_Fortress,y.position=c(7000,8000,7500),
             label = "p = {round(p.adj,3)} {p.adj.signif}", inherit.aes = FALSE,fontface="bold")+theme(axis.title=element_text(size=12,face="bold"),axis.text =element_text(size=12) )+ylab("Performance Retention Fortress Score (Day 14 - Day 5)")+rremove("legend")+scale_x_discrete(labels=c("SHAM","STIM-SD","STIM-HD"))+scale_y_continuous( breaks=seq(0,20000,2000))
plot_D14D5_Fortress

#FLIGHT
ph_DeltaD14D5_Flight=data_wide%>%
  emmeans_test(DeltaD14D5Flight~Group,p.adjust.method = "holm")

plot_D14D5_Flight=ggplot(data_wide,aes(Group,DeltaD14D5Flight,color=Group,fill=Group))+
  theme_pubr()+scale_fill_manual(values=couleurs_alpha)+
  scale_color_manual(values=couleurs)+
  geom_half_violin(width=0.3, position = position_nudge(x=-0.2,y=0))+geom_jitter(width=0.1)+
  geom_boxplot(width=0.1,outlier.shape=NA,show.legend = FALSE, position = position_nudge(x=+0.2,y=0))+
  stat_summary(fun.data = "mean_sdl", fun.args = list(mult = 1),size=1.3 ,show.legend = FALSE)

plot_D14D5_Flight=plot_D14D5_Flight+
  add_pvalue(ph_DeltaD14D5_Flight,y.position=c(3000,4000,3500),
             label = "p = {round(p.adj,3)} {p.adj.signif}", inherit.aes = FALSE,fontface="bold")+theme(axis.title=element_text(size=12,face="bold"),axis.text =element_text(size=12) )+ylab("Performance Retention Flight Score (Day 14 - Day 5)")+rremove("legend")+scale_x_discrete(labels=c("SHAM","STIM-SD","STIM-HD"))+scale_y_continuous( breaks=seq(0,20000,2000))
plot_D14D5_Flight

#BONUS
ph_DeltaD14D5_Bonus=data_wide%>%
  emmeans_test(DeltaD14D5Bonus~Group,p.adjust.method = "holm")

plot_D14D5_Bonus=ggplot(data_wide,aes(Group,DeltaD14D5Bonus,color=Group,fill=Group))+
  theme_pubr()+scale_fill_manual(values=couleurs_alpha)+
  scale_color_manual(values=couleurs)+
  geom_half_violin(width=0.3, position = position_nudge(x=-0.2,y=0))+geom_jitter(width=0.1)+
  geom_boxplot(width=0.1,outlier.shape=NA,show.legend = FALSE, position = position_nudge(x=+0.2,y=0))+
  stat_summary(fun.data = "mean_sdl", fun.args = list(mult = 1),size=1.3 ,show.legend = FALSE)

plot_D14D5_Bonus=plot_D14D5_Bonus+
  add_pvalue(ph_DeltaD14D5_Bonus,y.position=c(3000,4000,3500),
             label = "p = {round(p.adj,3)} {p.adj.signif}", inherit.aes = FALSE,fontface="bold")+theme(axis.title=element_text(size=12,face="bold"),axis.text =element_text(size=12) )+ylab("Performance Retention Bonus Score (Day 14 - Day 5)")+rremove("legend")+scale_x_discrete(labels=c("SHAM","STIM-SD","STIM-HD"))+scale_y_continuous( breaks=seq(0,20000,2000))
plot_D14D5_Bonus

#BONUS
ph_DeltaD14D5_Mine=data_wide%>%
  emmeans_test(DeltaD14D5Mine~Group,p.adjust.method = "holm")

plot_D14D5_Mine=ggplot(data_wide,aes(Group,DeltaD14D5Mine,color=Group,fill=Group))+
  theme_pubr()+scale_fill_manual(values=couleurs_alpha)+
  scale_color_manual(values=couleurs)+
  geom_half_violin(width=0.3, position = position_nudge(x=-0.2,y=0))+geom_jitter(width=0.1)+
  geom_boxplot(width=0.1,outlier.shape=NA,show.legend = FALSE, position = position_nudge(x=+0.2,y=0))+
  stat_summary(fun.data = "mean_sdl", fun.args = list(mult = 1),size=1.3 ,show.legend = FALSE)

plot_D14D5_Mine=plot_D14D5_Mine+
  add_pvalue(ph_DeltaD14D5_Mine,y.position=c(3000,4000,3500),
             label = "p = {round(p.adj,3)} {p.adj.signif}", inherit.aes = FALSE,fontface="bold")+theme(axis.title=element_text(size=12,face="bold"),axis.text =element_text(size=12) )+ylab("Performance Retention Mine Score (Day 14 - Day 5)")+rremove("legend")+scale_x_discrete(labels=c("SHAM","STIM-SD","STIM-HD"))+scale_y_continuous( breaks=seq(0,20000,2000))
plot_D14D5_Mine


#DELTA BY GROUP
library(reshape2)
data_D14D5_SousScore=subset(data_wide,select=c("Pseudo","Group","GameLevelLog","DeltaD14D5Flight","DeltaD14D5Bonus","DeltaD14D5Mine","DeltaD14D5Fortress"))
data_long_delta=melt(data_D14D5_SousScore,id.vars=c("Pseudo","Group","GameLevelLog"))
colnames(data_long_delta)=c("Pseudo","Group","GameLevelLog","Delta","Performance")

plot_delta_SousScore=ggplot(data_long_delta,aes(Delta,Performance,color=Group,fill=Group,group=Group))+scale_fill_manual(values=couleurs_alpha)+scale_color_manual(values=couleurs)+theme_pubr()+
   stat_summary(fun.data = "mean_se", fun.args = list(mult = 1) ,position=position_dodge(width=0.5))+
  scale_x_discrete(labels=c("Flight","Bonus","Mine","Fortress"))+ylab("Score")+xlab("Delta D14 - D5")+geom_point(alpha=0.2,position=position_dodge(width=0.5))

#LEARNING RATE
data_wide%>%
  anova_test(LearningRateLT~Group+GameLevelLog)

ph_LR_GL=data_wide%>%
  emmeans_test(LearningRateLT~Group,covariate = GameLevelLog,p.adjust.method = "holm")

plot_LR_GL=ggplot(data_wide,aes(Group,LearningRateLT,color=Group,fill=Group))+
  theme_pubr()+scale_fill_manual(values=couleurs_alpha)+
  scale_color_manual(values=couleurs)+
  geom_half_violin(width=0.3, position = position_nudge(x=-0.2,y=0))+geom_jitter(width=0.1)+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=1 ,show.legend = FALSE,geom="errorbar",width=0.1)+
  stat_summary(fun=mean, geom="point",size=4)
#geom_boxplot(width=0.1,outlier.shape=NA,show.legend = FALSE, position = position_nudge(x=+0.2,y=0))

plot_LR_GL=plot_LR_GL+add_pvalue(ph_LR_GL,y.position=c(7500,8500,8000),label = "p = {round(p.adj,3)} {p.adj.signif}",inherit.aes = FALSE,fontface="bold")+
  theme(axis.title=element_text(size=12,face="bold"),axis.text =element_text(size=12) )+
  ylab("Learning Rate LT")+rremove("legend")+
  scale_x_discrete(labels=c("SHAM","STIM-SD","STIM-HD"))
plot_LR_GL  

#LR BY SUB SCORE

plot_LR_Flight
data_LRSubScore=subset(data_wide,select=c("Pseudo","Group","LRFlight","LRBonus","LRMine","LRFortress"))
data_LRSubScore=melt(data_LRSubScore,id.vars=c("Pseudo","Group"))
colnames(data_LRSubScore)=c("Pseudo","Group","SousScore","LearningRate")
plot_LR_SubScore=ggplot(data_LRSubScore,aes(LR,SousScore,fill=Group,color=Group,group=Group))+theme_pubr()+scale_fill_manual(values=couleurs_alpha)+scale_color_manual(values=couleurs)+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1) ,position=position_dodge(width=0.5))+
  scale_x_discrete(labels=c("Flight","Bonus","Mine","Fortress"))+ylab("LearningRate")+xlab("Sous Score")+geom_point(alpha=0.2,position=position_dodge(width=0.5))

plot_LR_SubScore=ggplot(data_LRSubScore,aes(LR,SousScore,fill=Group,color=Group))+theme_pubr()+scale_fill_manual(values=couleurs_alpha)+scale_color_manual(values=couleurs)+
geom_half_violin(width=0.3, position = position_nudge(x=-0.2,y=0))+geom_point(position=position_dodge(width=0.5))+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=1 ,show.legend = FALSE,geom="errorbar",width=0.1,position=position_dodge(width=0.5))+
  stat_summary(fun=mean, geom="point",size=4,position=position_dodge(width=0.5))+ylab("LearningRate")+xlab("Sous Score")

plot_LR_SubScore

#PREPOST
data_prepost=read.csv("E:\\ISAE-2021\\Alldata\\PREPOST.csv",sep=";")
data_prepost$Group=factor(data_prepost$Group,levels=c("SHAM","STIMSD","STIMHD"))
data_prepost$PrePost=factor(data_prepost$PrePost,levels=c("Pre","Post"))
data_prepost$Jour=factor(data_prepost$Jour)
data_prepost$Pseudo=factor(data_prepost$Pseudo)
data_prepost$somme=data_prepost$somme-12
data_prepost$PrePost=factor(data_prepost$PrePost)
data_prepost_wide=subset(data_prepost,select=c("Pseudo","Group","PrePost","Jour","somme"))
data_prepost_wide=spread(data_prepost_wide,Jour,somme)

ggplot(data_prepost,aes(x=Group,y=somme,fill=PrePost,color=PrePost))+geom_jitter(position=position_jitterdodge(jitter.width = 0.7))+theme_pubr()+scale_fill_manual(values=couleurs_alpha)+scale_color_manual(values=couleurs)+facet_wrap(~Jour)

