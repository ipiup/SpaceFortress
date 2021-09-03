#Paper Plots
library(reshape2)

data_wide$Group=factor(data_wide$Group,levels=c("SHAM","STIMSD","STIMHD"))

couleurs=c("#868686FF","#0073C2FF","#A73030FF")
couleurs_alpha=c("#86868666","#0073C266","#A7303099")

data_long$Session=factor(data_long$Session)


#Demographie mean+sem
bxp_NET=ggplot(data_wide,aes(Group,NET,color=Group,fill=Group,shape=Group))+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=0.7 ,show.legend = FALSE,geom="errorbar",width=0.2)+
  geom_half_violin(position = position_nudge(x=-0.2,y=0))+geom_point(alpha=0.3,show.legend = FALSE)+
  stat_summary(fun=mean, geom="point",size=2,show.legend = FALSE)+theme_pubr()+rremove("legend")+
  scale_fill_manual(values=couleurs_alpha)+scale_color_manual(values=couleurs)+xlab("")+ylab("Education Level")+
  theme(axis.title=element_text(size=18,margin=0.1),text =element_text(size=16) )
bxp_GameLevel=ggplot(data_wide,aes(Group,GameLevel,color=Group,fill=Group,shape=Group))+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=0.7 ,show.legend = FALSE,geom="errorbar",width=0.2)+
  geom_half_violin(position = position_nudge(x=-0.2,y=0))+geom_point(alpha=0.3,show.legend = FALSE)+
  stat_summary(fun=mean, geom="point",size=2,show.legend = FALSE)+theme_pubr()+rremove("legend")+
  scale_fill_manual(values=couleurs_alpha)+scale_color_manual(values=couleurs)+xlab("")+ylab("Game Level")+
  theme(axis.title=element_text(size=18,margin=0.1),text =element_text(size=16) )
bxp_Age=ggplot(data_wide,aes(Group,Age,color=Group,fill=Group,shape=Group))+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=0.7 ,show.legend = FALSE,geom="errorbar",width=0.2)+
  geom_half_violin(position = position_nudge(x=-0.2,y=0))+geom_point(alpha=0.3,show.legend = FALSE)+
  stat_summary(fun=mean, geom="point",size=2,show.legend = FALSE)+theme_pubr()+rremove("legend")+
  scale_fill_manual(values=couleurs_alpha)+scale_color_manual(values=couleurs)+xlab("")+
  theme(axis.title=element_text(size=18,margin=0.1),text =element_text(size=16) )
bxp_ScoreJ1=ggplot(data_wide,aes(Group,D01P1,color=Group,fill=Group,shape=Group))+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=0.7 ,show.legend = FALSE,geom="errorbar",width=0.2)+
  geom_half_violin(position = position_nudge(x=-0.2,y=0))+geom_point(alpha=0.3,show.legend = FALSE)+
  stat_summary(fun=mean, geom="point",size=2,show.legend = FALSE)+theme_pubr()+rremove("legend")+
  scale_fill_manual(values=couleurs_alpha)+scale_color_manual(values=couleurs)+xlab("")+ylab("First Session")+
  theme(axis.title=element_text(size=18,margin=0.1),text =element_text(size=16) )
bxp_ScoreJ6=ggplot(data_wide,aes(Group,D14P2,color=Group,fill=Group,shape=Group))+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=0.7 ,show.legend = FALSE,geom="errorbar",width=0.2)+
  geom_half_violin(position = position_nudge(x=-0.2,y=0))+geom_point(alpha=0.3,show.legend = FALSE)+
  stat_summary(fun=mean, geom="point",size=2,show.legend = FALSE)+theme_pubr()+rremove("legend")+
  scale_fill_manual(values=couleurs_alpha)+scale_color_manual(values=couleurs)+xlab("")+ylab("Last Session")+
  theme(axis.title=element_text(size=18,margin=0.1),text =element_text(size=16) )
bxp_Gender=ggplot(data_wide,aes(Group,Gender,color=Group,fill=Group,shape=Group))+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=0.7 ,show.legend = FALSE,geom="errorbar",width=0.2)+
  geom_half_violin(position = position_nudge(x=-0.2,y=0))+geom_point(alpha=0.3,show.legend = FALSE)+
  stat_summary(fun=mean, geom="point",size=2,show.legend = FALSE)+theme_pubr()+rremove("legend")+
  scale_fill_manual(values=couleurs_alpha)+scale_color_manual(values=couleurs)+xlab("")+ylab("Gender")+
  theme(axis.title=element_text(size=18,margin=0.1),text =element_text(size=16) )
demo=ggarrange(bxp_NET,bxp_GameLevel,bxp_ScoreJ1,bxp_ScoreJ6,ncol=2,nrow=2,align="v")
demo
ggsave(plot=demo,"Paper\\FINAL\\Demographics.pdf",device="pdf",width=10,height=6)

#TotalScore
ggplot(data_long,aes(Group,TotalScore,color=Group,fill=Group))+
  theme_pubr()+scale_fill_manual(values=couleurs_alpha)+
  scale_color_manual(values=couleurs)+
  geom_half_violin(position = position_nudge(x=-0.2,y=0))+geom_point(alpha=0.3,show.legend = FALSE)+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=0.7 ,show.legend = FALSE,geom="errorbar",width=0.3)+
  stat_summary(fun=mean, geom="point",size=2,show.legend = FALSE)+facet_grid(~D,switch="both")+xlab("")+theme(strip.background=element_rect(color="white",fill="lightgrey"),axis.line.x = element_line(color="white"),axis.text.x = element_blank(),axis.ticks.x=element_blank())+
  scale_y_continuous(breaks=seq(-6000,22500,4000))

ggplot(data_long,aes(D,TotalScore,color=Group,group=Group))+theme_pubr()+
  stat_summary(geom="point",fun="mean",size=3,position=position_dodge(width=0.5))+scale_color_manual(values=couleurs)+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),position=position_dodge(width=0.5),size=0.3 ,show.legend = FALSE,geom="errorbar",width=0.3)+
  xlab("Session")+scale_x_continuous(breaks=1:11)

ggplot(data_long,aes(D,TotalScore,color=Group,group=Group,shape=Group))+theme_pubr()+
  geom_rect(data=data_long,aes(xmin=1.5,xmax=7.5,ymin=-Inf,ymax=+Inf),fill="grey",alpha=0.01,inherit.aes = FALSE)+
  stat_summary(geom="point",fun="mean",size=3)+stat_summary(geom="line",fun="mean",show.legend = FALSE)+
  scale_color_manual(values=couleurs)+
  geom_vline(xintercept = seq(1.5,7.5,2),linetype="dotted",alpha=0.5)+
  geom_vline(xintercept =9.5,alpha=0.3,linetype="solid",size=0.5)+
  scale_x_continuous(sec.axis=sec_axis(~.,breaks=c(1,4.5,8.5,10.5),labels=c("Baseline","Training","Short-term","Long-term")),breaks=1:11)+
  xlab("Sessions")+
  theme(axis.title=element_text(size=14),text =element_text(size=12),legend.position = c(0.71,0.15),legend.title = element_blank())

#Halves plots
#DELTA
ph_DeltaD1D5=data_wide%>%
  emmeans_test(DeltaD1D5~Group,covariate = GameLevelLog,p.adjust.method = "holm")
ph_DeltaD1D14=data_wide%>%
  emmeans_test(DeltaD1D14~Group,covariate = GameLevelLog,p.adjust.method = "holm")
ph_DeltaD14D5=data_wide%>%
  emmeans_test(DeltaD14D5~Group,covariate = GameLevelLog,p.adjust.method = "holm")
ph_DeltaD14D5$p.adj.signif[ph_DeltaD14D5$p.adj.signif=="ns"]=""
ph_DeltaD1D14$p.adj.signif[ph_DeltaD1D14$p.adj.signif=="ns"]=""
ph_DeltaD1D5$p.adj.signif[ph_DeltaD1D5$p.adj.signif=="ns"]=""

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
             label = "p = {round(p.adj,3)} {p.adj.signif}", inherit.aes = FALSE,fontface="bold")+
  theme(axis.title=element_text(size=12,face="bold"),axis.text =element_text(size=12) )+
  ylab("Delta Performance (Day 14 - Day 5)")+rremove("legend")+scale_x_discrete(labels=c("SHAM","STIM-SD","STIM-HD"))+
  scale_y_continuous( breaks=seq(-4000,8000,2000))
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
             label = "p = {round(p.adj,3)} {p.adj.signif}", inherit.aes = FALSE,fontface="bold")+
  theme(axis.title=element_text(size=12,face="bold"),axis.text =element_text(size=12) )+
  ylab("Delta Performance (Day 14 - Day 1)")+rremove("legend")+scale_x_discrete(labels=c("SHAM","STIM-SD","STIM-HD"))+scale_y_continuous( breaks=seq(0,20000,2000))
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
             label = "p = {round(p.adj,3)} {p.adj.signif}", inherit.aes = FALSE,fontface="bold")+
  theme(axis.title=element_text(size=12,face="bold"),axis.text =element_text(size=12) )+
  ylab("Delta Performance (Day 5 - Day 1)")+rremove("legend")+scale_x_discrete(labels=c("SHAM","STIM-SD","STIM-HD"))+scale_y_continuous( breaks=seq(0,20000,2000))
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


#LR BY GROUP
data_long$D=1:11
fit_all_ln=lm(TotalScore~ln(D),data=data_long)
fit_all_lin=lm(TotalScore~D,data=data_long)
AIC(fit_all_ln)
AIC(fit_all_lin)
plot_LR=ggplot(data_long,aes(D,TotalScore,color=Group,group=Group))+geom_point(position=position_dodge(width=0.5),alpha=0.3)+theme_classic2()+annotate("text",label=paste("Equation :","y~ln(x)"),x=3,y=2)+
  stat_smooth(method=lm,formula=y~ln(x),se=FALSE)+stat_summary(geom="point",fun="mean",size=3)+labs(x="Sessions")+
  scale_x_continuous(breaks=1:11)+scale_color_manual(values=couleurs)
plot_LR

p1=ggplot(data_long,aes(D,Flight,color=Group,group=Group))+theme_classic2()+annotate("text",label=paste("Equation :","y~ln(x)"),x=3,y=2)+
  stat_smooth(method=lm,formula=y~poly(x,2),se=FALSE)+stat_summary(geom="point",fun="mean",size=3)+labs(x="Sessions")+
  scale_x_continuous(breaks=1:11)+scale_color_manual(values=couleurs)

p2=ggplot(data_long,aes(D,Bonus,color=Group,group=Group))+theme_classic2()+annotate("text",label=paste("Equation :","y~ln(x)"),x=3,y=2)+
  stat_smooth(method=lm,formula=y~ln(x),se=FALSE)+stat_summary(geom="point",fun="mean",size=3)+labs(x="Sessions")+
  scale_x_continuous(breaks=1:11)+scale_color_manual(values=couleurs)
p3=ggplot(data_long,aes(D,Mine,color=Group,group=Group))+theme_classic2()+annotate("text",label=paste("Equation :","y~ln(x)"),x=3,y=2)+
  stat_smooth(method=lm,formula=y~ln(x),se=FALSE)+stat_summary(geom="point",fun="mean",size=3)+labs(x="Sessions")+
  scale_x_continuous(breaks=1:11)+scale_color_manual(values=couleurs)
p4=ggplot(data_long,aes(D,Fortress,color=Group,group=Group))+theme_classic2()+annotate("text",label=paste("Equation :","y~x"),x=3,y=2)+
  stat_smooth(method=lm,formula=y~x,se=FALSE)+stat_summary(geom="point",fun="mean",size=3)+labs(x="Sessions")+
  scale_x_continuous(breaks=1:11)+scale_color_manual(values=couleurs)

ggarrange(p1,p2,p3,p4,ncol=2,nrow=2)

data_wide%>%
  anova_test(LRFortress~Group)
data_wide%>%
  anova_test(LRFortress~Group+GameLevelLog)

#LR BY SUB SCORE
data_LRSubScore=subset(data_wide,select=c("Pseudo","Group","LRFlight","LRBonus","LRMine","LRFortress"))
data_LRSubScore=melt(data_LRSubScore,id.vars=c("Pseudo","Group"))
colnames(data_LRSubScore)=c("Pseudo","Group","SousScore","LR")
plot_LR_SubScore=ggplot(data_LRSubScore,aes(LR,SousScore,fill=Group,color=Group,group=Group))+theme_pubr()+scale_fill_manual(values=couleurs_alpha)+scale_color_manual(values=couleurs)+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1) ,position=position_dodge(width=0.5))+
  scale_x_discrete(labels=c("Flight","Bonus","Mine","Fortress"))+ylab("LearningRate")+xlab("Sous Score")+geom_point(alpha=0.2,position=position_dodge(width=0.5))

plot_LR_SubScore=ggplot(data_LRSubScore,aes(LR,SousScore,fill=Group,color=Group))+theme_pubr()+scale_fill_manual(values=couleurs_alpha)+scale_color_manual(values=couleurs)+
geom_half_violin(width=0.3, position = position_nudge(x=-0.2,y=0))+geom_point(position=position_dodge(width=0.5))+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=1 ,show.legend = FALSE,geom="errorbar",width=0.1,position=position_dodge(width=0.5))+
  stat_summary(fun=mean, geom="point",size=4,position=position_dodge(width=0.5))+ylab("LearningRate")+xlab("Sous Score")

plot_LR_SubScore

##SOUS SCORE PAR SESSION
data_long$Day=rep(c(1,2,2,3,3,4,4,5,5,14,14),61)
data_long$D=1:11
plot_Flight=ggplot(data_long,aes(D,Flight,color=Group,group=Group))+theme_pubr()+geom_rect(data=data_long,aes(xmin=1.5,xmax=7.5,ymin=-Inf,ymax=+Inf),fill="grey",alpha=0.01,inherit.aes = FALSE)+
  stat_summary(geom="point",fun="mean",size=3)+stat_summary(geom="line",fun="mean",show.legend = FALSE)+scale_color_manual(values=couleurs)+
  geom_vline(xintercept = seq(1.5,7.5,2),linetype="dotted",alpha=0.5)+geom_vline(xintercept =9.5,alpha=0.3,linetype="solid",size=0.5)+
  scale_x_continuous(sec.axis=sec_axis(~.,breaks=c(1,4.5,8.5,10.5),labels=c("Baseline","Training","Short-term","Long-term")),breaks=1:11)+
  xlab("Session")
plot_Bonus=ggplot(data_long,aes(D,Bonus,color=Group,group=Group))+theme_pubr()+geom_rect(data=data_long,aes(xmin=1.5,xmax=7.5,ymin=-Inf,ymax=+Inf),fill="grey",alpha=0.01,inherit.aes = FALSE)+
  stat_summary(geom="point",fun="mean",size=3)+stat_summary(geom="line",fun="mean",show.legend = FALSE)+scale_color_manual(values=couleurs)+
  geom_vline(xintercept = seq(1.5,7.5,2),linetype="dotted",alpha=0.5)+geom_vline(xintercept =9.5,alpha=0.3,linetype="solid",size=0.5)+
  scale_x_continuous(sec.axis=sec_axis(~.,breaks=c(1,4.5,8.5,10.5),labels=c("Baseline","Training","Short-term","Long-term")),breaks=1:11)+
  xlab("Session") 
plot_Mine=ggplot(data_long,aes(D,Mine,color=Group,group=Group))+theme_pubr()+geom_rect(data=data_long,aes(xmin=1.5,xmax=7.5,ymin=-Inf,ymax=+Inf),fill="grey",alpha=0.01,inherit.aes = FALSE)+
  stat_summary(geom="point",fun="mean",size=3)+stat_summary(geom="line",fun="mean",show.legend = FALSE)+scale_color_manual(values=couleurs)+
  geom_vline(xintercept = seq(1.5,7.5,2),linetype="dotted",alpha=0.5)+geom_vline(xintercept =9.5,alpha=0.3,linetype="solid",size=0.5)+
  scale_x_continuous(sec.axis=sec_axis(~.,breaks=c(1,4.5,8.5,10.5),labels=c("Baseline","Training","Short-term","Long-term")),breaks=1:11)+
  xlab("Session")
plot_Fortress=ggplot(data_long,aes(D,Fortress,color=Group,group=Group))+theme_pubr()+geom_rect(data=data_long,aes(xmin=1.5,xmax=7.5,ymin=-Inf,ymax=+Inf),fill="grey",alpha=0.01,inherit.aes = FALSE)+
  stat_summary(geom="point",fun="mean",size=3)+stat_summary(geom="line",fun="mean",show.legend = FALSE)+scale_color_manual(values=couleurs)+
  geom_vline(xintercept = seq(1.5,7.5,2),linetype="dotted",alpha=0.5)+geom_vline(xintercept =9.5,alpha=0.3,linetype="solid",size=0.5)+
  scale_x_continuous(sec.axis=sec_axis(~.,breaks=c(1,4.5,8.5,10.5),labels=c("Baseline","Training","Short-term","Long-term")),breaks=1:11)+
  xlab("Session")
ggarrange(plot_Flight,plot_Bonus,plot_Mine,plot_Fortress,ncol=2,nrow=2,common.legend = TRUE)


plot_Flight=ggplot(data_long,aes(D,Flight,color=Group,group=Group))+theme_pubr()+
  geom_rect(data=data_long,aes(xmin=1.5,xmax=7.5,ymin=-Inf,ymax=+Inf),fill="grey",alpha=0.01,inherit.aes = FALSE)+
  stat_summary(geom="point",fun="mean",size=3 ,position=position_dodge(width=0.3))+
  stat_summary(geom="line" ,position=position_dodge(width=0.3),fun="mean" ,show.legend = FALSE)+
  scale_color_manual(values=couleurs)+geom_vline(xintercept = seq(1.5,7.5,2),linetype="dotted",alpha=0.5)+
  geom_vline(xintercept =9.5,alpha=0.3,linetype="solid",size=0.5)+xlab("Sessions")+
  scale_x_continuous(sec.axis=sec_axis(~.,breaks=c(1,4.5,8.5,10.5),labels=c("Baseline","Training","Short-term","Long-term")),breaks=1:11)+
  stat_summary(fun.data = "mean_se",geom="errorbar",width=0.2, fun.args = list(mult = 1) ,position=position_dodge(width=0.3))
plot_Bonus=ggplot(data_long,aes(D,Bonus,color=Group,group=Group))+theme_pubr()+geom_rect(data=data_long,aes(xmin=1.5,xmax=7.5,ymin=-Inf,ymax=+Inf),fill="grey",alpha=0.01,inherit.aes = FALSE)+
  stat_summary(geom="point",fun="mean",size=3 ,position=position_dodge(width=0.3))+stat_summary(geom="line" ,position=position_dodge(width=0.3),fun="mean" ,show.legend = FALSE)+scale_color_manual(values=couleurs)+
  geom_vline(xintercept = seq(1.5,7.5,2),linetype="dotted",alpha=0.5)+geom_vline(xintercept =9.5,alpha=0.3,linetype="solid",size=0.5)+
  scale_x_continuous(sec.axis=sec_axis(~.,breaks=c(1,4.5,8.5,10.5),labels=c("Baseline","Training","Short-term","Long-term")),breaks=1:11)+
  xlab("Session")+stat_summary(fun.data = "mean_se",geom="errorbar",width=0.2, fun.args = list(mult = 1) ,position=position_dodge(width=0.3))
plot_Mine=ggplot(data_long,aes(D,Mine,color=Group,group=Group))+theme_pubr()+geom_rect(data=data_long,aes(xmin=1.5,xmax=7.5,ymin=-Inf,ymax=+Inf),fill="grey",alpha=0.01,inherit.aes = FALSE)+
  stat_summary(geom="point",fun="mean",size=3 ,position=position_dodge(width=0.3))+stat_summary(geom="line" ,position=position_dodge(width=0.3),fun="mean" ,show.legend = FALSE)+scale_color_manual(values=couleurs)+
  geom_vline(xintercept = seq(1.5,7.5,2),linetype="dotted",alpha=0.5)+geom_vline(xintercept =9.5,alpha=0.3,linetype="solid",size=0.5)+
  scale_x_continuous(sec.axis=sec_axis(~.,breaks=c(1,4.5,8.5,10.5),labels=c("Baseline","Training","Short-term","Long-term")),breaks=1:11)+
  xlab("Session")+stat_summary(fun.data = "mean_se",geom="errorbar",width=0.2, fun.args = list(mult = 1) ,position=position_dodge(width=0.3))
plot_Fortress=ggplot(data_long,aes(D,Fortress,color=Group,group=Group))+theme_pubr()+geom_rect(data=data_long,aes(xmin=1.5,xmax=7.5,ymin=-Inf,ymax=+Inf),fill="grey",alpha=0.01,inherit.aes = FALSE)+
  stat_summary(geom="point",fun="mean",size=3 ,position=position_dodge(width=0.3))+stat_summary(geom="line" ,position=position_dodge(width=0.3),fun="mean" ,show.legend = FALSE)+scale_color_manual(values=couleurs)+
  geom_vline(xintercept = seq(1.5,7.5,2),linetype="dotted",alpha=0.5)+geom_vline(xintercept =9.5,alpha=0.3,linetype="solid",size=0.5)+
  scale_x_continuous(sec.axis=sec_axis(~.,breaks=c(1,4.5,8.5,10.5),labels=c("Baseline","Training","Short-term","Long-term")),breaks=1:11)+
  xlab("Session")+stat_summary(fun.data = "mean_se",geom="errorbar",width=0.2, fun.args = list(mult = 1) ,position=position_dodge(width=0.3))
ggarrange(plot_Flight,plot_Bonus,plot_Mine,plot_Fortress,ncol=2,nrow=2,common.legend = TRUE)


#Delta Frotress
ggplot(data_wide,aes(Group,DeltaD5D1Fortress,color=Group,fill=Group))+geom_boxplot()+ylab("Fortress Delta (D5 - D1)")+
  scale_color_manual(values=couleurs)+scale_fill_manual(values=couleurs_alpha)+theme_pubr()
ggplot(data_wide,aes(Group,DeltaD14D5Fortress,color=Group,fill=Group))+geom_boxplot()+ylab("Fortress Delta (D14 - D5)")+
  scale_color_manual(values=couleurs)+scale_fill_manual(values=couleurs_alpha)+theme_pubr()
ggplot(data_wide,aes(Group,DeltaD1D14Fortress,color=Group,fill=Group))+geom_boxplot()+ylab("Fortress Delta (D14 - D1)")+
  scale_color_manual(values=couleurs)+scale_fill_manual(values=couleurs_alpha)+theme_pubr()
