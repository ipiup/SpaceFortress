library(ggExtra)

df_APM=data.frame(Pseudo=rep(df_APM_ScM$Pseudo,each=540),GameLevel=rep(df_APM_ScM$GameLevel,each=540),Group=rep(df_APM_ScM$Group,each=540),Session=rep(df_APM_ScM$Session,each=540),ScM=unlist(df_APM_ScM$ScoresMin),APM=unlist(df_APM_ScM$APM))
df_APM=data.frame(Pseudo=rep(df_APM_ScM$Pseudo,each=540),Group=rep(df_APM_ScM$Group,each=540),Session=rep(df_APM_ScM$Session,each=540),ScM=unlist(df_APM_ScM$ScoresMin),APM=unlist(df_APM_ScM$APM))

df_APM$Group=factor(df_APM$Group,levels=c("SHAM","STIMSD","STIMHD"))

ggplot(filter(df_APM,Session=="D14P2"), aes(x=ScM, y=APM) ) +
  geom_bin2d(bins=150)+scale_fill_gradient(low="lightgrey",high="black")+
  theme_pubr()+xlab("Score per Minute")+ylab("Action per Minute")+
  annotate(geom="text",label="r = .7, p <.001",x=-500,y=300)+
  geom_smooth(method="lm",color="red",size=1)
#stat_cor(method = "pearson")

#figure1
d=filter(df_APM,Session=="D01P1"|Session=="D14P2")
d=filter(d,Group!="STIMSD")

d$G = paste(d$Session,d$Group)
#ggplot(d, aes(x=ScM, y=APM,color=G))+
 # geom_point(alpha=0.2)+xlab("Score per Minute")+ylab("Action per Minute")+theme_pubr()


mean_d=d%>%
  group_by(Pseudo,Group,Session)%>%
  summarise(m_APM=mean(APM),m_ScM=mean(ScM))
ggplot(mean_d,aes(m_ScM,m_APM,color=Group,shape=Group))+geom_point(size=2)+
  xlab("Score per Minute")+ylab("Action per Minute")+theme_pubr()+facet_grid(~Session)+stat_smooth(method=lm)


mean_d_=df_APM%>%
  group_by(Group,Session)%>%
  summarise(m_APM=mean(APM),m_ScM=mean(ScM))
ggplot(mean_d_,aes(Session,m_ScM, color=Group,group=Group))+geom_point(size=2)+
  xlab("Session")+ylab("Score per Minute")+theme_pubr()+stat_smooth(method=lm)

ggplot(mean_d_,aes(Session,m_APM, color=Group,group=Group))+geom_point(size=2)+
  xlab("Session")+ylab("Action per Minute")+theme_pubr()+stat_smooth(method=lm)


#figure 2
mean_d = df_APM%>%
  group_by(Pseudo,Group,Session)%>%
  summarise(m_APM=mean(APM),m_ScM=mean(ScM))

mean_d$Pseudo=as.factor(mean_d$Pseudo)
mean_d$Group=factor(mean_d$Group,levels=c("SHAM","STIMSD","STIMHD"))
mean_d=as.data.frame(mean_d)
mean_d=subset(mean_d,Group!="STIMSD")
mean_d$day=1:11

p_APM=ggplot(mean_d,aes(day,m_APM,color=Group,shape=Group))+theme_pubr()+
  scale_x_continuous(sec.axis=sec_axis(~.,breaks=c(1,4.5,8.5,10.5),labels=c("Référence","Entraînement & stimulation","Court terme","Long terme")),breaks=1:11)+
  geom_rect(data=data_long,aes(xmin=1.5,xmax=7.5,ymin=-Inf,ymax=+Inf),fill="grey",alpha=0.01,inherit.aes = FALSE)+
  geom_vline(xintercept = seq(1.5,7.5,2),linetype="dotted",alpha=0.5)+geom_vline(xintercept =9.5,alpha=0.3,linetype="solid",size=0.5)+
  stat_smooth(method=lm,formula=y~ln(x),se=FALSE,show.legend = FALSE )+
  stat_summary(geom="point",fun="mean",size=3 ,position=position_dodge(width=0.5))+
  labs(x="Session de Jeu",y="Appui par Minute")+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=0.5 ,show.legend = FALSE,geom="errorbar",width=0.1 ,position=position_dodge(width=0.5))+
  #scale_y_continuous(breaks =seq(0, 15000, by = 2500))+
  theme(legend.position = c(0.815,0.15),legend.background = element_rect(fill=NA),legend.title = element_blank(),
        axis.title = element_text(size=18,margin=0.1),legend.text = element_text(size=16),text=element_text(size=16))+
  annotate("text",x=c(1,2.5,4.5,6.5,8.5,10.5),y=Inf,vjust=1.5,label=c("Jour 1","Jour 2","Jour 3","Jour 4","Jour 5","Jour 15"),size=5)
p_APM
ggsave(plot=p_APM,"Poster\\APM_SHAM_STIMHD_allSession.pdf",device="pdf",width=10,height=6)


p_ScM=ggplot(mean_d,aes(day,m_ScM,color=Group,shape=Group))+theme_pubr()+
  scale_x_continuous(sec.axis=sec_axis(~.,breaks=c(1,4.5,8.5,10.5),labels=c("Référence","Entraînement & stimulation","Court terme","Long terme")),breaks=1:11)+
  geom_rect(data=data_long,aes(xmin=1.5,xmax=7.5,ymin=-Inf,ymax=+Inf),fill="grey",alpha=0.01,inherit.aes = FALSE)+
  geom_vline(xintercept = seq(1.5,7.5,2),linetype="dotted",alpha=0.5)+geom_vline(xintercept =9.5,alpha=0.3,linetype="solid",size=0.5)+
  stat_smooth(method=lm,formula=y~ln(x),se=FALSE,show.legend = FALSE )+
  stat_summary(geom="point",fun="mean",size=3 ,position=position_dodge(width=0.5))+
  labs(x="Session de Jeu",y="Score par Minute")+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=0.5 ,show.legend = FALSE,geom="errorbar",width=0.1 ,position=position_dodge(width=0.5))+
  #scale_y_continuous(breaks =seq(0, 15000, by = 2500))+
  theme(legend.position = c(0.815,0.15),legend.background = element_rect(fill=NA),legend.title = element_blank(),
        axis.title = element_text(size=18,margin=0.1),legend.text = element_text(size=16),text=element_text(size=16))+
  annotate("text",x=c(1,2.5,4.5,6.5,8.5,10.5),y=Inf,vjust=1.5,label=c("Jour 1","Jour 2","Jour 3","Jour 4","Jour 5","Jour 15"),size=5)

ggsave(plot=p_ScM,"Poster\\ScM_SHAM_STIMHD_allSession.pdf",device="pdf",width=10,height=6)



#ttest
mean_d_D5D14 = subset(mean_d, Session=="D05P2"|Session=="D14P2")



#####
#LR by subScores
#FLIGHT

#Learning Rate by groups on FLIGHT SCORE
fit_SHAM=lm(Flight~ln(D),data=filter(data_long,Group=="SHAM"))
fit_SD=lm(Flight~ln(D),data=filter(data_long,Group=="STIMSD"))
fit_HD=lm(Flight~ln(D),data=filter(data_long,Group=="STIMHD"))


eq_SHAM=paste0("y= ",round(coef(fit_SHAM)[1])," + ",round(coef(fit_SHAM)[2]),"*ln(x)     ")
eq_SD=paste0("y= ",round(coef(fit_SD)[1])," + ",round(coef(fit_SD)[2]),"*ln(x)")
eq_HD=paste0("y= ",round(coef(fit_HD)[1])," + ",round(coef(fit_HD)[2]),"*ln(x)")
grob_SHAM=grobTree(textGrob(eq_SHAM,x=0.77,y=0.195,hjust=0,gp=gpar(col="#868686FF")),gp=gpar(fontsize=15))
grob_SD=grobTree(textGrob(eq_SD,x=0.77,y=0.145,hjust=0,gp=gpar(col="#0073C2FF")),gp=gpar(fontsize=15))
grob_HD=grobTree(textGrob(eq_HD,x=0.77,y=0.095,hjust=0,gp=gpar(col="#A73030FF")),gp=gpar(fontsize=15))

plot_LR_FLight=ggplot(data_long,aes(D,Flight,color=Group,shape=Group))+theme_pubr()+
  scale_x_continuous(sec.axis=sec_axis(~.,breaks=c(1,4.5,8.5,10.5),labels=c("Référence","Entraînement & stimulation","Court terme","Long terme")),breaks=1:11)+
  geom_rect(data=data_long,aes(xmin=1.5,xmax=7.5,ymin=-Inf,ymax=+Inf),fill="grey",alpha=0.01,inherit.aes = FALSE)+
  geom_vline(xintercept = seq(1.5,7.5,2),linetype="dotted",alpha=0.5)+geom_vline(xintercept =9.5,alpha=0.3,linetype="solid",size=0.5)+
  stat_smooth(method=lm,formula=y~ln(x),se=FALSE,show.legend = FALSE )+
  stat_summary(geom="point",fun="mean",size=3 ,position=position_dodge(width=0.5))+
  labs(x="Game Session",y="Flight Score")+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=1 ,show.legend = FALSE,geom="errorbar",width=0.1 ,position=position_dodge(width=0.5))+
  #scale_y_continuous(breaks =seq(0, 15000, by = 2500))+
  scale_colour_manual(values=couleurs)+
  annotation_custom(grob_SHAM)+ annotation_custom(grob_SD)+ annotation_custom(grob_HD)+
  theme(legend.position = c(0.7,0.15),legend.background = element_rect(fill=NA),legend.title = element_blank(),
        axis.title = element_text(size=18,margin=0.1),legend.text = element_text(size=16),text=element_text(size=16))+
  annotate("text",x=c(1,2.5,4.5,6.5,8.5,10.5),y=Inf,vjust=1.5,label=c("D1","D2","D3","D4","D5","D15"),size=5)
plot_LR_FLight

#ANCOVA with Gamelevel
ancova_LR_FLIGHT=data_wide%>%
  anova_test(LRFlight~Group+GameLevelLog) #EFFET DU GROUP p = 0.091

grob_LR=grobTree(textGrob(paste0("Group effect: p = ",toString(round(ancova_LR_FLIGHT$p[1],3)),ancova_LR_FLIGHT$`p<.05`[1]),
                          x=0.25,y=0.9,hjust=0,vjust=0),
                 text_grob(paste0("Gaming Experience effect: p = ",toString(round(ancova_LR_FLIGHT$p[2],3))),
                           x=0.25,y=0.85,hjust=0,vjust=0,face="bold")
                 ,gp=gpar(fontsize=16))

p_ancovaLR_Flight=ggplot(data_wide,aes(Group,LRFlight,color=Group,fill=Group,shape=Group))+
  theme_pubr()+scale_fill_manual(values=couleurs_alpha)+
  scale_color_manual(values=couleurs)+
  geom_half_violin(width=0.3, position = position_nudge(x=-0.2,y=0))+geom_jitter(width=0.1)+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=1 ,show.legend = FALSE,geom="errorbar",width=0.1)+
  stat_summary(fun=mean, geom="point",size=4)+
  ylab("Learning Rate Flight")+rremove("legend")+scale_x_discrete(labels=c("Sham","SD-tRNS","HD-tRNS"))+
 # scale_y_continuous( breaks=seq(-4000,8000,1000))+
  # add_pvalue(ph_LR,y.position=c(7500,8700,8100),
  # label = "p = {round(p.adj,3)} {p.adj.signif}", inherit.aes = FALSE)+
  annotation_custom(grob_LR)+
  theme(axis.title=element_text(margin=0.1,size=18),text =element_text(size=16) )
p_ancovaLR_Flight

#BONUS

#Learning Rate by groups on Bonus SCORE
fit_SHAM=lm(Bonus~ln(D),data=filter(data_long,Group=="SHAM"))
fit_SD=lm(Bonus~ln(D),data=filter(data_long,Group=="STIMSD"))
fit_HD=lm(Bonus~ln(D),data=filter(data_long,Group=="STIMHD"))


eq_SHAM=paste0("y= ",round(coef(fit_SHAM)[1])," + ",round(coef(fit_SHAM)[2]),"*ln(x)     ")
eq_SD=paste0("y= ",round(coef(fit_SD)[1])," + ",round(coef(fit_SD)[2]),"*ln(x)")
eq_HD=paste0("y= ",round(coef(fit_HD)[1])," + ",round(coef(fit_HD)[2]),"*ln(x)")
grob_SHAM=grobTree(textGrob(eq_SHAM,x=0.77,y=0.195,hjust=0,gp=gpar(col="#868686FF")),gp=gpar(fontsize=15))
grob_SD=grobTree(textGrob(eq_SD,x=0.77,y=0.145,hjust=0,gp=gpar(col="#0073C2FF")),gp=gpar(fontsize=15))
grob_HD=grobTree(textGrob(eq_HD,x=0.77,y=0.095,hjust=0,gp=gpar(col="#A73030FF")),gp=gpar(fontsize=15))

plot_LR_Bonus=ggplot(data_long,aes(D,Bonus,color=Group,shape=Group))+theme_pubr()+
  scale_x_continuous(sec.axis=sec_axis(~.,breaks=c(1,4.5,8.5,10.5),labels=c("Référence","Entraînement & stimulation","Court terme","Long terme")),breaks=1:11)+
  geom_rect(data=data_long,aes(xmin=1.5,xmax=7.5,ymin=-Inf,ymax=+Inf),fill="grey",alpha=0.01,inherit.aes = FALSE)+
  geom_vline(xintercept = seq(1.5,7.5,2),linetype="dotted",alpha=0.5)+geom_vline(xintercept =9.5,alpha=0.3,linetype="solid",size=0.5)+
  stat_smooth(method=lm,formula=y~ln(x),se=FALSE,show.legend = FALSE )+
  stat_summary(geom="point",fun="mean",size=3 ,position=position_dodge(width=0.5))+
  labs(x="Game Session",y="Bonus Score")+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=1 ,show.legend = FALSE,geom="errorbar",width=0.1 ,position=position_dodge(width=0.5))+
  #scale_y_continuous(breaks =seq(0, 15000, by = 2500))+
  scale_colour_manual(values=couleurs)+
  annotation_custom(grob_SHAM)+ annotation_custom(grob_SD)+ annotation_custom(grob_HD)+
  theme(legend.position = c(0.7,0.15),legend.background = element_rect(fill=NA),legend.title = element_blank(),
        axis.title = element_text(size=18,margin=0.1),legend.text = element_text(size=16),text=element_text(size=16))+
  annotate("text",x=c(1,2.5,4.5,6.5,8.5,10.5),y=Inf,vjust=1.5,label=c("D1","D2","D3","D4","D5","D15"),size=5)
plot_LR_Bonus

#ANCOVA with Gamelevel
ancova_LR_Bonus=data_wide%>%
  anova_test(LRBonus~Group+GameLevelLog) #EFFET DU GROUP p = 0.091

grob_LR=grobTree(textGrob(paste0("Group effect: p = ",toString(round(ancova_LR_Bonus$p[1],3)),ancova_LR_Bonus$`p<.05`[1]),
                          x=0.25,y=0.9,hjust=0,vjust=0),
                 text_grob(paste0("Gaming Experience effect: p = ",toString(round(ancova_LR_Bonus$p[2],3))),
                           x=0.25,y=0.85,hjust=0,vjust=0,face="bold")
                 ,gp=gpar(fontsize=16))

p_ancovaLR_Bonus=ggplot(data_wide,aes(Group,LRBonus,color=Group,fill=Group,shape=Group))+
  theme_pubr()+scale_fill_manual(values=couleurs_alpha)+
  scale_color_manual(values=couleurs)+
  geom_half_violin(width=0.3, position = position_nudge(x=-0.2,y=0))+geom_jitter(width=0.1)+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=1 ,show.legend = FALSE,geom="errorbar",width=0.1)+
  stat_summary(fun=mean, geom="point",size=4)+
  ylab("Learning Rate Bonus")+rremove("legend")+scale_x_discrete(labels=c("Sham","SD-tRNS","HD-tRNS"))+
  # scale_y_continuous( breaks=seq(-4000,8000,1000))+
  # add_pvalue(ph_LR,y.position=c(7500,8700,8100),
  # label = "p = {round(p.adj,3)} {p.adj.signif}", inherit.aes = FALSE)+
  annotation_custom(grob_LR)+
  theme(axis.title=element_text(margin=0.1,size=18),text =element_text(size=16) )
p_ancovaLR_Bonus


#Mine
#Learning Rate by groups on Mine SCORE
fit_SHAM=lm(Mine~ln(D),data=filter(data_long,Group=="SHAM"))
fit_SD=lm(Mine~ln(D),data=filter(data_long,Group=="STIMSD"))
fit_HD=lm(Mine~ln(D),data=filter(data_long,Group=="STIMHD"))

eq_SHAM=paste0("y= ",round(coef(fit_SHAM)[1])," + ",round(coef(fit_SHAM)[2]),"*ln(x)     ")
eq_SD=paste0("y= ",round(coef(fit_SD)[1])," + ",round(coef(fit_SD)[2]),"*ln(x)")
eq_HD=paste0("y= ",round(coef(fit_HD)[1])," + ",round(coef(fit_HD)[2]),"*ln(x)")
grob_SHAM=grobTree(textGrob(eq_SHAM,x=0.77,y=0.195,hjust=0,gp=gpar(col="#868686FF")),gp=gpar(fontsize=15))
grob_SD=grobTree(textGrob(eq_SD,x=0.77,y=0.145,hjust=0,gp=gpar(col="#0073C2FF")),gp=gpar(fontsize=15))
grob_HD=grobTree(textGrob(eq_HD,x=0.77,y=0.095,hjust=0,gp=gpar(col="#A73030FF")),gp=gpar(fontsize=15))

plot_LR_Mine=ggplot(data_long,aes(D,Mine,color=Group,shape=Group))+theme_pubr()+
  scale_x_continuous(sec.axis=sec_axis(~.,breaks=c(1,4.5,8.5,10.5),labels=c("Référence","Entraînement & stimulation","Court terme","Long terme")),breaks=1:11)+
  geom_rect(data=data_long,aes(xmin=1.5,xmax=7.5,ymin=-Inf,ymax=+Inf),fill="grey",alpha=0.01,inherit.aes = FALSE)+
  geom_vline(xintercept = seq(1.5,7.5,2),linetype="dotted",alpha=0.5)+geom_vline(xintercept =9.5,alpha=0.3,linetype="solid",size=0.5)+
  stat_smooth(method=lm,formula=y~ln(x),se=FALSE,show.legend = FALSE )+
  stat_summary(geom="point",fun="mean",size=3 ,position=position_dodge(width=0.5))+
  labs(x="Game Session",y="Mine Score")+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=1 ,show.legend = FALSE,geom="errorbar",width=0.1 ,position=position_dodge(width=0.5))+
  #scale_y_continuous(breaks =seq(0, 15000, by = 2500))+
  scale_colour_manual(values=couleurs)+
  annotation_custom(grob_SHAM)+ annotation_custom(grob_SD)+ annotation_custom(grob_HD)+
  theme(legend.position = c(0.7,0.15),legend.background = element_rect(fill=NA),legend.title = element_blank(),
        axis.title = element_text(size=18,margin=0.1),legend.text = element_text(size=16),text=element_text(size=16))+
  annotate("text",x=c(1,2.5,4.5,6.5,8.5,10.5),y=Inf,vjust=1.5,label=c("D1","D2","D3","D4","D5","D15"),size=5)
plot_LR_Mine

#ANCOVA with Gamelevel
ancova_LR_Mine=data_wide%>%
  anova_test(LRMine~Group+GameLevelLog) #EFFET DU GROUP p = 0.091

grob_LR=grobTree(textGrob(paste0("Group effect: p = ",toString(round(ancova_LR_Mine$p[1],3)),ancova_LR_Mine$`p<.05`[1]),
                          x=0.25,y=0.9,hjust=0,vjust=0),
                 text_grob(paste0("Gaming Experience effect: p = ",toString(round(ancova_LR_Mine$p[2],3))),
                           x=0.25,y=0.85,hjust=0,vjust=0,face="bold")
                 ,gp=gpar(fontsize=16))

p_ancovaLR_Mine=ggplot(data_wide,aes(Group,LRMine,color=Group,fill=Group,shape=Group))+
  theme_pubr()+scale_fill_manual(values=couleurs_alpha)+
  scale_color_manual(values=couleurs)+
  geom_half_violin(width=0.3, position = position_nudge(x=-0.2,y=0))+geom_jitter(width=0.1)+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=1 ,show.legend = FALSE,geom="errorbar",width=0.1)+
  stat_summary(fun=mean, geom="point",size=4)+
  ylab("Learning Rate Mine")+rremove("legend")+scale_x_discrete(labels=c("Sham","SD-tRNS","HD-tRNS"))+
  # scale_y_continuous( breaks=seq(-4000,8000,1000))+
  # add_pvalue(ph_LR,y.position=c(7500,8700,8100),
  # label = "p = {round(p.adj,3)} {p.adj.signif}", inherit.aes = FALSE)+
  annotation_custom(grob_LR)+
  theme(axis.title=element_text(margin=0.1,size=18),text =element_text(size=16) )
p_ancovaLR_Mine

#FORTRESS

#Learning Rate by groups on Fortress SCORE
fit_SHAM=lm(Fortress~ln(D),data=filter(data_long,Group=="SHAM"))
fit_SD=lm(Fortress~ln(D),data=filter(data_long,Group=="STIMSD"))
fit_HD=lm(Fortress~ln(D),data=filter(data_long,Group=="STIMHD"))


eq_SHAM=paste0("y= ",round(coef(fit_SHAM)[1])," + ",round(coef(fit_SHAM)[2]),"*ln(x)     ")
eq_SD=paste0("y= ",round(coef(fit_SD)[1])," + ",round(coef(fit_SD)[2]),"*ln(x)")
eq_HD=paste0("y= ",round(coef(fit_HD)[1])," + ",round(coef(fit_HD)[2]),"*ln(x)")
grob_SHAM=grobTree(textGrob(eq_SHAM,x=0.77,y=0.195,hjust=0,gp=gpar(col="#868686FF")),gp=gpar(fontsize=15))
grob_SD=grobTree(textGrob(eq_SD,x=0.77,y=0.145,hjust=0,gp=gpar(col="#0073C2FF")),gp=gpar(fontsize=15))
grob_HD=grobTree(textGrob(eq_HD,x=0.77,y=0.095,hjust=0,gp=gpar(col="#A73030FF")),gp=gpar(fontsize=15))

plot_LR_Fortress=ggplot(data_long,aes(D,Fortress,color=Group,shape=Group))+theme_pubr()+
  scale_x_continuous(sec.axis=sec_axis(~.,breaks=c(1,4.5,8.5,10.5),labels=c("Référence","Entraînement & stimulation","Court terme","Long terme")),breaks=1:11)+
  geom_rect(data=data_long,aes(xmin=1.5,xmax=7.5,ymin=-Inf,ymax=+Inf),fill="grey",alpha=0.01,inherit.aes = FALSE)+
  geom_vline(xintercept = seq(1.5,7.5,2),linetype="dotted",alpha=0.5)+geom_vline(xintercept =9.5,alpha=0.3,linetype="solid",size=0.5)+
  stat_smooth(method=lm,formula=y~ln(x),se=FALSE,show.legend = FALSE )+
  stat_summary(geom="point",fun="mean",size=3 ,position=position_dodge(width=0.5))+
  labs(x="Game Session",y="Fortress Score")+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=1 ,show.legend = FALSE,geom="errorbar",width=0.1 ,position=position_dodge(width=0.5))+
  #scale_y_continuous(breaks =seq(0, 15000, by = 2500))+
  scale_colour_manual(values=couleurs)+
  annotation_custom(grob_SHAM)+ annotation_custom(grob_SD)+ annotation_custom(grob_HD)+
  theme(legend.position = c(0.7,0.15),legend.background = element_rect(fill=NA),legend.title = element_blank(),
        axis.title = element_text(size=18,margin=0.1),legend.text = element_text(size=16),text=element_text(size=16))+
  annotate("text",x=c(1,2.5,4.5,6.5,8.5,10.5),y=Inf,vjust=1.5,label=c("D1","D2","D3","D4","D5","D15"),size=5)
plot_LR_Fortress

#ANCOVA with Gamelevel
ancova_LR_Fortress=data_wide%>%
  anova_test(LRFortress~Group+GameLevelLog) #EFFET DU GROUP p = 0.091

grob_LR=grobTree(textGrob(paste0("Group effect: p = ",toString(round(ancova_LR_Fortress$p[1],3)),ancova_LR_Fortress$`p<.05`[1]),
                          x=0.25,y=0.9,hjust=0,vjust=0),
                 text_grob(paste0("Gaming Experience effect: p = ",toString(round(ancova_LR_Fortress$p[2],3))),
                           x=0.25,y=0.85,hjust=0,vjust=0,face="bold")
                 ,gp=gpar(fontsize=16))

p_ancovaLR_Fortress=ggplot(data_wide,aes(Group,LRFortress,color=Group,fill=Group,shape=Group))+
  theme_pubr()+scale_fill_manual(values=couleurs_alpha)+
  scale_color_manual(values=couleurs)+
  geom_half_violin(width=0.3, position = position_nudge(x=-0.2,y=0))+geom_jitter(width=0.1)+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=1 ,show.legend = FALSE,geom="errorbar",width=0.1)+
  stat_summary(fun=mean, geom="point",size=4)+
  ylab("Learning Rate Fortress")+rremove("legend")+scale_x_discrete(labels=c("Sham","SD-tRNS","HD-tRNS"))+
  # scale_y_continuous( breaks=seq(-4000,8000,1000))+
  # add_pvalue(ph_LR,y.position=c(7500,8700,8100),
  # label = "p = {round(p.adj,3)} {p.adj.signif}", inherit.aes = FALSE)+
  annotation_custom(grob_LR)+
  theme(axis.title=element_text(margin=0.1,size=18),text =element_text(size=16) )
p_ancovaLR_Fortress



#Learning Rate LINEAR by groups on Fortress SCORE
fit_SHAM=lm(Fortress~D,data=filter(data_long,Group=="SHAM"))
fit_SD=lm(Fortress~D,data=filter(data_long,Group=="STIMSD"))
fit_HD=lm(Fortress~D,data=filter(data_long,Group=="STIMHD"))

eq_SHAM=paste0("y= ",round(coef(fit_SHAM)[1])," + ",round(coef(fit_SHAM)[2]),"*x     ")
eq_SD=paste0("y= ",round(coef(fit_SD)[1])," + ",round(coef(fit_SD)[2]),"*x")
eq_HD=paste0("y= ",round(coef(fit_HD)[1])," + ",round(coef(fit_HD)[2]),"*x")
grob_SHAM=grobTree(textGrob(eq_SHAM,x=0.77,y=0.195,hjust=0,gp=gpar(col="#868686FF")),gp=gpar(fontsize=15))
grob_SD=grobTree(textGrob(eq_SD,x=0.77,y=0.145,hjust=0,gp=gpar(col="#0073C2FF")),gp=gpar(fontsize=15))
grob_HD=grobTree(textGrob(eq_HD,x=0.77,y=0.095,hjust=0,gp=gpar(col="#A73030FF")),gp=gpar(fontsize=15))

plot_LR_Fortress=ggplot(data_long,aes(D,Fortress,color=Group,shape=Group))+theme_pubr()+
  scale_x_continuous(sec.axis=sec_axis(~.,breaks=c(1,4.5,8.5,10.5),labels=c("Référence","Entraînement & stimulation","Court terme","Long terme")),breaks=1:11)+
  geom_rect(data=data_long,aes(xmin=1.5,xmax=7.5,ymin=-Inf,ymax=+Inf),fill="grey",alpha=0.01,inherit.aes = FALSE)+
  geom_vline(xintercept = seq(1.5,7.5,2),linetype="dotted",alpha=0.5)+geom_vline(xintercept =9.5,alpha=0.3,linetype="solid",size=0.5)+
  stat_smooth(method=lm,formula=y~x,se=FALSE,show.legend = FALSE )+
  stat_summary(geom="point",fun="mean",size=3 ,position=position_dodge(width=0.5))+
  labs(x="Game Session",y="Fortress Score")+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=1 ,show.legend = FALSE,geom="errorbar",width=0.1 ,position=position_dodge(width=0.5))+
  #scale_y_continuous(breaks =seq(0, 15000, by = 2500))+
  scale_colour_manual(values=couleurs)+
  annotation_custom(grob_SHAM)+ annotation_custom(grob_SD)+ annotation_custom(grob_HD)+
  theme(legend.position = c(0.7,0.15),legend.background = element_rect(fill=NA),legend.title = element_blank(),
        axis.title = element_text(size=18,margin=0.1),legend.text = element_text(size=16),text=element_text(size=16))+
  annotate("text",x=c(1,2.5,4.5,6.5,8.5,10.5),y=Inf,vjust=1.5,label=c("D1","D2","D3","D4","D5","D15"),size=5)
plot_LR_Fortress


#Learning Rate LINEAR by groups on Mine SCORE
fit_SHAM=lm(Mine~D,data=filter(data_long,Group=="SHAM"))
fit_SD=lm(Mine~D,data=filter(data_long,Group=="STIMSD"))
fit_HD=lm(Mine~D,data=filter(data_long,Group=="STIMHD"))

eq_SHAM=paste0("y= ",round(coef(fit_SHAM)[1])," + ",round(coef(fit_SHAM)[2]),"*x     ")
eq_SD=paste0("y= ",round(coef(fit_SD)[1])," + ",round(coef(fit_SD)[2]),"*x")
eq_HD=paste0("y= ",round(coef(fit_HD)[1])," + ",round(coef(fit_HD)[2]),"*x")
grob_SHAM=grobTree(textGrob(eq_SHAM,x=0.77,y=0.195,hjust=0,gp=gpar(col="#868686FF")),gp=gpar(fontsize=15))
grob_SD=grobTree(textGrob(eq_SD,x=0.77,y=0.145,hjust=0,gp=gpar(col="#0073C2FF")),gp=gpar(fontsize=15))
grob_HD=grobTree(textGrob(eq_HD,x=0.77,y=0.095,hjust=0,gp=gpar(col="#A73030FF")),gp=gpar(fontsize=15))

plot_LR_Mine=ggplot(data_long,aes(D,Mine,color=Group,shape=Group))+theme_pubr()+
  scale_x_continuous(sec.axis=sec_axis(~.,breaks=c(1,4.5,8.5,10.5),labels=c("Référence","Entraînement & stimulation","Court terme","Long terme")),breaks=1:11)+
  geom_rect(data=data_long,aes(xmin=1.5,xmax=7.5,ymin=-Inf,ymax=+Inf),fill="grey",alpha=0.01,inherit.aes = FALSE)+
  geom_vline(xintercept = seq(1.5,7.5,2),linetype="dotted",alpha=0.5)+geom_vline(xintercept =9.5,alpha=0.3,linetype="solid",size=0.5)+
  stat_smooth(method=lm,formula=y~x,se=FALSE,show.legend = FALSE )+
  stat_summary(geom="point",fun="mean",size=3 ,position=position_dodge(width=0.5))+
  labs(x="Game Session",y="Mine Score")+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=1 ,show.legend = FALSE,geom="errorbar",width=0.1 ,position=position_dodge(width=0.5))+
  #scale_y_continuous(breaks =seq(0, 15000, by = 2500))+
  scale_colour_manual(values=couleurs)+
  annotation_custom(grob_SHAM)+ annotation_custom(grob_SD)+ annotation_custom(grob_HD)+
  theme(legend.position = c(0.7,0.15),legend.background = element_rect(fill=NA),legend.title = element_blank(),
        axis.title = element_text(size=18,margin=0.1),legend.text = element_text(size=16),text=element_text(size=16))+
  annotate("text",x=c(1,2.5,4.5,6.5,8.5,10.5),y=Inf,vjust=1.5,label=c("D1","D2","D3","D4","D5","D15"),size=5)
plot_LR_Mine


#### BONUS MINE PCRT

plot_Flight=ggplot(filter(data_long,!grepl("P1",Session)&Group!="STIMSD"),aes(D,Flight,color=Group,group=Group))+theme_pubr()+
  geom_rect(data=data_long,aes(xmin=1.5,xmax=7.5,ymin=-Inf,ymax=+Inf),fill="grey",alpha=0.01,inherit.aes = FALSE)+
  stat_summary(geom="point",fun="mean",size=3 ,position=position_dodge(width=0.3))+
  stat_summary(geom="line" ,position=position_dodge(width=0.3),fun="mean" ,show.legend = FALSE)+
  scale_color_manual(values=couleurs)+geom_vline(xintercept = seq(1.5,7.5,2),linetype="dotted",alpha=0.5)+
  geom_vline(xintercept =9.5,alpha=0.3,linetype="solid",size=0.5)+xlab("Sessions")+
  scale_x_continuous(sec.axis=sec_axis(~.,breaks=c(1,4.5,8.5,10.5),labels=c("Baseline","Training","Short-term","Long-term")),breaks=1:11)
#stat_summary(fun.data = "mean_se",geom="errorbar",width=0.2, fun.args = list(mult = 1) ,position=position_dodge(width=0.3))
plot_Flight


plot_Bonus=ggplot(filter(data_long,!grepl("P1",Session)&Group!="STIMSD"),aes(D,Bonus_Prct,color=Group,group=Group))+theme_pubr()+geom_rect(data=data_long,aes(xmin=1.5,xmax=7.5,ymin=-Inf,ymax=+Inf),fill="grey",alpha=0.01,inherit.aes = FALSE)+
  stat_summary(geom="point",fun="mean",size=3 ,position=position_dodge(width=0.3))+stat_summary(geom="line" ,position=position_dodge(width=0.3),fun="mean" ,show.legend = FALSE)+scale_color_manual(values=couleurs)+
  geom_vline(xintercept = seq(1.5,7.5,2),linetype="dotted",alpha=0.5)+geom_vline(xintercept =9.5,alpha=0.3,linetype="solid",size=0.5)+
  scale_x_continuous(sec.axis=sec_axis(~.,breaks=c(1,4.5,8.5,10.5),labels=c("Baseline","Training","Short-term","Long-term")),breaks=1:11)+
  xlab("Session")#+stat_summary(fun.data = "mean_se",geom="errorbar",width=0.2, fun.args = list(mult = 1) ,position=position_dodge(width=0.3))
plot_Bonus


plot_Mine=ggplot(filter(data_long,!grepl("P1",Session)&Group!="STIMSD"),aes(D,Mine_Prct,color=Group,group=Group))+theme_pubr()+geom_rect(data=data_long,aes(xmin=1.5,xmax=7.5,ymin=-Inf,ymax=+Inf),fill="grey",alpha=0.01,inherit.aes = FALSE)+
  stat_summary(geom="point",fun="mean",size=3 ,position=position_dodge(width=0.3))+stat_summary(geom="line" ,position=position_dodge(width=0.3),fun="mean" ,show.legend = FALSE)+scale_color_manual(values=couleurs)+
  geom_vline(xintercept = seq(1.5,7.5,2),linetype="dotted",alpha=0.5)+geom_vline(xintercept =9.5,alpha=0.3,linetype="solid",size=0.5)+
  scale_x_continuous(sec.axis=sec_axis(~.,breaks=c(1,4.5,8.5,10.5),labels=c("Baseline","Training","Short-term","Long-term")),breaks=1:11)+
  xlab("Session")#+stat_summary(fun.data = "mean_se",geom="errorbar",width=0.2, fun.args = list(mult = 1) ,position=position_dodge(width=0.3))
plot_Mine

plot_Fortress=ggplot(filter(data_long,!grepl("P1",Session)&Group!="STIMSD"),aes(D,Fortress,color=Group,group=Group))+theme_pubr()+geom_rect(data=data_long,aes(xmin=1.5,xmax=7.5,ymin=-Inf,ymax=+Inf),fill="grey",alpha=0.01,inherit.aes = FALSE)+
  stat_summary(geom="point",fun="mean",size=3 ,position=position_dodge(width=0.3))+stat_summary(geom="line" ,position=position_dodge(width=0.3),fun="mean" ,show.legend = FALSE)+scale_color_manual(values=couleurs)+
  geom_vline(xintercept = seq(1.5,7.5,2),linetype="dotted",alpha=0.5)+geom_vline(xintercept =9.5,alpha=0.3,linetype="solid",size=0.5)+
  scale_x_continuous(sec.axis=sec_axis(~.,breaks=c(1,4.5,8.5,10.5),labels=c("Baseline","Training","Short-term","Long-term")),breaks=1:11)+
  xlab("Session")#+stat_summary(fun.data = "mean_se",geom="errorbar",width=0.2, fun.args = list(mult = 1) ,position=position_dodge(width=0.3))
ggarrange(plot_Flight,plot_Bonus,plot_Mine,plot_Fortress,ncol=2,nrow=2,common.legend = TRUE)

plot_APM=ggplot(filter(data_long,!grepl("P1",Session)&Group!="STIMSD"),aes(D,APM,color=Group,group=Group))+theme_pubr()+geom_rect(data=data_long,aes(xmin=1.5,xmax=7.5,ymin=-Inf,ymax=+Inf),fill="grey",alpha=0.01,inherit.aes = FALSE)+
  stat_summary(geom="point",fun="mean",size=3 ,position=position_dodge(width=0.3))+stat_summary(geom="line" ,position=position_dodge(width=0.3),fun="mean" ,show.legend = FALSE)+scale_color_manual(values=couleurs)+
  geom_vline(xintercept = seq(1.5,7.5,2),linetype="dotted",alpha=0.5)+geom_vline(xintercept =9.5,alpha=0.3,linetype="solid",size=0.5)+
  scale_x_continuous(sec.axis=sec_axis(~.,breaks=c(1,4.5,8.5,10.5),labels=c("Baseline","Training","Short-term","Long-term")),breaks=1:11)+
  xlab("Session")
plot_APM


#####
#FIGURE
couleurs_poster = c("#0073C2FF","#A73030FF")
data_long=subset(data_long,Group!="STIMSD")%>%arrange(Pseudo)
data_long$D=1:11
data_long$Pseudo=factor(data_long$Pseudo)
data_long$Session = factor(data_long$Session)
#LR 
#Learning Rate by groups
fit_SHAM=lm(TotalScore~ln(D),data=filter(data_long,Group=="SHAM"))
fit_HD=lm(TotalScore~ln(D),data=filter(data_long,Group=="STIMHD"))

eq_SHAM=paste0("y= ",round(coef(fit_SHAM)[1])," + ",round(coef(fit_SHAM)[2]),"*ln(x)     ")
eq_HD=paste0("y= ",round(coef(fit_HD)[1])," + ",round(coef(fit_HD)[2]),"*ln(x)")
grob_SHAM=grobTree(textGrob(eq_SHAM,x=0.77,y=0.17,hjust=0,gp=gpar(col="#0073C2FF")),gp=gpar(fontsize=15))
grob_HD=grobTree(textGrob(eq_HD,x=0.77,y=0.12,hjust=0,gp=gpar(col="#A73030FF")),gp=gpar(fontsize=15))

plot_LR=ggplot(data_long,aes(D,TotalScore,color=Group,shape=Group))+theme_pubr()+
  scale_x_continuous(sec.axis=sec_axis(~.,breaks=c(1,4.5,8.5,10.5),labels=c("Reference","Training & Stimulation","Short-term","Long-term")),breaks=1:11)+
  geom_rect(data=data_long,aes(xmin=1.5,xmax=7.5,ymin=-Inf,ymax=+Inf),fill="grey",alpha=0.01,inherit.aes = FALSE)+
  geom_vline(xintercept = seq(1.5,7.5,2),linetype="dotted",alpha=0.5)+geom_vline(xintercept =9.5,alpha=0.3,linetype="solid",size=0.5)+
  stat_smooth(method=lm,formula=y~ln(x),se=FALSE,show.legend = FALSE )+
  stat_summary(geom="point",fun="mean",size=3 ,position=position_dodge(width=0.5))+
  labs(x="Game Session",y="Total Score")+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=1 ,show.legend = FALSE,geom="errorbar",width=0.1 ,position=position_dodge(width=0.5))+
  scale_y_continuous(breaks =seq(0, 15000, by = 2500))+
  scale_colour_manual(values=couleurs_poster,labels=c("Sham","Stim"))+scale_shape(labels=c("Sham","Stim"))+
  annotation_custom(grob_SHAM)+annotation_custom(grob_HD)+
  theme(legend.position = c(0.7,0.15),legend.background = element_rect(fill=NA),legend.title = element_blank(),
        axis.title = element_text(size=18,margin=0.1),legend.text = element_text(size=16),text=element_text(size=16))+
  annotate("text",x=c(1,2.5,4.5,6.5,8.5,10.5),y=Inf,vjust=1.5,label=c("D1","D2","D3","D4","D5","D15"),size=5)
plot_LR


p_Fortress = ggplot(filter(data_long,Session=="D01P1"|Session=="D05P2"|Session=="D14P2"),aes(Session,DestroyedFortress,color=Group,group=Group))+theme_pubr()+
  geom_point(alpha=0.4,position=position_jitterdodge(jitter.width = 0.2,dodge.width = 0.3))+
  stat_summary(geom="point",fun="mean",size=3,position = position_dodge(width=0.3))+
  stat_summary(geom="line",fun="mean",show.legend = FALSE,position = position_dodge(width=0.3))+
  scale_colour_manual(values=couleurs_poster,labels = c("Sham","Stim"))+
  labs(title = "Mean Destroyed Fortress per Session", y ="Destroyed Fortress" ,x="")+
  geom_rect(data=data_long,aes(xmin=1.2,xmax=1.8,ymin=-Inf,ymax=+Inf),fill="grey",alpha=0.01,inherit.aes = FALSE)+
  geom_vline(xintercept = c(1.2,1.8),linetype="dotted",alpha=0.5)+
  theme(legend.position = c(0.9,0.2),plot.title = element_text(hjust=0.5))+
  annotate("text",x=1.5,y=5,label="Training")
p_Fortress

p_Flight = ggplot(filter(data_long,Session=="D01P1"|Session=="D05P2"|Session=="D14P2"),aes(Session,FlightBadEvents,color=Group,group=Group))+theme_pubr()+
  geom_point(alpha=0.4,position=position_jitterdodge(jitter.width = 0.2,dodge.width = 0.3))+
  stat_summary(geom="point",fun="mean",size=3,position = position_dodge(width=0.3))+
  stat_summary(geom="line",fun="mean",show.legend = FALSE,position = position_dodge(width=0.3))+
  scale_colour_manual(values=couleurs_poster,labels = c("Sham","Stim"))+
  labs(title = "Mean Bad Flight Events per Session", y ="Bad Flight Events" ,x="")+
  geom_rect(data=data_long,aes(xmin=1.2,xmax=1.8,ymin=-Inf,ymax=+Inf),fill="grey",alpha=0.01,inherit.aes = FALSE)+
  geom_vline(xintercept = c(1.2,1.8),linetype="dotted",alpha=0.5)+
  theme(legend.position = c(0.9,0.2),plot.title = element_text(hjust=0.5))+
  annotate("text",x=1.5,y=95,label="Training")+
  scale_y_reverse(limits = c(100, 0))
p_Flight

p_Mine = ggplot(filter(data_long,Session=="D01P1"|Session=="D05P2"|Session=="D14P2"),aes(Session,Mine_Prct,color=Group,group=Group))+theme_pubr()+
  geom_point(alpha=0.4,position=position_jitterdodge(jitter.width = 0.2,dodge.width = 0.3))+
  stat_summary(geom="point",fun="mean",size=3,position = position_dodge(width=0.3))+
  stat_summary(geom="line",fun="mean",show.legend = FALSE,position = position_dodge(width=0.3))+
  scale_colour_manual(values=couleurs_poster,labels = c("Sham","Stim"))+
  labs(title = "Mean Mine Percentage Destruction per Session", y ="Mine Percentage Destruction" ,x="")+
  geom_rect(data=data_long,aes(xmin=1.2,xmax=1.8,ymin=-Inf,ymax=+Inf),fill="grey",alpha=0.01,inherit.aes = FALSE)+
  geom_vline(xintercept = c(1.2,1.8),linetype="dotted",alpha=0.5)+
  ylim(0,100)+theme(legend.position = c(0.9,0.2),plot.title = element_text(hjust=0.5))+
  annotate("text",x=1.5,y=5,label="Training")
p_Mine

p_Bonus = ggplot(filter(data_long,Session=="D01P1"|Session=="D05P2"|Session=="D14P2"),aes(Session,Bonus_Prct,color=Group,group=Group))+theme_pubr()+
  geom_point(alpha=0.4,position=position_jitterdodge(jitter.width = 0.2,dodge.width = 0.3))+
  stat_summary(geom="point",fun="mean",size=3,position = position_dodge(width=0.3))+
  stat_summary(geom="line",fun="mean",show.legend = FALSE,position = position_dodge(width=0.3))+
  scale_colour_manual(values=couleurs_poster,labels = c("Sham","Stim"))+
  labs(title = "Mean Bonus Percentage Destruction per Session", y ="Bonus Percentage" ,x="")+
  geom_rect(data=data_long,aes(xmin=1.2,xmax=1.8,ymin=-Inf,ymax=+Inf),fill="grey",alpha=0.01,inherit.aes = FALSE)+
  ylim(0,100)+theme(legend.position = c(0.9,0.2),plot.title = element_text(hjust=0.5))+
  annotate("text",x=1.5,y=5,label="Training")
p_Bonus

ggarrange(p_Fortress,p_Flight,p_Mine,p_Bonus,ncol=2,nrow=2,common.legend = TRUE)


library(patchwork)

figure=(plot_spacer() + p_Flight + plot_spacer() + plot_layout(widths = c(1,2,1)))/( p_Bonus|p_Mine)/(p_Flight|p_Fortress) +
  plot_annotation(tag_levels = "a")&theme(plot.tag.position = c(0.01,0.95),plot.tag = element_text(size=12,face = 'bold'))
figure

#####