#PaperPlots


couleurs=c("#868686FF","#0073C2FF","#A73030FF")
couleurs_alpha=c("#86868666","#0073C266","#A7303099")
library(grid)
data_long$D=1:11
#TotalScore
ggplot(data_long,aes(D,TotalScore,color=Group,group=Group,shape=Group))+theme_pubr()+
  geom_rect(data=data_long,aes(xmin=1.5,xmax=7.5,ymin=-Inf,ymax=+Inf),fill="grey",alpha=0.01,inherit.aes = FALSE)+
  stat_summary(geom="point",fun="mean",size=3)+stat_summary(geom="line",fun="mean",show.legend = FALSE)+
  scale_color_manual(values=couleurs)+
  geom_vline(xintercept = seq(1.5,7.5,2),linetype="dotted",alpha=0.5)+
  geom_vline(xintercept =9.5,alpha=0.3,linetype="solid",size=0.5)+
  scale_x_continuous(sec.axis=sec_axis(~.,breaks=c(1,4.5,8.5,10.5),labels=c("Baseline","Training","Short-term","Long-term")),breaks=1:11)+
  xlab("Sessions")+
  theme(axis.title=element_text(size=18,margin=0.1),text =element_text(size=16),legend.position = c(0.71,0.15),legend.title = element_blank())

#Learning Rate by groups


fit_SHAM=lm(TotalScore~ln(D),data=filter(data_long,Group=="SHAM"))
fit_SD=lm(TotalScore~ln(D),data=filter(data_long,Group=="STIMSD"))
fit_HD=lm(TotalScore~ln(D),data=filter(data_long,Group=="STIMHD"))

eq_SHAM=paste0("y= ",round(coef(fit_SHAM)[1])," + ",round(coef(fit_SHAM)[2]),"*ln(x)     ")
eq_SD=paste0("y= ",round(coef(fit_SD)[1])," + ",round(coef(fit_SD)[2]),"*ln(x)")
eq_HD=paste0("y= ",round(coef(fit_HD)[1])," + ",round(coef(fit_HD)[2]),"*ln(x)")
grob_SHAM=grobTree(textGrob(eq_SHAM,x=0.77,y=0.195,hjust=0,gp=gpar(col="#868686FF")),gp=gpar(fontsize=15))
grob_SD=grobTree(textGrob(eq_SD,x=0.77,y=0.145,hjust=0,gp=gpar(col="#0073C2FF")),gp=gpar(fontsize=15))
grob_HD=grobTree(textGrob(eq_HD,x=0.77,y=0.095,hjust=0,gp=gpar(col="#A73030FF")),gp=gpar(fontsize=15))

plot_LR=ggplot(data_long,aes(D,TotalScore,color=Group,shape=Group))+theme_pubr()+
  scale_x_continuous(sec.axis=sec_axis(~.,breaks=c(1,4.5,8.5,10.5),labels=c("Baseline","Training","Short-term","Long-term")),breaks=1:11)+
  geom_rect(data=data_long,aes(xmin=1.5,xmax=7.5,ymin=-Inf,ymax=+Inf),fill="grey",alpha=0.01,inherit.aes = FALSE)+
  geom_vline(xintercept = seq(1.5,7.5,2),linetype="dotted",alpha=0.5)+geom_vline(xintercept =9.5,alpha=0.3,linetype="solid",size=0.5)+
  stat_smooth(method=lm,formula=y~ln(x),se=FALSE,show.legend = FALSE )+
  stat_summary(geom="point",fun="mean",size=3 ,position=position_dodge(width=0.5))+
  stat_summary(geom="line" ,position=position_dodge(width=0.3),fun="mean" ,show.legend = FALSE,alpha=0.8,linetype="dashed")+
  labs(x="Sessions",y="Total Score")+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=1 ,show.legend = FALSE,geom="errorbar",width=0.1 ,position=position_dodge(width=0.5))+
  scale_color_manual(values=couleurs)+
  scale_y_continuous(breaks =seq(0, 15000, by = 2500))+
  annotation_custom(grob_SHAM)+ annotation_custom(grob_SD)+ annotation_custom(grob_HD)+
  theme(legend.position = c(0.69,0.15),legend.background = element_rect(fill=NA),legend.title = element_blank(),
        axis.title = element_text(size=18,margin=0.1),text=element_text(size=16))
plot_LR

plot_LR=ggplot(data_long,aes(D,TotalScore,color=Group,shape=Group))+theme_pubr()+
  geom_rect(data=data_long,aes(xmin=1.5,xmax=7.5,ymin=-Inf,ymax=+Inf),fill="grey",alpha=0.01,inherit.aes = FALSE)+
  geom_vline(xintercept = seq(1.5,7.5,2),linetype="dotted",alpha=0.5)+geom_vline(xintercept =9.5,alpha=0.3,linetype="solid",size=0.5)+
  stat_smooth(method=lm,formula=y~ln(x),se=FALSE,show.legend = FALSE )+
  stat_summary(geom="point",fun="mean",size=3 )+
  labs(x="Sessions",y="Total Score")+scale_x_continuous(sec.axis=sec_axis(~.,breaks=c(1,4.5,8.5,10.5),labels=c("Baseline","Training","Short-term","Long-term")),breaks=1:11)+
  scale_color_manual(values=couleurs)+
  theme(legend.position = c(0.71,0.15),legend.title = element_blank(),axis.title = element_text(size=18),text=element_text(size=16))+
  scale_y_continuous(breaks =seq(0, 15000, by = 2500))+
  annotation_custom(grob_SHAM)+ annotation_custom(grob_SD)+ annotation_custom(grob_HD)+
  theme(legend.position = c(0.7,0.15),legend.background = element_rect(fill=NA),legend.title = element_blank(),
        axis.title = element_text(size=18,margin=0.1),text=element_text(size=16))
plot_LR

#####
# annotation_df <- data.frame(label = c(eq_SHAM,eq_SD,eq_HD),Group =c("SHAM","STIMSD","STIMHD"),x = c(9, 9, 9),y = c(500, 500, 500))
# 
# plot_LR=ggplot(data_long,aes(D,TotalScore,color=Group,shape=Group))+theme_pubr()+facet_grid(Group~.,margin=FALSE)+
#   geom_rect(data=data_long,aes(xmin=1.5,xmax=7.5,ymin=-Inf,ymax=+Inf),fill="grey",alpha=0.01,inherit.aes = FALSE)+
#   geom_vline(xintercept = seq(1.5,7.5,2),linetype="dotted",alpha=0.5)+geom_vline(xintercept =9.5,alpha=0.3,linetype="solid",size=0.5)+
#   stat_smooth(method=lm,formula=y~ln(x),se=FALSE,show.legend = FALSE)+
#   stat_summary(geom="point",fun="mean",size=2)+
#   stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=1 ,show.legend = FALSE,geom="errorbar",width=0.1)+
#   labs(x="Sessions",y="Total Score")+
#   scale_x_continuous(breaks=1:11)+scale_color_manual(values=couleurs)+
#   theme(legend.position ="bottom",legend.title = element_blank(),axis.title = element_text(size=14),text=element_text(size=12))+
#   scale_y_continuous(breaks =seq(0, 14000, by = 2500))+
#   geom_text(data = annotation_df,mapping = aes(x = x,y = y,label = label),show.legend = FALSE)
# 
# plot_LR


#####
#ANCOVA DELTA
#S11-S9
ancova_S11S9=data_wide%>%
  anova_test(DeltaD14D5~Group+GameLevelLog) #EFFET DU GROUP p = 0.018
ph_S11S9=data_wide%>%
  emmeans_test(DeltaD14D5~Group,covariate = GameLevelLog,p.adjust.method = "holm")
grob_S11S9=grobTree(textGrob(paste0("Group effect: p=",toString(round(ancova_S11S9$p[1],3)),ancova_S11S9$`p<.05`[1],
                                    "\nGaming Experience effect: p=",toString(round(ancova_S11S9$p[2],3)),ancova_S11S9$`p<.05`[2]),
                             x=0.1,y=0.9,hjust=0,vjust=0,gp=gpar(fontsize=16)))
ph_S11S9$p.adj.signif[ph_S11S9$p.adj.signif=="ns"]=""
text_S11S9=paste0("Group effect: p=",toString(round(ancova_S11S9$p[1],3)),ancova_S11S9$`p<.05`[1],
                  "\nGaming Experience effect: p=",toString(round(ancova_S11S9$p[2],3)),ancova_S11S9$`p<.05`[2])

p_S11S9=ggplot(data_wide,aes(Group,DeltaD14D5,color=Group,fill=Group,shape=Group))+
  theme_pubr()+scale_fill_manual(values=couleurs_alpha)+
  scale_color_manual(values=couleurs)+
  geom_half_violin(width=0.3, position = position_nudge(x=-0.2,y=0))+geom_jitter(width=0.1)+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=1 ,show.legend = FALSE,geom="errorbar",width=0.1)+
  stat_summary(fun=mean, geom="point",size=4)+
  ylab("Delta Performance (S11 - S9)")+rremove("legend")+scale_x_discrete(labels=c("SHAM","STIM-SD","STIM-HD"))+
  scale_y_continuous( breaks=seq(-4000,10000,2000),limits=c(-4000,12000))+
  add_pvalue(ph_S11S9,y.position=c(7500,9500,8500),
             label = "p = {round(p.adj,3)} {p.adj.signif}", inherit.aes = FALSE,label.size=5)+
  #annotation_custom(grob_S11S9)+
  annotate("text",x=1,y=11300,label=text_S11S9,size=6,hjust=0)+
  theme(axis.title=element_text(size=18,margin=0.1),text =element_text(size=16) )
p_S11S9  

#S11-S1
ancova_S11S1=data_wide%>%
  anova_test(DeltaD1D14~Group+GameLevelLog) #EFFET DU GROUP p = 0.02
ph_S11S1=data_wide%>%
  emmeans_test(DeltaD1D14~Group,covariate = GameLevelLog,p.adjust.method = "holm")
grob_S11S1=grobTree(textGrob(paste0("Group effect: p=",toString(round(ancova_S11S1$p[1],3)),ancova_S11S1$`p<.05`[1],
                                    "\nGaming Experience effect: p=",toString(round(ancova_S11S1$p[2],3)),ancova_S11S1$`p<.05`[2]),
                             x=0.1,y=0.9,hjust=0,vjust=0,gp=gpar(fontsize=16)))

txt_S11S1=paste0("Group effect: p=",toString(round(ancova_S11S1$p[1],3)),ancova_S11S1$`p<.05`[1],
       "\nGaming Experience effect: p=",toString(round(ancova_S11S1$p[2],3)),ancova_S11S1$`p<.05`[2])
ph_S11S1$p.adj.signif[ph_S11S1$p.adj.signif=="ns"]=""

p_S11S1=ggplot(data_wide,aes(Group,DeltaD1D14,color=Group,fill=Group,shape=Group))+
  theme_pubr()+scale_fill_manual(values=couleurs_alpha)+
  scale_color_manual(values=couleurs)+
  geom_half_violin(width=0.3, position = position_nudge(x=-0.2,y=0))+geom_jitter(width=0.1)+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=1 ,show.legend = FALSE,geom="errorbar",width=0.1)+
  stat_summary(fun=mean, geom="point",size=4)+
  ylab("Delta Performance (S11 - S1)")+rremove("legend")+scale_x_discrete(labels=c("SHAM","STIM-SD","STIM-HD"))+
  scale_y_continuous( breaks=seq(2000,24000,3000),limits=c(2800,24500))+
  add_pvalue(ph_S11S1,y.position=c(20000,22000,21000),
             label = "p = {round(p.adj,3)} {p.adj.signif}", inherit.aes = FALSE,label.size=5)+
  #annotation_custom(grob_S11S1)+
  annotate("text",x=1,y=24300,label=txt_S11S1,size=6,hjust=0)+
  theme(axis.title=element_text(size=18,margin=0.1),text =element_text(size=16) )
p_S11S1 

#S9-S1
ancova_S9S1=data_wide%>%
  anova_test(DeltaD1D5~Group+GameLevelLog) #PAS D'EFFET DU GROUP 
grob_S9S1=grobTree(textGrob(paste0("Group effect: p=",toString(round(ancova_S9S1$p[1],3)),ancova_S9S1$`p<.05`[1],
                                    "\nGaming Experience effect: p=",toString(round(ancova_S9S1$p[2],3)),ancova_S9S1$`p<.05`[2]),
                            x=0.1,y=0.9,hjust=0,vjust=0,gp=gpar(fontsize=16)))

txt_S9S1=paste0("Group effect: p=",toString(round(ancova_S9S1$p[1],3)),ancova_S9S1$`p<.05`[1],
                "\nGaming Experience effect: p=",toString(round(ancova_S9S1$p[2],3)),ancova_S9S1$`p<.05`[2])
#x=0.1,y=0.9,hjust=0,vjust=0,gp=gpar(fontsize=16)))

p_S9S1=ggplot(data_wide,aes(Group,DeltaD1D5,color=Group,fill=Group,shape=Group))+
  theme_pubr()+scale_fill_manual(values=couleurs_alpha)+
  scale_color_manual(values=couleurs)+
  geom_half_violin(width=0.3, position = position_nudge(x=-0.2,y=0))+geom_jitter(width=0.1)+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=1 ,show.legend = FALSE,geom="errorbar",width=0.1)+
  stat_summary(fun=mean, geom="point",size=4)+
  ylab("Delta Performance (S9 - S1)")+rremove("legend")+scale_x_discrete(labels=c("SHAM","STIM-SD","STIM-HD"))+
  scale_y_continuous( breaks=seq(1500,18000,2000),limits=c(1000,18500))+
  #annotation_custom(grob_S9S1)+
  annotate("text",x=1,y=18300,label=txt_S9S1,size=6,hjust=0)+
  theme(axis.title=element_text(size=18,margin=0.1),text =element_text(size=16) )
p_S9S1

layout_matrix <- matrix(c(1, 1, 2, 2, 4, 3, 3, 4), nrow = 2, byrow = TRUE)
grid.arrange(p_S9S1,p_S11S1,p_S11S9, layout_matrix = layout_matrix)
ggarrange(p_S9S1,p_S11S1,p_S11S9,ncol=2,nrow=2,)

ggsave(plot=p_S11S1,"Paper\\FINAL\\DeltaS11S1.pdf",device="pdf",width=10,height=6)
ggsave(plot=p_S9S1,"Paper\\FINAL\\DeltaS9S1.pdf",device="pdf",width=10,height=6)
ggsave(plot=p_S11S9,"Paper\\FINAL\\DeltaS11S9.pdf",device="pdf",width=10,height=6)

#ANCOVA LR
ancova_LR=data_wide%>%
  anova_test(LearningRateLT~Group+GameLevelLog) #EFFET DU GROUP p = 0.091
# ph_LR=data_wide%>%
#   emmeans_test(LearningRateLT~Group,covariate = GameLevelLog,p.adjust.method = "holm")
grob_LR=grobTree(textGrob(paste0("Group effect: p=",toString(round(ancova_LR$p[1],3)),ancova_LR$`p<.05`[1],
                                    "\nGaming Experience effect: p=",toString(round(ancova_LR$p[2],3)),ancova_LR$`p<.05`[2]),
                          x=0.25,y=0.9,hjust=0,vjust=0),gp=gpar(fontsize=16))
# ph_LR$p.adj.signif[ph_LR$p.adj.signif=="ns"]=""

p_ancovaLR=ggplot(data_wide,aes(Group,LearningRateLT,color=Group,fill=Group,shape=Group))+
  theme_pubr()+scale_fill_manual(values=couleurs_alpha)+
  scale_color_manual(values=couleurs)+
  geom_half_violin(width=0.3, position = position_nudge(x=-0.2,y=0))+geom_jitter(width=0.1)+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=1 ,show.legend = FALSE,geom="errorbar",width=0.1)+
  stat_summary(fun=mean, geom="point",size=4)+
  ylab("Learning Rate")+rremove("legend")+scale_x_discrete(labels=c("SHAM","STIM-SD","STIM-HD"))+
  scale_y_continuous( breaks=seq(-4000,8000,1000))+
  # add_pvalue(ph_LR,y.position=c(7500,8700,8100),
             # label = "p = {round(p.adj,3)} {p.adj.signif}", inherit.aes = FALSE)+
  annotation_custom(grob_LR)+
  theme(axis.title=element_text(margin=0.1,size=18),text =element_text(size=16) )
p_ancovaLR  

ggsave(plot=p_ancovaLR,"Paper\\FINAL\\LearningRateAncova.pdf",device="pdf",width=10,height=6)


#Sous Score by session

plot_Flight=ggplot(data_long,aes(D,Flight,color=Group,group=Group,shape=Group))+theme_pubr()+
  geom_rect(data=data_long,aes(xmin=1.5,xmax=7.5,ymin=-Inf,ymax=+Inf),fill="grey",alpha=0.01,inherit.aes = FALSE)+
  stat_summary(geom="point",fun="mean",size=3 ,position=position_dodge(width=0.3))+
  stat_summary(geom="line" ,position=position_dodge(width=0.3),fun="mean" ,show.legend = FALSE)+
  scale_color_manual(values=couleurs)+geom_vline(xintercept = seq(1.5,7.5,2),linetype="dotted",alpha=0.5)+
  geom_vline(xintercept =9.5,alpha=0.3,linetype="solid",size=0.5)+xlab("Sessions")+
  scale_x_continuous(sec.axis=sec_axis(~.,breaks=c(1,4.5,8.5,10.5),labels=c("Baseline","Training","Short-term","Long-term")),breaks=1:11)+
  stat_summary(fun.data = "mean_se",geom="errorbar",width=0.2, fun.args = list(mult = 1) ,position=position_dodge(width=0.3),show.legend = FALSE)+
  theme(axis.title=element_text(size=18,margin=0.1),text =element_text(size=16),legend.position = c(0.71,0.15),legend.title = element_blank())

plot_Bonus=ggplot(data_long,aes(D,Bonus,color=Group,group=Group,shape=Group))+theme_pubr()+
  geom_rect(data=data_long,aes(xmin=1.5,xmax=7.5,ymin=-Inf,ymax=+Inf),fill="grey",alpha=0.01,inherit.aes = FALSE)+
  stat_summary(geom="point",fun="mean",size=3 ,position=position_dodge(width=0.3))+
  stat_summary(geom="line" ,position=position_dodge(width=0.3),fun="mean" ,show.legend = FALSE)+
  scale_color_manual(values=couleurs)+geom_vline(xintercept = seq(1.5,7.5,2),linetype="dotted",alpha=0.5)+
  geom_vline(xintercept =9.5,alpha=0.3,linetype="solid",size=0.5)+xlab("Sessions")+
  scale_x_continuous(sec.axis=sec_axis(~.,breaks=c(1,4.5,8.5,10.5),labels=c("Baseline","Training","Short-term","Long-term")),breaks=1:11)+
  stat_summary(fun.data = "mean_se",geom="errorbar",width=0.2, fun.args = list(mult = 1) ,position=position_dodge(width=0.3),show.legend = FALSE)+
  theme(axis.title=element_text(size=18,margin=0.1),text =element_text(size=16),legend.position = c(0.71,0.15),legend.title = element_blank())

plot_Mine=ggplot(data_long,aes(D,Mine,color=Group,group=Group,shape=Group))+theme_pubr()+
  geom_rect(data=data_long,aes(xmin=1.5,xmax=7.5,ymin=-Inf,ymax=+Inf),fill="grey",alpha=0.01,inherit.aes = FALSE)+
  stat_summary(geom="point",fun="mean",size=3 ,position=position_dodge(width=0.3))+
  stat_summary(geom="line" ,position=position_dodge(width=0.3),fun="mean" ,show.legend = FALSE)+
  scale_color_manual(values=couleurs)+geom_vline(xintercept = seq(1.5,7.5,2),linetype="dotted",alpha=0.5)+
  geom_vline(xintercept =9.5,alpha=0.3,linetype="solid",size=0.5)+xlab("Sessions")+
  scale_x_continuous(sec.axis=sec_axis(~.,breaks=c(1,4.5,8.5,10.5),labels=c("Baseline","Training","Short-term","Long-term")),breaks=1:11)+
  stat_summary(fun.data = "mean_se",geom="errorbar",width=0.2, fun.args = list(mult = 1) ,position=position_dodge(width=0.3),show.legend = FALSE)+
  theme(axis.title=element_text(size=18,margin=0.1),text =element_text(size=16),legend.position = c(0.71,0.15),legend.title = element_blank())

plot_Fortress=ggplot(data_long,aes(D,Fortress,color=Group,group=Group,shape=Group))+theme_pubr()+
  geom_rect(data=data_long,aes(xmin=1.5,xmax=7.5,ymin=-Inf,ymax=+Inf),fill="grey",alpha=0.01,inherit.aes = FALSE)+
  stat_summary(geom="point",fun="mean",size=3 ,position=position_dodge(width=0.3))+
  stat_summary(geom="line" ,position=position_dodge(width=0.3),fun="mean" ,show.legend = FALSE)+
  scale_color_manual(values=couleurs)+geom_vline(xintercept = seq(1.5,7.5,2),linetype="dotted",alpha=0.5)+
  geom_vline(xintercept =9.5,alpha=0.3,linetype="solid",size=0.5)+xlab("Sessions")+
  scale_x_continuous(sec.axis=sec_axis(~.,breaks=c(1,4.5,8.5,10.5),labels=c("Baseline","Training","Short-term","Long-term")),breaks=1:11)+
  stat_summary(fun.data = "mean_se",geom="errorbar",width=0.2, fun.args = list(mult = 1) ,position=position_dodge(width=0.3),show.legend = FALSE)+
  theme(axis.title=element_text(size=18,margin=0.1),text =element_text(size=16),legend.position = c(0.71,0.15),legend.title = element_blank())

ggarrange(plot_Flight,plot_Bonus,plot_Mine,plot_Fortress,ncol=2,nrow=2,common.legend = TRUE)
ggsave(plot=plot_Flight,"Paper\\FINAL\\FlightScore.pdf",device="pdf",width=10,height=6)
ggsave(plot=plot_Bonus,"Paper\\FINAL\\BonusScore.pdf",device="pdf",width=10,height=6)
ggsave(plot=plot_Mine,"Paper\\FINAL\\MineScore.pdf",device="pdf",width=10,height=6)
ggsave(plot=plot_Fortress,"Paper\\FINAL\\FortressScore.pdf",device="pdf",width=10,height=6)

plot_TotalScore=ggplot(data_long,aes(D,TotalScore,color=Group,group=Group,shape=Group))+theme_pubr()+
  geom_rect(data=data_long,aes(xmin=1.5,xmax=7.5,ymin=-Inf,ymax=+Inf),fill="grey",alpha=0.01,inherit.aes = FALSE)+
  stat_summary(geom="point",fun="mean",size=3 ,position=position_dodge(width=0.3))+
  stat_summary(geom="line" ,position=position_dodge(width=0.3),fun="mean" ,show.legend = FALSE)+
  scale_color_manual(values=couleurs)+geom_vline(xintercept = seq(1.5,7.5,2),linetype="dotted",alpha=0.5)+
  geom_vline(xintercept =9.5,alpha=0.3,linetype="solid",size=0.5)+xlab("Sessions")+
  scale_x_continuous(sec.axis=sec_axis(~.,breaks=c(1,4.5,8.5,10.5),labels=c("Baseline","Training","Short-term","Long-term")),breaks=1:11)+
  stat_summary(fun.data = "mean_se",geom="errorbar",width=0.2, fun.args = list(mult = 1) ,position=position_dodge(width=0.3),show.legend = FALSE)+
  theme(axis.title=element_text(size=18,margin=0.1),text =element_text(size=16),legend.position = c(0.71,0.15),legend.title = element_blank())
plot_TotalScore
