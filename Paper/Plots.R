#PaperPlots
library(ggplot2)
library(ggpubr)
library(dplyr)
library(rstatix)
library(ggsci)
library(MANOVA.RM)
library(viridis)
library(hrbrthemes)
library(emmeans)
library(gghalves)
library(Hmisc)
library(ggprism)
library(grid)

couleurs=c("#868686FF","#0073C2FF","#A73030FF")
couleurs_alpha=c("#86868666","#0073C266","#A7303099")
data_wide$Group=factor(data_wide$Group,levels=c("SHAM","STIMSD","STIMHD"))
data_long$Group=factor(data_long$Group,levels=c("SHAM","STIMSD","STIMHD"))
data_long$D=1:11
#TotalScore
p_total=ggplot(data_long,aes(D,TotalScore,color=Group,group=Group,shape=Group))+theme_pubr()+
  geom_rect(data=data_long,aes(xmin=1.5,xmax=7.5,ymin=-Inf,ymax=+Inf),fill="grey",alpha=0.01,inherit.aes = FALSE)+
  stat_summary(geom="point",fun="mean",size=3)+stat_summary(geom="line",fun="mean",show.legend = FALSE)+
  scale_colour_manual(values=couleurs,labels=c("Sham","SD-tRNS","HD-tRNS"))+
  scale_shape(labels=c("Sham","SD-tRNS","HD-tRNS"))+
  geom_vline(xintercept = seq(1.5,7.5,2),linetype="dotted",alpha=0.5)+
  geom_vline(xintercept =9.5,alpha=0.3,linetype="solid",size=0.5)+
  scale_x_continuous(sec.axis=sec_axis(~.,breaks=c(1,4.5,8.5,10.5),labels=c("Baseline","Training","Short-term","Long-term")),breaks=1:11)+
  xlab("Game Session")+
  theme(axis.title=element_text(size=18,margin=0.1),text =element_text(size=16),legend.position = c(0.71,0.15),legend.title = element_blank())+
  annotate("text",x=c(1,2.5,4.5,6.5,8.5,10.5),y=Inf,vjust=1.5,label=c("D1","D2","D3","D4","D5","D15"),size=5)
p_total
ggsave(plot=p_total,"Paper\\FINAL\\TotalScoreperSession.pdf",device="pdf",width=10,height=6)

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
#grob_all=grobTree(textGrob(expression("y = "~alpha~"+"~beta~"*ln(x)"),x=0.1,y=0.85,hjust=0,gp=gpar(col="black")),gp=gpar(fontsize=15))

plot_LR=ggplot(data_long,aes(D,TotalScore,color=Group,shape=Group))+theme_pubr()+
  scale_x_continuous(sec.axis=sec_axis(~.,breaks=c(1,4.5,8.5,10.5),labels=c("Baseline","Training","Short-term","Long-term")),breaks=1:11)+
  geom_rect(data=data_long,aes(xmin=1.5,xmax=7.5,ymin=-Inf,ymax=+Inf),fill="grey",alpha=0.01,inherit.aes = FALSE)+
  geom_vline(xintercept = seq(1.5,7.5,2),linetype="dotted",alpha=0.5)+geom_vline(xintercept =9.5,alpha=0.3,linetype="solid",size=0.5)+
  stat_smooth(method=lm,formula=y~ln(x),se=FALSE,show.legend = FALSE )+
  stat_summary(geom="point",fun="mean",size=3 ,position=position_dodge(width=0.5))+
  #stat_summary(geom="line" ,position=position_dodge(width=0.3),fun="mean" ,show.legend = FALSE,alpha=0.8,linetype="dashed")+
  labs(x="Game Session",y="Total Score")+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=1 ,show.legend = FALSE,geom="errorbar",width=0.1 ,position=position_dodge(width=0.5))+
  scale_colour_manual(values=couleurs,labels=c("Sham","SD-tRNS","HD-tRNS"))+
  scale_shape(labels=c("Sham","SD-tRNS","HD-tRNS"))+
  scale_y_continuous(breaks =seq(0, 15000, by = 2500))+
  #annotation_custom(grob_all)+
  annotation_custom(grob_SHAM)+ annotation_custom(grob_SD)+ annotation_custom(grob_HD)+
  theme(legend.position = c(0.69,0.15),legend.background = element_rect(fill=NA),legend.title = element_blank(),
        axis.title = element_text(size=18,margin=0.1),legend.text = element_text(size=16),text=element_text(size=16))+
  annotate("text",x=c(1,2.5,4.5,6.5,8.5,10.5),y=Inf,vjust=1.5,label=c("D1","D2","D3","D4","D5","D15"),size=5)
plot_LR

ggsave(plot=plot_LR,"Paper\\FINAL\\RegressionByGroupV2.pdf",device="pdf",width=10,height=6)


plot_LR=ggplot(data_long,aes(D,TotalScore,color=Group,shape=Group))+theme_pubr()+
  geom_rect(data=data_long,aes(xmin=1.5,xmax=7.5,ymin=-Inf,ymax=+Inf),fill="grey",alpha=0.01,inherit.aes = FALSE)+
  geom_vline(xintercept = seq(1.5,7.5,2),linetype="dotted",alpha=0.5)+geom_vline(xintercept =9.5,alpha=0.3,linetype="solid",size=0.5)+
  stat_smooth(method=lm,formula=y~ln(x),se=FALSE,show.legend = FALSE )+
  stat_summary(geom="point",fun="mean",size=3 )+
  labs(x="Session",y="Total Score")+scale_x_continuous(sec.axis=sec_axis(~.,breaks=c(1,4.5,8.5,10.5),labels=c("Baseline","Training","Short-term","Long-term")),breaks=1:11)+
  scale_colour_manual(values=couleurs,labels=c("Sham","SD-tRNS","HD-tRNS"))+
  scale_shape(labels=c("Sham","SD-tRNS","HD-tRNS"))+
  theme(legend.position = c(0.71,0.15),legend.title = element_blank(),axis.title = element_text(size=18),text=element_text(size=16))+
  scale_y_continuous(breaks =seq(0, 15000, by = 2500))+
  annotation_custom(grob_SHAM)+ annotation_custom(grob_SD)+ annotation_custom(grob_HD)+
  theme(legend.position = c(0.7,0.15),legend.background = element_rect(fill=NA),legend.title = element_blank(),
        axis.title = element_text(size=18,margin=0.1),text=element_text(size=16))


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
# grob_S11S9=grobTree(textGrob(paste0("Group effect: p=",toString(round(ancova_S11S9$p[1],3)),ancova_S11S9$`p<.05`[1],
#                                     "\nGaming Experience effect: p=",toString(round(ancova_S11S9$p[2],3)),ancova_S11S9$`p<.05`[2]),
#                              x=0.1,y=0.9,hjust=0,vjust=0,gp=gpar(fontsize=16)))

grob_S11S9=grobTree(text_grob(paste0("Group effect: p = ",toString(round(ancova_S11S9$p[1],3))),
                              x=0.1,y=0.95,hjust=0,vjust=0,face="bold"),
                    text_grob(paste0("\nGaming Experience effect: p = ",toString(round(ancova_S11S9$p[2],3))),
                              x=0.1,y=0.9,hjust=0,vjust=0),
                             gp=gpar(fontsize=16))

ph_S11S9$p.adj.signif[ph_S11S9$p.adj.signif=="ns"]=""
text_S11S9=paste0("Group effect: p = ",toString(round(ancova_S11S9$p[1],3)),ancova_S11S9$`p<.05`[1],
                  "\nGaming Experience effect: p = ",toString(round(ancova_S11S9$p[2],3)),ancova_S11S9$`p<.05`[2])

p_S11S9=ggplot(data_wide,aes(Group,DeltaD14D5,color=Group,fill=Group,shape=Group))+
  theme_pubr()+scale_fill_manual(values=couleurs_alpha)+
  scale_color_manual(values=couleurs)+
  geom_half_violin(width=0.3, position = position_nudge(x=-0.2,y=0))+geom_jitter(width=0.1)+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=1 ,show.legend = FALSE,geom="errorbar",width=0.1)+
  stat_summary(fun=mean, geom="point",size=4)+
  #ylab("Delta Performance (GS11 - GS9)")+  
  ylab("Delta Performance (Retention)")+
  rremove("legend")+scale_x_discrete(labels=c("Sham","SD-tRNS","HD-tRNS"))+
  scale_y_continuous( breaks=seq(-4000,10000,2000),limits=c(-4000,12000))+
  add_pvalue(ph_S11S9[1,],y.position=7500,
             label = "p = {round(p.adj,3)} ", inherit.aes = FALSE,label.size=5)+
  add_pvalue(ph_S11S9[2,],y.position=9500,
             label = "p = {round(p.adj,3)}", inherit.aes = FALSE,label.size=5,fontface="bold")+
  add_pvalue(ph_S11S9[3,],y.position=8500,
             label = "p = {round(p.adj,3)}", inherit.aes = FALSE,label.size=5,fontface="bold")+
  annotation_custom(grob_S11S9)+
  #annotate("text",x=1,y=11300,label=text_S11S9,size=6,hjust=0)+
  theme(axis.title=element_text(size=18,margin=0.1),text =element_text(size=16) )
p_S11S9  

#S11-S1
ancova_S11S1=data_wide%>%
  anova_test(DeltaD1D14~Group+GameLevelLog) #EFFET DU GROUP p = 0.02
ph_S11S1=data_wide%>%
  emmeans_test(DeltaD1D14~Group,covariate = GameLevelLog,p.adjust.method = "holm")
# grob_S11S1=grobTree(textGrob(paste0("Group effect: p=",toString(round(ancova_S11S1$p[1],3)),ancova_S11S1$`p<.05`[1],
#                                     "\nGaming Experience effect: p=",toString(round(ancova_S11S1$p[2],3)),
#                                     ancova_S11S1$`p<.05`[2]),
#                              x=0.1,y=0.9,hjust=0,vjust=0,gp=gpar(fontsize=16)))

grob_S11S1=grobTree(text_grob(paste0("Group effect: p = ",toString(round(ancova_S11S1$p[1],3))),
                              x=0.1,y=0.95,hjust=0,vjust=0,face="bold"),
                   text_grob(paste0("Gaming Experience effect: p = ",toString(round(ancova_S11S1$p[2],3))),
                   x=0.1,y=0.9,hjust=0,vjust=0),gp=gpar(fontsize=16))

txt_S11S1=paste0("Group effect: p=",toString(round(ancova_S11S1$p[1],3)),ancova_S11S1$`p<.05`[1],
       "\nGaming Experience effect: p=",toString(round(ancova_S11S1$p[2],3)),ancova_S11S1$`p<.05`[2])

ph_S11S1=ph_S11S1%>%
  mutate(face=case_when(
    p.adj.signif=="*" ~ "bold",
    p.adj.signif=="ns" ~ "plain"
  ))

ph_S11S1$p.adj.signif[ph_S11S1$p.adj.signif=="ns"]=""

p_S11S1=ggplot(data_wide,aes(Group,DeltaD1D14,color=Group,fill=Group,shape=Group))+
  theme_pubr()+scale_fill_manual(values=couleurs_alpha)+
  scale_color_manual(values=couleurs)+
  geom_half_violin(width=0.3, position = position_nudge(x=-0.2,y=0))+geom_jitter(width=0.1)+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=1 ,show.legend = FALSE,geom="errorbar",width=0.1)+
  stat_summary(fun=mean, geom="point",size=4)+
  #ylab("Delta Performance (GS11 - GS1)")+
  ylab("Delta Performance (Long Term)")+
  rremove("legend")+scale_x_discrete(labels=c("Sham","SD-tRNS","HD-tRNS"))+
  scale_y_continuous( breaks=seq(2000,24000,3000),limits=c(2800,24500))+
  add_pvalue(ph_S11S1[1,],y.position=19000,
             label = "p = {round(p.adj,3)} ", inherit.aes = FALSE,label.size=5)+
  add_pvalue(ph_S11S1[2,],y.position=22000,
             label = "p = {round(p.adj,3)}", inherit.aes = FALSE,label.size=5)+
  add_pvalue(ph_S11S1[3,],y.position=20500,
             label = "p = {round(p.adj,3)}", inherit.aes = FALSE,label.size=5,fontface="bold")+
  annotation_custom(grob_S11S1)+
  theme(axis.title=element_text(size=18,margin=0.1),text =element_text(size=16) )
p_S11S1 

#S9-S1
ancova_S9S1=data_wide%>%
  anova_test(DeltaD1D5~Group+GameLevelLog) #PAS D'EFFET DU GROUP 
# grob_S9S1=grobTree(textGrob(paste0("Group effect: p=",toString(round(ancova_S9S1$p[1],3)),ancova_S9S1$`p<.05`[1],
#                                     "\nGaming Experience effect: p=",toString(round(ancova_S9S1$p[2],3)),ancova_S9S1$`p<.05`[2]),
#                             x=0.1,y=0.9,hjust=0,vjust=0,gp=gpar(fontsize=16)))
grob_S9S1=grobTree(text_grob(paste0("Group effect: p = ",toString(round(ancova_S9S1$p[1],3)),ancova_S9S1$`p<.05`[1]),
                             x=0.1,y=0.95,hjust=0,vjust=0),
                   text_grob(paste0("Gaming Experience effect: p = ",toString(round(ancova_S9S1$p[2],3)))
                             ,x=0.1,y=0.9,hjust=0,vjust=0,face="bold")
                  ,gp=gpar(fontsize=16))

txt_S9S1=paste0("Group effect: p=",toString(round(ancova_S9S1$p[1],3)),ancova_S9S1$`p<.05`[1],
                "\nGaming Experience effect: p=",toString(round(ancova_S9S1$p[2],3)),ancova_S9S1$`p<.05`[2])
#x=0.1,y=0.9,hjust=0,vjust=0,gp=gpar(fontsize=16)))

p_S9S1=ggplot(data_wide,aes(Group,DeltaD1D5,color=Group,fill=Group,shape=Group))+
  theme_pubr()+scale_fill_manual(values=couleurs_alpha)+
  scale_color_manual(values=couleurs)+
  geom_half_violin(width=0.8, position = position_nudge(x=-0.15,y=0))+geom_jitter(width=0.1)+
  #geom_half_violin(width=0.3, position = position_nudge(x=-0.2,y=0))+geom_jitter(width=0.1)+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=1 ,show.legend = FALSE,geom="errorbar",width=0.1)+
  stat_summary(fun=mean, geom="point",size=4)+
  #ylab("Delta Performance (GS9 - GS1)")+
  ylab("Delta Performance (Short Term)")+
  rremove("legend")+scale_x_discrete(labels=c("Sham","SD-tRNS","HD-tRNS"))+
  scale_y_continuous( breaks=seq(1500,18000,2000),limits=c(1000,18500))+
  annotation_custom(grob_S9S1)+
  #annotate("text",x=1,y=18300,label=txt_S9S1,size=6,hjust=0)+
  theme(axis.title=element_text(size=18,margin=0.1),text =element_text(size=16))
p_S9S1

library(patchwork)

f_delta=( p_S9S1|p_S11S1) /(plot_spacer() + p_S11S9 + plot_spacer() + plot_layout(widths = c(1,2,1)))+
  plot_annotation(tag_levels = "a")&theme(plot.tag.position = c(0.01,0.95),plot.tag = element_text(size=22,face = 'bold'))
f_delta

ggsave(plot=f_delta,"Paper\\FINAL\\FigureDelta.pdf",device="pdf",width=14,height=12)


ggsave(plot=p_S11S1,"Paper\\FINAL\\DeltaS11S1.pdf",device="pdf",width=10,height=6)
ggsave(plot=p_S9S1,"Paper\\FINAL\\DeltaS9S1.pdf",device="pdf",width=10,height=6)
ggsave(plot=p_S11S9,"Paper\\FINAL\\DeltaS11S9.pdf",device="pdf",width=10,height=6)


#ANCOVA LR
ancova_LR=data_wide%>%
  anova_test(LearningRateLT~Group+GameLevelLog) #EFFET DU GROUP p = 0.091
# ph_LR=data_wide%>%
#   emmeans_test(LearningRateLT~Group,covariate = GameLevelLog,p.adjust.method = "holm")
# grob_LR=grobTree(textGrob(paste0("Group effect: p=",toString(round(ancova_LR$p[1],3)),ancova_LR$`p<.05`[1],
#                           "\nGaming Experience effect: p=",toString(round(ancova_LR$p[2],3)),ancova_LR$`p<.05`[2]),
#                            x=0.25,y=0.9,hjust=0,vjust=0),gp=gpar(fontsize=16))

grob_LR=grobTree(textGrob(paste0("Group effect: p = ",toString(round(ancova_LR$p[1],3)),ancova_LR$`p<.05`[1]),
                          x=0.25,y=0.9,hjust=0,vjust=0),
                          text_grob(paste0("Gaming Experience effect: p = ",toString(round(ancova_LR$p[2],3))),
                           x=0.25,y=0.85,hjust=0,vjust=0,face="bold")
                          ,gp=gpar(fontsize=16))

as_ggplot(grob_LR)
# ph_LR$p.adj.signif[ph_LR$p.adj.signif=="ns"]=""

p_ancovaLR=ggplot(data_wide,aes(Group,LearningRateLT,color=Group,fill=Group,shape=Group))+
  theme_pubr()+scale_fill_manual(values=couleurs_alpha)+
  scale_color_manual(values=couleurs)+
  geom_half_violin(width=0.3, position = position_nudge(x=-0.2,y=0))+geom_jitter(width=0.1)+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=1 ,show.legend = FALSE,geom="errorbar",width=0.1)+
  stat_summary(fun=mean, geom="point",size=4)+
  ylab("Learning Rate")+rremove("legend")+scale_x_discrete(labels=c("Sham","SD-tRNS","HD-tRNS"))+
  scale_y_continuous( breaks=seq(-4000,8000,1000))+
  # add_pvalue(ph_LR,y.position=c(7500,8700,8100),
             # label = "p = {round(p.adj,3)} {p.adj.signif}", inherit.aes = FALSE)+
  annotation_custom(grob_LR)+
  theme(axis.title=element_text(margin=0.1,size=18),text =element_text(size=16) )
p_ancovaLR  

ggsave(plot=p_ancovaLR,"Paper\\FINAL\\LearningRateAncova_small.pdf",device="pdf",width=7,height=6)


#Sous Score by session
plot_Flight=ggplot(data_long,aes(D,Flight,color=Group,group=Group,shape=Group))+theme_pubr()+
  geom_rect(data=data_long,aes(xmin=1.5,xmax=7.5,ymin=-Inf,ymax=+Inf),fill="grey",alpha=0.01,inherit.aes = FALSE)+
  stat_summary(geom="point",fun="mean",size=3 ,position=position_dodge(width=0.3))+
  stat_summary(geom="line" ,position=position_dodge(width=0.3),fun="mean" ,show.legend = FALSE)+
  scale_colour_manual(values=couleurs,labels=c("Sham","SD-tRNS","HD-tRNS"))+
  scale_shape(labels=c("Sham","SD-tRNS","HD-tRNS"))+
  geom_vline(xintercept = seq(1.5,7.5,2),linetype="dotted",alpha=0.5)+
  geom_vline(xintercept =9.5,alpha=0.3,linetype="solid",size=0.5)+xlab("Game Session")+
  scale_x_continuous(sec.axis=sec_axis(~.,breaks=c(1,4.5,8.5,10.5),labels=c("Baseline","Training","Short-term","Long-term")),breaks=1:11)+
  stat_summary(fun.data = "mean_se",geom="errorbar",width=0.2, fun.args = list(mult = 1) ,position=position_dodge(width=0.3),show.legend = FALSE)+
  theme(axis.title=element_text(size=18,margin=0.1),text =element_text(size=16),legend.position = c(0.71,0.15),legend.title = element_blank())+
  annotate("text",x=c(1,2.5,4.5,6.5,8.5,10.5),y=Inf,vjust=1.5,label=c("D1","D2","D3","D4","D5","D15"),size=5)

plot_Bonus=ggplot(data_long,aes(D,Bonus,color=Group,group=Group,shape=Group))+theme_pubr()+
  geom_rect(data=data_long,aes(xmin=1.5,xmax=7.5,ymin=-Inf,ymax=+Inf),fill="grey",alpha=0.01,inherit.aes = FALSE)+
  stat_summary(geom="point",fun="mean",size=3 ,position=position_dodge(width=0.3))+
  stat_summary(geom="line" ,position=position_dodge(width=0.3),fun="mean" ,show.legend = FALSE)+
  scale_colour_manual(values=couleurs,labels=c("Sham","SD-tRNS","HD-tRNS"))+
  scale_shape(labels=c("Sham","SD-tRNS","HD-tRNS"))
  +geom_vline(xintercept = seq(1.5,7.5,2),linetype="dotted",alpha=0.5)+
  geom_vline(xintercept =9.5,alpha=0.3,linetype="solid",size=0.5)+xlab("Game Session")+
  scale_x_continuous(sec.axis=sec_axis(~.,breaks=c(1,4.5,8.5,10.5),labels=c("Baseline","Training","Short-term","Long-term")),breaks=1:11)+
  stat_summary(fun.data = "mean_se",geom="errorbar",width=0.2, fun.args = list(mult = 1) ,position=position_dodge(width=0.3),show.legend = FALSE)+
  theme(axis.title=element_text(size=18,margin=0.1),text =element_text(size=16),legend.position = c(0.71,0.15),legend.title = element_blank())+
  annotate("text",x=c(1,2.5,4.5,6.5,8.5,10.5),y=Inf,vjust=1.5,label=c("D1","D2","D3","D4","D5","D15"),size=5)

plot_Mine=ggplot(data_long,aes(D,Mine,color=Group,group=Group,shape=Group))+theme_pubr()+
  geom_rect(data=data_long,aes(xmin=1.5,xmax=7.5,ymin=-Inf,ymax=+Inf),fill="grey",alpha=0.01,inherit.aes = FALSE)+
  stat_summary(geom="point",fun="mean",size=3 ,position=position_dodge(width=0.3))+
  stat_summary(geom="line" ,position=position_dodge(width=0.3),fun="mean" ,show.legend = FALSE)+
  scale_colour_manual(values=couleurs,labels=c("Sham","SD-tRNS","HD-tRNS"))+
  scale_shape(labels=c("Sham","SD-tRNS","HD-tRNS"))+
  geom_vline(xintercept = seq(1.5,7.5,2),linetype="dotted",alpha=0.5)+
  geom_vline(xintercept =9.5,alpha=0.3,linetype="solid",size=0.5)+xlab("Game Session")+
  scale_x_continuous(sec.axis=sec_axis(~.,breaks=c(1,4.5,8.5,10.5),labels=c("Baseline","Training","Short-term","Long-term")),breaks=1:11)+
  stat_summary(fun.data = "mean_se",geom="errorbar",width=0.2, fun.args = list(mult = 1) ,position=position_dodge(width=0.3),show.legend = FALSE)+
  theme(axis.title=element_text(size=18,margin=0.1),text =element_text(size=16),legend.position = c(0.71,0.15),legend.title = element_blank())+
  annotate("text",x=c(1,2.5,4.5,6.5,8.5,10.5),y=Inf,vjust=1.5,label=c("D1","D2","D3","D4","D5","D15"),size=5)
plot_Mine

plot_Fortress=ggplot(data_long,aes(D,Fortress,color=Group,group=Group,shape=Group))+theme_pubr()+
  geom_rect(data=data_long,aes(xmin=1.5,xmax=7.5,ymin=-Inf,ymax=+Inf),fill="grey",alpha=0.01,inherit.aes = FALSE)+
  stat_summary(geom="point",fun="mean",size=3 ,position=position_dodge(width=0.3))+
  stat_summary(geom="line" ,position=position_dodge(width=0.3),fun="mean" ,show.legend = FALSE)+
  scale_colour_manual(values=couleurs,labels=c("Sham","SD-tRNS","HD-tRNS"))+
  scale_shape(labels=c("Sham","SD-tRNS","HD-tRNS"))+
  geom_vline(xintercept = seq(1.5,7.5,2),linetype="dotted",alpha=0.5)+
  geom_vline(xintercept =9.5,alpha=0.3,linetype="solid",size=0.5)+xlab("Game Session")+
  scale_x_continuous(sec.axis=sec_axis(~.,breaks=c(1,4.5,8.5,10.5),labels=c("Baseline","Training","Short-term","Long-term")),breaks=1:11)+
  stat_summary(fun.data = "mean_se",geom="errorbar",width=0.2, fun.args = list(mult = 1) ,position=position_dodge(width=0.3),show.legend = FALSE)+
  theme(axis.title=element_text(size=18,margin=0.1),text =element_text(size=16),legend.position = c(0.71,0.15),legend.title = element_blank())+
  annotate("text",x=c(1,2.5,4.5,6.5,8.5,10.5),y=Inf,vjust=1.5,label=c("D1","D2","D3","D4","D5","D15"),size=5)
plot_Fortress

ggarrange(plot_Flight,plot_Bonus,plot_Mine,plot_Fortress,ncol=2,nrow=2,common.legend = TRUE)
ggsave(plot=plot_Flight,"Paper\\FINAL\\FlightScore.pdf",device="pdf",width=10,height=6)
ggsave(plot=plot_Bonus,"Paper\\FINAL\\BonusScore.pdf",device="pdf",width=10,height=6)
ggsave(plot=plot_Mine,"Paper\\FINAL\\MineScore.pdf",device="pdf",width=10,height=6)
ggsave(plot=plot_Fortress,"Paper\\FINAL\\FortressScore.pdf",device="pdf",width=10,height=6)

plot_TotalScore=ggplot(data_long,aes(D,TotalScore,color=Group,group=Group,shape=Group))+theme_pubr()+
  geom_rect(data=data_long,aes(xmin=1.5,xmax=7.5,ymin=-Inf,ymax=+Inf),fill="grey",alpha=0.01,inherit.aes = FALSE)+
  stat_summary(geom="point",fun="mean",size=3 ,position=position_dodge(width=0.3))+
  stat_summary(geom="line" ,position=position_dodge(width=0.3),fun="mean" ,show.legend = FALSE)+
  scale_colour_manual(values=couleurs,labels=c("Sham","SD-tRNS","HD-tRNS"))+
  scale_shape(labels=c("Sham","SD-tRNS","HD-tRNS"))+
  geom_vline(xintercept = seq(1.5,7.5,2),linetype="dotted",alpha=0.5)+
  geom_vline(xintercept =9.5,alpha=0.3,linetype="solid",size=0.5)+xlab("Game Session")+
  scale_x_continuous(sec.axis=sec_axis(~.,breaks=c(1,4.5,8.5,10.5),labels=c("Baseline","Training","Short-term","Long-term")),breaks=1:11)+
  stat_summary(fun.data = "mean_se",geom="errorbar",width=0.2, fun.args = list(mult = 1) ,position=position_dodge(width=0.3),show.legend = FALSE)+
  theme(axis.title=element_text(size=18,margin=0.1),text =element_text(size=16),legend.position = c(0.71,0.15),legend.title = element_blank())+
  annotate("text",x=c(1,2.5,4.5,6.5,8.5,10.5),y=Inf,vjust=1.5,label=c("D1","D2","D3","D4","D5","D15"),size=5)

plot_TotalScore
ggsave(plot=plot_TotalScore,"Paper\\FINAL\\TotalScoreSupp.pdf",device="pdf",width=10,height=6)


#Game Level
p_GameLevel=ggplot(data_wide,aes(Group,GameLevel,color=Group,fill=Group,shape=Group))+
  theme_pubr()+scale_fill_manual(values=couleurs_alpha)+
  scale_color_manual(values=couleurs)+
  geom_half_violin(width=0.3, position = position_nudge(x=-0.2,y=0))+geom_jitter(width=0.1)+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=1 ,show.legend = FALSE,geom="errorbar",width=0.1)+
  stat_summary(fun=mean, geom="point",size=4)+
  ylab("Game Level")+rremove("legend")+scale_x_discrete(labels=c("Sham","SD-tRNS","HD-tRNS"))+
  theme(axis.title=element_text(margin=0.1,size=18),text =element_text(size=16) )
p_GameLevel
ggsave(plot=p_GameLevel,"Paper\\FINAL\\GameLevel.pdf",device="pdf",width=10,height=6)

kruskal=data_wide%>%
  kruskal_test(GameLevel~Group)

kruskal$p[kruskal$p<0.001]="<0.001"

grob_post=grobTree(textGrob(paste0("Group effect (Kruskal-Wallis): p = ",kruskal$p),
                            x=0.1,y=0.92,hjust=0,vjust=0,gp=gpar(fontsize=16)))

p_gameLevel=ggplot(data_wide,aes(Group,GameLevel,color=Group,fill=Group,shape=Group))+
  theme_pubr()+scale_fill_manual(values=couleurs_alpha)+
  scale_color_manual(values=couleurs)+
  geom_half_violin(width=1, position = position_nudge(x=-0.15,y=0))+
  geom_jitter(alpha=0.6,position=position_jitterdodge(jitter.width = 0.25,jitter.height = 0.1,dodge.width = 0.5))+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=1 ,show.legend = FALSE,geom="errorbar",width=0.08)+
  stat_summary(fun=mean, geom="point",size=4)+
  scale_y_continuous( breaks=seq(0,10,3),limits=c(0,11))+
  scale_x_discrete(labels=c("Sham","SD-tRNS","HD-tRNS"),expand=c(0.5,0))+
  ylab("Gaming Experience")+rremove("legend")+
  annotation_custom(grob_post)+
  theme(axis.title=element_text(margin=0.1,size=18),text =element_text(size=16))
p_gameLevel
ggsave(plot=p_gameLevel,"Paper\\FINAL\\GameLevelv2.pdf",device="pdf",width=10,height=6)


#PREPOST
require(memoise)
library(ggpattern)
data_prepost=read.csv("E:\\ISAE-2021\\Alldata\\PREPOST.csv",sep=";")
data_prepost$Group=factor(data_prepost$Group,levels=c("SHAM","STIMSD","STIMHD"))
levels(data_prepost$Group)=c("Sham","SD-tRNS","HD-tRNS")

data_prepost$PrePost=factor(data_prepost$PrePost,levels=c("Pre","Post"))
data_prepost$somme=data_prepost$somme-12
data_prepost[,5:16]=lapply(data_prepost[,5:16],function(x) x-1)

p_prepost=ggplot(data_prepost,aes(x=Group,y=somme,group=PrePost,color=Group,fill=Group,pattern=PrePost))+
  geom_bar_pattern(stat="summary",position=position_dodge(0.9),width=0.85,fun="mean",
                   aes(pattern=PrePost,pattern_color=Group,pattern_fill=Group),pattern_angle = 45,
                   pattern_density = 0.2,
                   pattern_spacing = 0.02)+
  scale_pattern_manual(values=c(Pre="none",Post="stripe"))+
  scale_pattern_fill_manual(values=couleurs)+
  scale_pattern_color_manual(values=couleurs_alpha)+
  scale_x_discrete(labels=c("Sham","SD-tRNS","HD-tRNS"))+
  stat_summary(fun.data = "mean_se",geom="errorbar", fun.args = list(mult = 1),
               position=position_dodge(0.9),show.legend = FALSE,width=0.3)+
  theme_pubr()+scale_color_manual(values=couleurs)+scale_fill_manual(values=couleurs_alpha)+
  guides(pattern = guide_legend(override.aes = list(fill = "white",color="black"),title = ""),
         fill = guide_legend(override.aes = list(pattern = "none")))+
  ylab("tRNS adverse effect Questionnaire Score")+scale_y_continuous(breaks = seq(0,5,1),limits = c(0,5))+
  theme(axis.title=element_text(margin=0.1,size=18),text =element_text(size=16),
        legend.position = c(0.2,0.85),legend.box = "horizontal")
p_prepost


p_post=ggplot(filter(data_prepost,PrePost=="Post"),aes(x=Group,y=somme,color=Group,fill=Group))+
  geom_bar(stat="summary",fun="mean")+
  stat_summary(fun.data = "mean_se",geom="errorbar", fun.args = list(mult = 1),show.legend = FALSE,width=0.2)+
  theme_pubr()+scale_color_manual(values=couleurs)+scale_fill_manual(values=couleurs_alpha)+
  ylab("tRNS adverse effect Questionnaire Score (Post Session)")+geom_point()+
  theme(axis.title=element_text(margin=0.1,size=18),text =element_text(size=16),legend.position = c(0.2,0.85),legend.box = "horizontal")
p_post

data_prepost%>%
  group_by(PrePost,Group)%>%
  summarise(mean(somme))

ggsave(plot=p_prepost,"Paper\\FINAL\\QuestionnairePrePost.pdf",device="pdf",width=10,height=6)

d_pp=data_prepost%>%
  group_by(PrePost,Group)%>%
  dplyr::summarise(across(Headache:Nauseau,mean))

d_pp=d_pp%>%gather(Question,Answer,Headache:Nauseau)
d_pp=d_pp%>%unite("GroupPrePost",PrePost:Group,remove=TRUE,sep="_")
d_pp=d_pp%>%spread(GroupPrePost,Answer)

ftable(Group+PrePost~Headache,data=data_prepost)
  
summary(data_prepost)
sapply(data_prepost[,5:16],sd)
library(tables)
library(weights)
tmp <- tabular( Headache+Difficulties.in.concentrating+Mood.swing+Flickering+Tingling+
                  Itching+Burning+Pain+Fatigue+Nervousness+Unpleasant.sensations+Nauseau ~ 
                  (Group*PrePost)* PlusMinus(mean,sd), data=data_prepost )
tmp[]=lapply(tmp,rd,2,add=FALSE)
tmp
latexTable(tmp)

library(table1)
my.render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits=2), c(sprintf("%s (&plusmn; %s)", MEAN, SD)))}

table1::table1(~Headache+Difficulties.in.concentrating+Mood.swing+Flickering+
         Tingling+Itching+Burning+Pain+Fatigue+Nervousness+Unpleasant.sensations+Nauseau|Group*PrePost,
       data=data_prepost,render.continuous=my.render.cont,overall = FALSE)


#Barplot likert
library(reshape2)
library(ggsci)
m=melt(data_prepost)

m=m%>%group_by(variable,value,Group,PrePost)%>%
  count(value)

m=subset(m,variable!="somme")
m$value=as.factor(m$value)
m=m%>%
  group_by(variable,Group,PrePost)%>%
  mutate(freq=(n/sum(n))*100)
#m$n=(m$n/sum(m$n[]))*100
data_wide$Group=factor(data_wide$Group,levels=c("SHAM","STIMSD","STIMHD"))

ggplot()+geom_bar(data=m,aes(x=reorder(variable,n),y=n,fill=value), position = position_stack(reverse = TRUE),width=0.5, stat="identity")+
  coord_flip() +theme_pubr()+scale_fill_jco()+
  theme(legend.position="bottom")+facet_grid(PrePost~Group)

library(forcats)
fct_reorder(m$variable,m$n,max)

prepost_levels=names(table(m$variable))[order(table(m$variable))]
m$variable=factor(m$variable,levels=prepost_levels)
p_questionnaire=ggplot()+
  geom_col(data=filter(m,PrePost=="Post"),aes(x=variable,y=freq,alpha=value,fill=Group),width=0.5,
           position= position_stack(reverse = TRUE))+
  coord_flip()+scale_fill_manual(values=couleurs)+theme_pubr()+
  facet_grid(~Group)+xlab("tRNS adverse effect Questions (Post Session)")+ylab("")+labs(alpha="Likert Scale")+
  scale_x_discrete(labels=c(Nauseau="Nausea",Unpleasant.sensations="Unpleasant Sensations",Mood.swing="Mood Swing",Difficulties.in.concentrating="Difficulties in concentrating"))+
  theme(strip.text.x = element_text(size = 14),axis.title=element_text(margin=0.1,size=18),text =element_text(size=16),strip.background = element_rect(colour="white", fill="white"),legend.position="bottom")+scale_alpha_discrete(range=c(0.3,1))+
  guides(fill=FALSE)
p_questionnaire

ggsave(plot=p_questionnaire,"Paper\\FINAL\\Questionnaire.pdf",device="pdf",width=10,height=6)

# Kruskal Wallis on mean average af the 3 days

data_post_mean=data_prepost%>%
  group_by(Pseudo,Group,PrePost)%>%
  summarise(somme=mean(somme))
data_post_mean$Group=factor(data_post_mean$Group,levels=c("SHAM","STIMSD","STIMHD"))

data_post_mean=subset(data_prepost,PrePost=="Post")
data_post_mean=data_post_mean%>%
  group_by(Pseudo,Group)%>%
  summarise(somme=mean(somme))

data_post_mean=ungroup(data_post_mean)
kruskal=data_post_mean%>%
  kruskal_test(somme~Group)
kruskal_test(somme~Group,data=data_post_mean)

ph_post=data_post_mean%>%
  dunn_test(somme~Group,p.adjust.method = "holm")

ph_post$p.adj.signif[ph_post$p.adj.signif=="ns"]=""
ph_post$p.adj=round(ph_post$p.adj,3)

ph_post$p.adj[ph_post$p.adj<0.001]="<0.001"
kruskal$p[kruskal$p<0.001]="<0.001"

grob_post=grobTree(text_grob(paste0("Group effect (Kruskal-Wallis): p = ",kruskal$p),
                            x=0.1,y=0.95,hjust=0,vjust=0,face="bold"),gp=gpar(fontsize=16))
p_post=ggplot(data_post_mean,aes(Group,somme,color=Group,fill=Group,shape=Group))+
  theme_pubr()+scale_fill_manual(values=couleurs_alpha)+
  scale_color_manual(values=couleurs)+
  geom_half_violin(width=1, position = position_nudge(x=-0.15,y=0))+
  geom_jitter(size=1.5,alpha=0.6,position=position_jitterdodge(jitter.width = 0.5,jitter.height = 0.1,dodge.width = 0.5))+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=1 ,show.legend = FALSE,geom="errorbar",width=0.08)+
  stat_summary(fun=mean, geom="point",size=4)+
  scale_y_continuous( breaks=seq(0,20,3),limits=c(-1,19))+
  scale_x_discrete(labels=c("Sham","SD-tRNS","HD-tRNS"),expand=c(0.5,0))+
  ylab("tRNS adverse effect Questionnaire Score \n(Post Session)")+rremove("legend")+
  add_pvalue(ph_post[1,],y.position=c(15.3),
             label = "p = {p.adj}", inherit.aes = FALSE,label.size=5)+
  add_pvalue(ph_post[2,],y.position=c(17.3),
             label = "p = {p.adj}", inherit.aes = FALSE,label.size=5,fontface="bold")+
  add_pvalue(ph_post[3,],y.position=c(16.3),
             label = "p = {p.adj}", inherit.aes = FALSE,label.size=5)+
  annotation_custom(grob_post)+
  theme(axis.title=element_text(margin=0.1,size=18),text =element_text(size=16))
p_post


ggsave(plot=p_post,"Paper\\FINAL\\Post.pdf",device="pdf",width=10,height=6)
figure_post=ggarrange(p_gameLevel,p_post,labels=c("A","B"))
figure_post
ggsave(plot=figure_post,"Paper\\FINAL\\FigurePostGameLevel.pdf",device="pdf",width=14,height=6)



###Thèse Quentin APM and ScM
library(ggExtra)
df_APM=data.frame(Pseudo=rep(df_APM_ScM$Pseudo,each=540),Group=rep(df_APM_ScM$Group,each=540),Session=rep(df_APM_ScM$Session,each=540),ScM=unlist(df_APM_ScM$ScoresMin),APM=unlist(df_APM_ScM$APM))

df_APM$Group=factor(df_APM$Group,levels=c("SHAM","STIMSD","STIMHD"))

ggplot(filter(df_APM,Session=="D14P2"), aes(x=ScM, y=APM) ) +
  geom_bin2d(bins=150)+scale_fill_gradient(low="lightgrey",high="black")+
  theme_pubr()+xlab("Score per Minute")+ylab("Action per Minute")+
  annotate(geom="text",label="r = .7, p <.001",x=-500,y=300)+
  geom_smooth(method="lm",color="red",size=1)
  #stat_cor(method = "pearson")

ggplot(filter(df_APM,Session=="D14P2"),aes(x=ScM,y=APM))+
  geom_point()+theme_pubr()+stat_cor(method = "pearson")+geom_smooth(method="lm",color="red",size=2)

# 
# p1=ggplot(filter(df_APM,Session=="D14P2"), aes(x=ScM, y=APM,fill=Group))+
#   scale_fill_manual(values=couleurs_alpha)+theme_pubr()+
#   stat_density_2d(geom = "polygon",aes(fill=Group),bins=3,alpha=0.3)
# p1
# 
# p1=ggMarginal(p1 ,type="density",groupColour = TRUE)
# p1
# p2=ggplot(filter(df_APM,Session=="D14P2"), aes(x=ScM, y=APM,color=Group)) +
#   geom_point(alpha=0.1) +scale_color_manual(values=couleurs)+theme_pubr()+xlim(-2000,3200)+ylim(0,400)
# p2=ggMarginal(p2 ,type="density",groupColour = TRUE)
# 
# ggarrange(p1,p2,nrow=1,ncol=2)


#fNIRS 
df <- read.csv("Data\\HbO_Hhb_absorption.csv",header=TRUE,sep = "\t", dec = ".")
df=df%>%rename(HbR=Hhb)

df_long <- df %>%
  gather(Type, Value,-Lambda)

ggplot(df_long, aes(x=Lambda, y=Value, color=Type)) + geom_line(size=1)+theme_pubr()+scale_y_log10()+scale_color_manual(values=c("red","blue"))+
  ylab("Molar extinction coefficient (1/cm*mM)")+xlab("Wavelength (nm)")+geom_vline(xintercept=c(760,850),linetype="dashed")+ggtitle("Absorption Spectra of Hemoglobin")+theme(legend.position=c(0.8,0.8))

#Imagerie
df=read.csv("Data\\Imagerie.csv",header=TRUE,sep=";",dec=",")
df$Image=as.factor(df$Image)
df$Mobility=as.factor(df$Mobility)
ggplot()+geom_rect(data=df,aes(xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax,color=Mobility,fill=Mobility),alpha=0.5)+theme_pubr()+
  scale_color_grey()+scale_fill_grey()+geom_text(data=df,aes(x=xmin+0.08,y=ymax-0.12,label=Image))+ylab("Satial Resolution")+xlab("Temporal Resolution")

#fNIRS 
df=read.csv("Data\\2chan_HbO.csv",header=TRUE,sep=";",dec=",")

p_A=ggplot(df)+theme_pubr()+geom_line(aes(Time,Channel.1,color="blue"))+geom_line(aes(Time,Channel.2,color="red"))+
  scale_color_discrete(name="",labels=c("Channel 1","Channel 2"))+ylab("HbO")+scale_y_continuous(limits=c(-35,35),breaks=seq(-30,30,10))+theme(legend.position=c(0.15, 0.15))
p_B=ggplot(df,aes(Channel.1,Channel.2))+geom_point(size=0.3)+theme_pubr()+annotate(geom="text",label="r = .81",x=-25,y=25)+xlab("Channel 1 (HbO)")+ylab("Channel 2 (HbO)")+
  scale_x_continuous(limits=c(-35,35),breaks=seq(-30,30,10))+scale_y_continuous(limits=c(-35,35),breaks=seq(-30,30,10))+stat_smooth(method="lm",color="red",size=1)
#  stat_cor(aes(label=..r.label..),cor.coef.name=c("r"),digits=2)

ggarrange(p_A,p_B,nrow=1,ncol=2,labels = c("A","B"),align="v")


#####
#different type of transcranial electrical stimulation
library(truncnorm)

x=seq(0,10,by=0.01)
#y_SHAM=c(1*x[1:10],rep(0,length(x)-200),-1*x[(length(x)-99):length(x)]+10)

y_SHAM=c(2*x[1:50],-2*x[51:100]+2,rep(0,length(x)-200),
         2*x[2:51],-2*x[52:101]+2)

#SHAM
sham_anodal=geom_line(aes(x,y_SHAM),size=0.5)
sham_cathodal=geom_line(aes(x,-y_SHAM),size=0.5)
p_sham=ggplot() +theme_pubr()+
  theme(axis.line.x=element_blank(),axis.text.x = element_blank(),axis.title = element_blank(),axis.ticks = element_blank())+
  geom_hline(yintercept = 0)+
  geom_segment(aes(x=c(1,9),xend=c(1,9),y=-1.1,yend=1.1),linetype="dashed")+
  scale_y_continuous(breaks=c(-1,0,1),labels=c("-","0","+"),limits=c(-1.1,1.3))

p_sham_anodal=p_sham+sham_anodal+annotate("text",label="Sham (Anodal tDCS)",x=5,y=1.3)
p_sham_anodal

#tDCS
x=seq(0,10,by=0.01)
y_tDCS_anodal=c(1*x[1:100],rep(1,length(x)-200),-1*x[(length(x)-99):length(x)]+10)
p_tDCS_anodal=ggplot() +theme_pubr()+
  theme(axis.line.x=element_blank(),axis.text.x = element_blank(),axis.title = element_blank(),axis.ticks = element_blank())+
  geom_hline(yintercept = 0)+
  annotate("text",label="Anodal tDCS",x=5,y=1.3)+
  scale_y_continuous(breaks=c(-1,0,1),labels=c("-","0","+"),limits=c(-1.1,1.3))+
  geom_segment(aes(x=c(1,9),xend=c(1,9),y=-1.1,yend=1.1),linetype="dashed")+
  geom_line(aes(x,y_tDCS_anodal),size=0.5)
 
y_tDCS_cathodal=c(-1*x[1:100],rep(-1,length(x)-200),1*x[(length(x)-99):length(x)]-10)
p_tDCS_cathodal=ggplot() +theme_pubr()+
  theme(axis.line.x=element_blank(),axis.text.x = element_blank(),axis.title = element_blank(),axis.ticks = element_blank())+
  geom_hline(yintercept = 0)+
  annotate("text",label="Cathodal tDCS",x=5,y=1.3)+
  scale_y_continuous(breaks=c(-1,0,1),labels=c("-","0","+"),limits=c(-1.1,1.3))+
  geom_segment(aes(x=c(1,9),xend=c(1,9),y=-1.1,yend=1.1),linetype="dashed")+
  geom_line(aes(x,y_tDCS_cathodal),size=0.5)
 
p_sham_cathodal=p_sham+sham_cathodal+annotate("text",label="Sham (Cathodal tDCS)",x=5,y=1.3)
#tACS
alpha=8
y_tACS_sham=c(seq(0,0.99,by=0.02)*cos(pi*alpha*x[1:50]),seq(0.99,0,by=-0.02)*cos(pi*alpha*x[51:100]),
              rep(0,length(x)-200),seq(0,0.99,by=0.02)*cos(alpha*pi*x[902:951]),
              seq(0.99,0,by=-0.02)*cos(alpha*pi*x[952:1001]))
sham_tACS=geom_line(aes(x,y_tACS_sham),size=0.5)
x=seq(0,10,by=0.01)
y_tACS=c(seq(0,0.99,by=0.01)*cos(alpha*pi*x[1:100]),cos(alpha*pi*x[101:(length(x)-100)]),seq(0.99,0,by=-0.01)*cos(alpha*pi*x[902:1001]))
p_tACS=ggplot() +theme_pubr()+
  theme(axis.line.x=element_blank(),axis.text.x = element_blank(),axis.title = element_blank(),axis.ticks = element_blank())+
  geom_hline(yintercept = 0)+
  annotate("text",label="tACS",x=5,y=1.3)+
  scale_y_continuous(breaks=c(-1,0,1),labels=c("-","0","+"),limits=c(-1.1,1.3))+
  geom_segment(aes(x=c(1,9),xend=c(1,9),y=-1.1,yend=1.1),linetype="dashed")+
  geom_line(aes(x,y_tACS),size=0.5)

p_sham_tACS=p_sham+sham_tACS+annotate("text",label="Sham (tACS)",x=5,y=1.3)
p_sham_tACS
p_tACS
#tRNS
s=sample(100:900,801,replace=T)
x_tRNS=c((1:100),s,(902:1001))
up_sham=seq(0,0.99,by=0.02)*rtruncnorm(n=50, a=-1, b=1, mean=0, sd=0.33)
down_sham=seq(0,0.99,by=0.02)*rtruncnorm(n=50, a=-1, b=1, mean=0, sd=0.33)

#down=seq(0.99,0,by=-0.01)*rtruncnorm(n=100, a=-1, b=1, mean=0, sd=0.33)

y_tRNS_sham =c(up_sham,rev(down_sham),rep(0,length(x_tRNS)-200),up_sham,rev(down_sham))
sham_tRNS=geom_line(aes(x_tRNS,y_tRNS_sham),size=0.5)
up=seq(0,0.99,by=0.01)*rtruncnorm(n=100, a=-1, b=1, mean=0, sd=0.33)
down=seq(0.99,0,by=-0.01)*rtruncnorm(n=100, a=-1, b=1, mean=0, sd=0.33)
y_tRNS =c(up,rtruncnorm(n=801, a=-1, b=1, mean=0, sd=0.33),down)
p_tRNS=ggplot() +theme_pubr()+
  theme(axis.line.x=element_blank(),axis.text.x = element_blank(),axis.title = element_blank(),axis.ticks = element_blank())+
  annotate("text",label="tRNS",x=500,y=1.3)+
  geom_hline(yintercept = 0)+
  scale_y_continuous(breaks=c(-1,0,1),labels=c("-","0","+"),limits=c(-1.1,1.3))+
  geom_segment(aes(x=c(100,900),xend=c(100,900),y=-1.1,yend=1.1),linetype="dashed")+
  geom_line(aes(x_tRNS,y_tRNS),size=0.5)

p_sham_tRNS=ggplot() +theme_pubr()+
  theme(axis.line.x=element_blank(),axis.text.x = element_blank(),axis.title = element_blank(),axis.ticks = element_blank())+
  geom_hline(yintercept = 0)+
  geom_segment(aes(x=c(100,900),xend=c(100,900),y=-1.1,yend=1.1),linetype="dashed")+
  annotate("text",label="Sham (tRNS)",x=500,y=1.3)+
  scale_y_continuous(breaks=c(-1,0,1),labels=c("-","0","+"),limits=c(-1.1,1.3))+sham_tRNS
p_sham_tRNS

ggarrange(p_tDCS_anodal,p_sham_anodal,p_tDCS_cathodal,p_sham_cathodal,
          p_tACS,p_sham_tACS,p_tRNS,
          p_sham_tRNS,nrow=4,ncol=2,align="hv")



#tRNS percentage HD-montage
#tRNS
s=sample(100:900,801,replace=T)
x_tRNS=c((1:100),s,(902:1001))
up_sham=seq(0,0.99,by=0.02)*rtruncnorm(n=50, a=-1, b=1, mean=0, sd=0.33)
down_sham=seq(0,0.99,by=0.02)*rtruncnorm(n=50, a=-1, b=1, mean=0, sd=0.33)

#down=seq(0.99,0,by=-0.01)*rtruncnorm(n=100, a=-1, b=1, mean=0, sd=0.33)

y_tRNS_sham =c(up_sham,rev(down_sham),rep(0,length(x_tRNS)-200),up_sham,rev(down_sham))
sham_tRNS=geom_line(aes(x_tRNS,y_tRNS_sham),size=0.5)
up=seq(0,0.99,by=0.01)*rtruncnorm(n=100, a=-1, b=1, mean=0, sd=0.33)
down=seq(0.99,0,by=-0.01)*rtruncnorm(n=100, a=-1, b=1, mean=0, sd=0.33)
y_tRNS =c(up,rtruncnorm(n=801, a=-1, b=1, mean=0, sd=0.33),down)
p_tRNS=ggplot() +theme_pubr()+
  theme(axis.line.x=element_blank(),axis.text.x = element_blank(),axis.title = element_blank(),axis.ticks = element_blank())+
  annotate("text",label="tRNS",x=500,y=1.3)+
  geom_hline(yintercept = 0)+
  scale_y_continuous(breaks=c(-1,0,1),labels=c("-1","0","1"),limits=c(-1.1,1.3))+
  geom_segment(aes(x=c(100,900),xend=c(100,900),y=-1.1,yend=1.1),linetype="dashed")+
  geom_line(aes(x_tRNS,y_tRNS),size=0.5)
p_tRNS_23=ggplot() +theme_pubr()+
  theme(axis.line.x=element_blank(),axis.text.x = element_blank(),axis.title = element_blank(),axis.ticks = element_blank())+
  annotate("text",label="tRNS",x=500,y=1.3)+
  geom_hline(yintercept = 0)+
  scale_y_continuous(breaks=c(-1,0,1),labels=c("-1","0","1"),limits=c(-1.1,1.3))+
  geom_segment(aes(x=c(100,900),xend=c(100,900),y=-1.1,yend=1.1),linetype="dashed")+
  geom_line(aes(x_tRNS,y_tRNS),size=0.5,alpha=0.15)+
  geom_line(aes(x_tRNS,-0.23*y_tRNS),size=0.5)
p_tRNS_27=ggplot() +theme_pubr()+
  theme(axis.line.x=element_blank(),axis.text.x = element_blank(),axis.title = element_blank(),axis.ticks = element_blank())+
  annotate("text",label="tRNS",x=500,y=1.3)+
  geom_hline(yintercept = 0)+
  scale_y_continuous(breaks=c(-1,0,1),labels=c("-1","0","1"),limits=c(-1.1,1.3))+
  geom_segment(aes(x=c(100,900),xend=c(100,900),y=-1.1,yend=1.1),linetype="dashed")+
  geom_line(aes(x_tRNS,y_tRNS),size=0.5,alpha=0.15)+
  geom_line(aes(x_tRNS,-0.27*y_tRNS),size=0.5)


p_tRNS=p_tRNS+theme(legend.background = element_rect(fill = "transparent"),
                          legend.box.background = element_rect(fill = "transparent"),
                          panel.background = element_rect(fill = "transparent"),
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(),
                          plot.background = element_rect(fill = "transparent", color = NA))+
                          coord_cartesian(xlim=c(200,400))
p_tRNS_27=p_tRNS_27+theme(legend.background = element_rect(fill = "transparent"),
                          legend.box.background = element_rect(fill = "transparent"),
                          panel.background = element_rect(fill = "transparent"),
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank(),
                          plot.background = element_rect(fill = "transparent", color = NA))+
                          coord_cartesian(xlim=c(200,400))

p_tRNS_23=p_tRNS_23+theme(legend.background = element_rect(fill = "transparent"),
                    legend.box.background = element_rect(fill = "transparent"),
                    panel.background = element_rect(fill = "transparent"),
                    panel.grid.major = element_blank(),
                    panel.grid.minor = element_blank(),
                    plot.background = element_rect(fill = "transparent", color = NA))+
                    coord_cartesian(xlim=c(200,400))
p_tRNS_23
ggsave(p_tRNS,filename="p_tRNS_transparent.png",bg = "transparent",width=6,height=3)
ggsave(p_tRNS_27,filename="p_tRNS27_transparent.png",bg = "transparent",width=6,height=3)
ggsave(p_tRNS_23,filename="p_tRNS23_transparent.png",bg = "transparent",width=6,height=3)
