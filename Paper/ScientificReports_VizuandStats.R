#R Analysis and plots
#Scientific Reports paper

#DATA
#Data are extracted from SF raw .txt files into long or wide format 

data_long = read.csv("E:\\SpaceFortress\\Paper\\CleanData\\data_long.csv") #here insert the data_long path
data_wide = read.csv("E:\\SpaceFortress\\Paper\\CleanData\\data_wide.csv") #here insert the data_long path
data_prepost=read.csv("E:\\SpaceFortress\\Paper\\CleanData\\PREPOST.csv",sep=";") #here insert the data_prepost path
#PARAMETERS

couleurs=c("#868686FF","#0073C2FF","#A73030FF")
couleurs_alpha=c("#86868666","#0073C266","#A7303099")
data_wide$Group=factor(data_wide$Group,levels=c("SHAM","STIMSD","STIMHD"))
data_long$Group=factor(data_long$Group,levels=c("SHAM","STIMSD","STIMHD"))
data_long$D=1:11

#Libraries
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
library(patchwork)
library(reshape2)
library(ggsci)
library(forcats)
library(SciViews)
#####
#Figure 2
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
  scale_x_continuous(sec.axis=sec_axis(~.,breaks=c(1,4.5,8.5,10.5),labels=c("Référence","Entraînement & stimulation","Court terme","Long terme")),breaks=1:11)+
  geom_rect(data=data_long,aes(xmin=1.5,xmax=7.5,ymin=-Inf,ymax=+Inf),fill="grey",alpha=0.01,inherit.aes = FALSE)+
  geom_vline(xintercept = seq(1.5,7.5,2),linetype="dotted",alpha=0.5)+geom_vline(xintercept =9.5,alpha=0.3,linetype="solid",size=0.5)+
  stat_smooth(method=lm,formula=y~ln(x),se=FALSE,show.legend = FALSE )+
  stat_summary(geom="point",fun="mean",size=3 ,position=position_dodge(width=0.5))+
  labs(x="Game Session",y="Total Score")+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=1 ,show.legend = FALSE,geom="errorbar",width=0.1 ,position=position_dodge(width=0.5))+
  scale_y_continuous(breaks =seq(0, 15000, by = 2500))+
  scale_colour_manual(values=couleurs)+
  annotation_custom(grob_SHAM)+ annotation_custom(grob_SD)+ annotation_custom(grob_HD)+
  theme(legend.position = c(0.7,0.15),legend.background = element_rect(fill=NA),legend.title = element_blank(),
        axis.title = element_text(size=18,margin=0.1),legend.text = element_text(size=16),text=element_text(size=16))+
  annotate("text",x=c(1,2.5,4.5,6.5,8.5,10.5),y=Inf,vjust=1.5,label=c("D1","D2","D3","D4","D5","D15"),size=5)
plot_LR
#ggsave(plot=plot_LR,"Figure2.pdf",device="pdf",width=10,height=6)

#####
#Figure 3
#ANCOVA LR
ancova_LR=data_wide%>%
  anova_test(LearningRateLT~Group+GameLevelLog) #EFFET DU GROUP p = 0.091

grob_LR=grobTree(textGrob(paste0("Group effect: p = ",toString(round(ancova_LR$p[1],3)),ancova_LR$`p<.05`[1]),
                          x=0.25,y=0.9,hjust=0,vjust=0),
                 text_grob(paste0("Gaming Experience effect: p = ",toString(round(ancova_LR$p[2],3))),
                           x=0.25,y=0.85,hjust=0,vjust=0,face="bold")
                 ,gp=gpar(fontsize=16))

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
#ggsave(plot=p_ancovaLR,"Paper\\FINAL\\LearningRateAncova_small.pdf",device="pdf",width=7,height=6)

#####
#Figure 4
#ANCOVA DELTA
#S11-S9
ancova_S11S9=data_wide%>%
  anova_test(DeltaD14D5~Group+GameLevelLog) #EFFET DU GROUP p = 0.018
ph_S11S9=data_wide%>%
  emmeans_test(DeltaD14D5~Group,covariate = GameLevelLog,p.adjust.method = "holm")

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
  theme(axis.title=element_text(size=18,margin=0.1),text =element_text(size=16) )#+

#S11-S1
ancova_S11S1=data_wide%>%
  anova_test(DeltaD1D14~Group+GameLevelLog) #EFFET DU GROUP p = 0.02
ph_S11S1=data_wide%>%
  emmeans_test(DeltaD1D14~Group,covariate = GameLevelLog,p.adjust.method = "holm")

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

#S9-S1
ancova_S9S1=data_wide%>%
  anova_test(DeltaD1D5~Group+GameLevelLog) #PAS D'EFFET DU GROUP 
grob_S9S1=grobTree(text_grob(paste0("Group effect: p = ",toString(round(ancova_S9S1$p[1],3)),ancova_S9S1$`p<.05`[1]),
                             x=0.1,y=0.95,hjust=0,vjust=0),
                   text_grob(paste0("Gaming Experience effect: p = ",toString(round(ancova_S9S1$p[2],3)))
                             ,x=0.1,y=0.9,hjust=0,vjust=0,face="bold")
                   ,gp=gpar(fontsize=16))

txt_S9S1=paste0("Group effect: p=",toString(round(ancova_S9S1$p[1],3)),ancova_S9S1$`p<.05`[1],
                "\nGaming Experience effect: p=",toString(round(ancova_S9S1$p[2],3)),ancova_S9S1$`p<.05`[2])


p_S9S1=ggplot(data_wide,aes(Group,DeltaD1D5,color=Group,fill=Group,shape=Group))+
  theme_pubr()+scale_fill_manual(values=couleurs_alpha)+
  scale_color_manual(values=couleurs)+
  geom_half_violin(width=0.8, position = position_nudge(x=-0.15,y=0))+geom_jitter(width=0.1)+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=1 ,show.legend = FALSE,geom="errorbar",width=0.1)+
  stat_summary(fun=mean, geom="point",size=4)+
  ylab("Delta Performance (Short Term)")+
  rremove("legend")+scale_x_discrete(labels=c("Sham","SD-tRNS","HD-tRNS"))+
  scale_y_continuous( breaks=seq(1500,18000,2000),limits=c(1000,18500))+
  annotation_custom(grob_S9S1)+
  theme(axis.title=element_text(size=18,margin=0.1),text =element_text(size=16))

f_delta=( p_S9S1|p_S11S1) /(plot_spacer() + p_S11S9 + plot_spacer() + plot_layout(widths = c(1,2,1)))+
  plot_annotation(tag_levels = "a")&theme(plot.tag.position = c(0.01,0.95),plot.tag = element_text(size=22,face = 'bold'))
#ggsave(plot=f_delta,"Paper\\FINAL\\FigureDelta.pdf",device="pdf",width=14,height=12)


#####
#SUPPLEMENTARY
#PREPOST questionnaire data extraction
data_prepost$Group=factor(data_prepost$Group,levels=c("SHAM","STIMSD","STIMHD"))
levels(data_prepost$Group)=c("Sham","SD-tRNS","HD-tRNS")

data_prepost$PrePost=factor(data_prepost$PrePost,levels=c("Pre","Post"))
data_prepost$somme=data_prepost$somme-12
data_prepost[,5:16]=lapply(data_prepost[,5:16],function(x) x-1)

#Figure 6
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
#ggsave(plot=p_gameLevel,"Paper\\FINAL\\GameLevelv2.pdf",device="pdf",width=10,height=6)

#Figure 7
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


#Figure 8
#Barplot likert
m=melt(data_prepost)
m=m%>%group_by(variable,value,Group,PrePost)%>%
  count(value)
m=subset(m,variable!="somme")
m$value=as.factor(m$value)
m=m%>%
  group_by(variable,Group,PrePost)%>%
  mutate(freq=(n/sum(n))*100)
#m$n=(m$n/sum(m$n[]))*100
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
#ggsave(plot=p_questionnaire,"Paper\\FINAL\\Questionnaire.pdf",device="pdf",width=10,height=6)
