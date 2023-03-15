#VISU ON ALL GROUPS (pour présentation modex)

data_wide = read.csv("E:\\SpaceFortress\\Paper\\CleanData\\data_wide.csv") #here insert the data_long path
couleurs=c("#868686FF","#0073C2FF","#A73030FF")
couleurs_alpha=c("#86868666","#0073C266","#A7303099")
data_wide$Group=factor(data_wide$Group,levels=c("SHAM","STIMSD","STIMHD"))

#ANCOVAs
ancova_total_LT = data_wide%>%
  anova_test(DeltaD1D14~Group*GameLevelLog)
ancova_total_ST = data_wide%>%
  anova_test(DeltaD1D5~Group*GameLevelLog)
ancova_total_RT =data_wide%>%
  anova_test(DeltaD14D5~Group*GameLevelLog)
ancova_fortress_LT = data_wide%>%
  anova_test(DeltaD1D14Fortress~Group*GameLevelLog)
ancova_fortress_ST = data_wide%>%
  anova_test(DeltaD5D1Fortress~Group*GameLevelLog)
ancova_fortress_RT = data_wide%>%
  anova_test(DeltaD14D5Fortress~Group*GameLevelLog)

#Short Term & Log Term
d_STLT=subset(data_wide,select=c("Group","DeltaD1D5","DeltaD1D14","DeltaD5D1Fortress","DeltaD1D14Fortress"))
d_STLT=melt(d_STLT,id.vars = "Group")
colnames(d_STLT)=c("Group","Delta","Value")
d_STLT$Delta=as.factor(d_STLT$Delta)
p_text_STLT=data.frame(label=c(rep("Group effect :",4),rep("Game Level effect :",4)),Delta=c("DeltaD1D5","DeltaD1D14","DeltaD5D1Fortress","DeltaD1D14Fortress"),x=c(1,2,3,4,1,2,3,4),y=c(rep(20000,4),rep(21500,4)),p= c(ancova_total_ST$p[1],ancova_total_LT$p[1],ancova_fortress_ST$p[1],ancova_fortress_LT$p[1],ancova_total_ST$p[2],ancova_total_LT$p[2],ancova_fortress_ST$p[2],ancova_fortress_LT$p[2]))

p_STLT=ggplot(d_STLT,aes(Delta,Value,color=Group,fill=Group,shape=Group))+theme_pubr()+
  geom_half_violin()+geom_half_point(show.legend = T,alpha=0.6)+
  stat_summary(aes(Delta,Value),fun.data = "mean_se", fun.args = list(mult = 1),size=0.5 ,show.legend = FALSE,geom="errorbar",width=0.1,position = position_dodge(width=0.3))+
  stat_summary(geom="point",fun="mean",size=2,position = position_dodge(width=0.3),color="black",show.legend = F)+
  scale_color_manual(values=couleurs,labels = c("Sham","StimSD","StimHD"))+
  scale_fill_manual(values=couleurs_alpha,labels = c("Sham","StimSD","StimHD"))+
  scale_shape_manual(values=c(16,17,15),labels = c("Sham","StimSD","StimHD"))+
  labs(y ="Delta Scores",title="Total and Fortress Delta Scores on Short-Term and Long-Term")+
  theme(axis.title.x = element_blank(), plot.margin = unit(c(1, 1, 4, 1), "lines"),plot.title=element_text(hjust=0.5))+
  scale_x_discrete(labels=c("Short-Term","Long-Term","Short-Term","Long-Term"))+
  geom_text(p_text_STLT,mapping = aes(x = x, y = y, label=ifelse( p<0.05,paste(label,p),paste(label,"ns"))), inherit.aes = FALSE,fontface = ifelse(p_text_STLT$p<0.05,2,1) )+
  coord_cartesian(clip = "off") +annotate(geom="text",x=c(1.5,3.5),y=-1000,label=c("Total Score","Fortress Score"),size=5,vjust=4)+
  annotate(geom="text",x=c(0.7,1,1.3,1.7,2,2.3,2.7,3,3.3,3.7,4,4.3),y=-1000,label=rep(c("Sham","Stim SD","Stim HD"),4))
p_STLT

#Retention
d_RT=subset(data_wide,select=c("Group","DeltaD14D5","DeltaD14D5Fortress"))
d_RT=melt(d_RT,id.vars = "Group")
colnames(d_RT)=c("Group","Delta","Value")
d_RT$Delta=as.factor(d_RT$Delta)
p_text_RT=data.frame(label=c(rep("Group effect :",2),rep("Game Level effect :",2)),Delta=c("DeltaD14P1D5","DeltaD14P1D5Fortress"),x=c(1,2,1,2),y=c(rep(8000,2),rep(9000,2)),p= c(ancova_total_RT$p[1],ancova_fortress_RT$p[1],ancova_total_RT$p[2],ancova_fortress_RT$p[2]))

p_RT=ggplot(d_RT,aes(Delta,Value,color=Group,fill=Group,shape=Group))+theme_pubr()+
  geom_half_violin()+geom_half_point(show.legend = T,alpha=0.6)+stat_summary(aes(Delta,Value),fun.data = "mean_se", fun.args = list(mult = 1),size=0.5 ,show.legend = F,geom="errorbar",width=0.1,position = position_dodge(width=0.3))+
  stat_summary(geom="point",fun="mean",size=2,position = position_dodge(width=0.3),color="black",show.legend = F)+
  scale_color_manual(values=couleurs,labels = c("Sham","Stim SD","Stim HD"))+
  scale_fill_manual(values=couleurs_alpha,labels = c("Sham","Stim SD","Stim HD"))+
  scale_shape_manual(values=c(16,17,15),labels = c("Sham","Stim SD","Stim HD"))+
  labs(y ="Delta Scores",title="Total and Fortress Delta Scores on Retention")+
  theme(axis.title.x = element_blank(), plot.margin = unit(c(1, 1, 4, 1), "lines"),plot.title=element_text(hjust=0.5))+
  scale_x_discrete(labels=c("Retention","Retention"))+
  geom_text(p_text_RT,mapping = aes(x = x, y = y, label=ifelse( p<0.05,paste(label,p),paste(label,"ns"))), inherit.aes = FALSE,fontface = ifelse(p_text_RT$p<0.05,2,1) )+
  coord_cartesian(clip = "off")+
  annotate(geom="text",x=c(1,2),y=-5000,label=c("Total Score","Fortress Score"),size=5,vjust=4)+
  annotate(geom="text",x=c(0.7,1,1.3,1.7,2,2.3),y=-5000,label=c("Sham","Stim SD","Stim HD","Sham","Stim SD","Stim HD"))
p_RT

fig= ggarrange(p_STLT,p_RT,common.legend = T,nrow=2,ncol=1,align = "hv")
fig

#####
#LEARNING RATE

#AKAIKE CRITERION
fit_ln=lm(Fortress~ln(D),data=data_long) #fit_all_lin=lm(TotalScore~D,data=data_long)
fit_lin=lm(Fortress~D,data=data_long)
AIC(fit_ln)
AIC(fit_lin)

#Learning Rate by groups
fit_SHAM=lm(Fortress~D,data=filter(data_long,Group=="SHAM"))
fit_SD=lm(Fortress~D,data=filter(data_long,Group=="STIMSD"))
fit_HD=lm(Fortress~D,data=filter(data_long,Group=="STIMHD"))


eq_SHAM=paste0("y= ",round(coef(fit_SHAM)[1])," + ",round(coef(fit_SHAM)[2]),"*x     ")
eq_SD=paste0("y= ",round(coef(fit_SD)[1])," + ",round(coef(fit_SD)[2]),"*x")
eq_HD=paste0("y= ",round(coef(fit_HD)[1])," + ",round(coef(fit_HD)[2]),"*x")
grob_SHAM=grobTree(textGrob(eq_SHAM,x=0.77,y=0.195,hjust=0,gp=gpar(col="#868686FF")),gp=gpar(fontsize=15))
grob_SD=grobTree(textGrob(eq_SD,x=0.77,y=0.145,hjust=0,gp=gpar(col="#0073C2FF")),gp=gpar(fontsize=15))
grob_HD=grobTree(textGrob(eq_HD,x=0.77,y=0.095,hjust=0,gp=gpar(col="#A73030FF")),gp=gpar(fontsize=15))

plot_LR=ggplot(data_long,aes(D,Fortress,color=Group,shape=Group))+theme_pubr()+
  scale_x_continuous(sec.axis=sec_axis(~.,breaks=c(1,4.5,8.5,10.5),labels=c("Référence","Entraînement & stimulation","Court terme","Long terme")),breaks=1:11)+
  geom_rect(data=data_long,aes(xmin=1.5,xmax=7.5,ymin=-Inf,ymax=+Inf),fill="grey",alpha=0.01,inherit.aes = FALSE)+
  geom_vline(xintercept = seq(1.5,7.5,2),linetype="dotted",alpha=0.5)+geom_vline(xintercept =9.5,alpha=0.3,linetype="solid",size=0.5)+
  stat_smooth(method=lm,formula=y~x,se=FALSE,show.legend = FALSE )+
  stat_summary(geom="point",fun="mean",size=3 ,position=position_dodge(width=0.5))+
  labs(x="Game Session",y="Fortress Score")+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=1 ,show.legend = FALSE,geom="errorbar",width=0.1 ,position=position_dodge(width=0.5))+
  scale_y_continuous(breaks =seq(0, 15000, by = 2500))+
  scale_colour_manual(values=couleurs)+
  annotation_custom(grob_SHAM)+ annotation_custom(grob_SD)+ annotation_custom(grob_HD)+
  theme(legend.position = c(0.7,0.15),legend.background = element_rect(fill=NA),legend.title = element_blank(),
        axis.title = element_text(size=18,margin=0.1),legend.text = element_text(size=16),text=element_text(size=16))+
  annotate("text",x=c(1,2.5,4.5,6.5,8.5,10.5),y=Inf,vjust=1.5,label=c("D1","D2","D3","D4","D5","D15"),size=5)
plot_LR


#ANCOVA 
#ANCOVA LR
ancova_LR=data_wide%>%
  anova_test(LRFortress~Group+GameLevelLog) #EFFET DU GROUP p = 0.091

grob_LR=grobTree(textGrob(paste0("Group effect: p = ",toString(round(ancova_LR$p[1],3)),ancova_LR$`p<.05`[1]),
                          x=0.25,y=0.9,hjust=0,vjust=0),
                 text_grob(paste0("Gaming Experience effect: p = ",toString(round(ancova_LR$p[2],3))),
                           x=0.25,y=0.85,hjust=0,vjust=0,face="bold")
                 ,gp=gpar(fontsize=20))
ph_LRFortress=data_wide%>%
  emmeans_test(LRFortress~Group,covariate = GameLevelLog,p.adjust.method = "holm")

grob_LRFortress=grobTree(text_grob(paste0("Group effect: p = ",toString(round(ancova_LR$p[1],3))),
                              x=0.1,y=0.95,hjust=0,vjust=0),
                    text_grob(paste0("\nGaming Experience effect: p = ",toString(round(ancova_LR$p[2],3))),
                              x=0.1,y=0.9,hjust=0,vjust=0,face="bold"),
                    gp=gpar(fontsize=16))

ph_LRFortress$p.adj.signif[ph_LRFortress$p.adj.signif=="ns"]=""

p_ancovaLR=ggplot(data_wide,aes(Group,DeltaD14D5,color=Group,fill=Group,shape=Group))+
  theme_pubr()+scale_fill_manual(values=couleurs_alpha)+
  scale_color_manual(values=couleurs)+
  geom_half_violin(width=0.3, position = position_nudge(x=-0.2,y=0))+geom_jitter(width=0.1)+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=1 ,show.legend = FALSE,geom="errorbar",width=0.1)+
  stat_summary(fun=mean, geom="point",size=4)+
  ylab("Learning Rate (Fortress Score)")+
  rremove("legend")+scale_x_discrete(labels=c("Sham","SD-tRNS","HD-tRNS"))+
  scale_y_continuous( breaks=seq(-4000,10000,2000),limits=c(-4000,12000))+
  add_pvalue(ph_LRFortress[1,],y.position=7500,
             label = "p = {round(p.adj,3)} ", inherit.aes = FALSE,label.size=5)+
  add_pvalue(ph_LRFortress[2,],y.position=9500,
             label = "p = {round(p.adj,3)}", inherit.aes = FALSE,label.size=5)+
  add_pvalue(ph_LRFortress[3,],y.position=8500,
             label = "p = {round(p.adj,3)}", inherit.aes = FALSE,label.size=5)+
  annotation_custom(grob_LRFortress)+
  theme(axis.title=element_text(size=18,margin=0.1),text =element_text(size=16) )
p_ancovaLR
