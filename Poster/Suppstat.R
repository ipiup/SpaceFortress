### Supp Statistics for Poster
#####DATA & librairies

data_wide = read.csv("E:\\SpaceFortress\\Paper\\CleanData\\data_wide.csv") #here insert the data_long path
data_long = read.csv("E:\\SpaceFortress\\Paper\\CleanData\\data_long.csv") #here insert the data_long path
couleurs=c("#868686FF","#0073C2FF","#A73030FF")
couleurs_alpha=c("#86868666","#0073C266","#A7303099")
data_wide$Group=factor(data_wide$Group,levels=c("SHAM","STIMSD","STIMHD"))
data_long$Group=factor(data_long$Group,levels=c("SHAM","STIMSD","STIMHD"))
data_long$D=1:11

library(dplyr)
library(foreach)
library(ggplot2)
library(ggpubr)
library(rstatix)
library(gghalves)
library(tidyr)
library(reshape2)
library(patchwork)
library(ggprism)
library(grid)
#####
#Nb destroyed fortress
destroyed_fortress<-function(file){return(sum(file$Type=="FortressDestruction"))}
#Flight bad events
flight_bad_event<-function(file){return(sum(file$Type=="BorderCrossing"|file$Type=="FortessCollision"|file$Type=="ShipDamage"))}
#Temp passé à détruire des mines
mine_kill_time <- function(file){
  df_mine=subset(file,Group=="Mine"|Type=="NewMine"|(Type=="ShipDamage" & e1=="collide" & e2 == "mine_0"))
  j=1
  #killtime=0
  l_rt=c()
  for(n in 1:nrow(df_mine)){
    j=n
    tmp = FALSE
    if(df_mine[n,"Type"]=="NewMine"){
      t_newmine = df_mine[n,"system_time"]
      while(!tmp && j<nrow(df_mine)){
        j=j+1
        print(n)
        if(df_mine[j,"Type"]=="FriendMineDestruction"|df_mine[j,"Type"]=="FoeMineDestruction"){
          t_destrmine = df_mine[j,"system_time"]
          #killtime=killtime+(t_destrmine - t_newmine)
          l_rt = append(l_rt,t_destrmine - t_newmine)
          tmp=TRUE
        }else if (df_mine[j,"Type"]=="NewMine"|df_mine[j,"Type"]=="MineExtinction"|df_mine[j,"Type"]=="ShipDamage"){
          tmp=TRUE
        }
      }
    }
  }
  
  return(mean(l_rt))
}
#Proportion Bonus
shot_bonus<-function(file){return(sum(file$Type=="ShotsBonusCapture")/sum(file$Type=="NewBonus"))}
point_bonus<-function(file){return(sum(file$Type=="PointsBonusCapture")/sum(file$Type=="NewBonus"))}
bonus_failure<-function(file){return(sum(file$Type=="BonusFailure")/sum(file$Type=="NewBonus"))}
missed_bonus<-function(file){return((sum(file$Type=="NewBonus")-sum(file$Type=="ShotsBonusCapture")-sum(file$Type=="PointsBonusCapture"))/sum(file$Type=="NewBonus"))}

path_clean="E:\\ISAE-2021\\Alldata\\Data_clean\\"
files_data=list.files(path=path_clean,recursive = T) 
d=foreach(i=1:length(files_data),.combine=rbind)%do%{
  print(i)
  file=read.table(paste0(path_clean,"/",files_data[i]), header=TRUE, sep="\t",dec=".",fill=TRUE)
  #data.frame(file$Session[1],file$Pseudo[1],destroyed_fortress(file),mine_kill_time(file),flight_bad_event(file),shot_bonus(file),point_bonus(file),bonus_failure(file),missed_bonus(file))
  data.frame(file$Session[1],file$Pseudo[1],destroyed_fortress(file),flight_bad_event(file),shot_bonus(file),point_bonus(file),bonus_failure(file),missed_bonus(file))
}
colnames(d)=c("Session","Pseudo","DestroyedFortress","FlightBadEvents","ShotBonus","PointBonus","FailedBonus","MissedBonus")
d$Pseudo[d$Pseudo=="SL2804"]="SL0804"
d=subset(d,Pseudo!="LM2411"&Pseudo!="EC1603"&Pseudo!="TB0301"&Pseudo!="CP1809"&Pseudo!="MM0301"&Pseudo!="SP0801"&Pseudo!="CH0205")#outliers on long format
data_long=merge(d,data_long,by=c("Session","Pseudo"))


###PLOTS

data_long = data_long%>%
  filter(Group!="STIMSD")
#FORTRESS
p_destroyed_fortress = ggplot(filter(data_long_detailed,Session=="D01P1"|Session=="D05P2"|Session=="D14P2"),aes(Group,DestroyedFortress,color=Group))+theme_pubr()+
  stat_compare_means(method = "anova")+
  scale_colour_manual(values=couleurs)+facet_grid(~Session)+scale_x_discrete(labels=c("SHAM" = "Sham", "STIMHD" = "HD"))+
  stat_summary(geom="point",fun="mean",size=2 ,position=position_dodge(width=0.5))+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=0.5 ,show.legend = FALSE,geom="errorbar",width=0.1 ,position=position_dodge(width=0.5))
p_destroyed_fortress


p_destroyed_fortress=ggplot(filter(data_long,Session=="D01P1"|Session=="D05P2"|Session=="D14P2"),aes(Group,DestroyedFortress,color=Group,fill=Group,shape=Group))+facet_wrap(~Session)+
  theme_pubr()+scale_fill_manual(values=couleurs_alpha)+
  scale_color_manual(values=couleurs)+
  geom_half_violin(width=0.3, position = position_nudge(x=-0.2,y=0))+geom_jitter(width=0.1,alpha=0.6)+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=1 ,show.legend = FALSE,geom="errorbar",width=0.1)+
  stat_summary(fun=mean, geom="point",size=4)+
  ylab("Destroyed Fortress")+rremove("legend")+
  stat_compare_means(method = "anova") 
#ggsave(plot=p_destroyed_fortress,"Poster\\SousScores\\p_destroyed_fortress.pdf",device="pdf",width=10,height=6)

#FLIGHT
p_flight_bad_event = ggplot(filter(data_long,Session=="D01P1"|Session=="D05P2"|Session=="D14P2"),aes(Group,FlightBadEvents,color=Group))+theme_pubr()+
  scale_colour_manual(values=couleurs)+facet_grid(~Session)+scale_x_discrete(labels=c("SHAM" = "Sham", "STIMHD" = "HD"))+
  stat_summary(geom="point",fun="mean",size=2 ,position=position_dodge(width=0.5))+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=0.5 ,show.legend = FALSE,geom="errorbar",width=0.1 ,position=position_dodge(width=0.5))+
  stat_compare_means(method = "anova")

ggplot(filter(data_long,Session=="D01P1"|Session=="D05P2"|Session=="D14P2"),aes(Group,FlightBadEvents,color=Group,fill=Group,shape=Group))+facet_wrap(~Session)+
  theme_pubr()+scale_fill_manual(values=couleurs_alpha)+
  scale_color_manual(values=couleurs)+
  geom_half_violin(width=0.3, position = position_nudge(x=-0.2,y=0))+geom_jitter(width=0.1,alpha=0.6)+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=1 ,show.legend = FALSE,geom="errorbar",width=0.1)+
  stat_summary(fun=mean, geom="point",size=4)+
  ylab("FlightBadEvents")+rremove("legend")+
  stat_compare_means(method = "anova") 
p_flight_bad_event
#ggsave(plot=p_flight_bad_event,"Poster\\SousScores\\p_flight_bad_event.pdf",device="pdf",width=10,height=6)

#BONUS

d_bonus=data_long%>%
  select(Session,Pseudo,ShotBonus,PointBonus,MissedBonus,Group)%>%
  filter(Session=="D01P1"|Session == "D05P2"|Session=="D14P2")%>%
  filter(Group!="STIMSD")%>%
  pivot_longer(cols=c("ShotBonus","PointBonus","MissedBonus"),names_to = "Bonus",values_to = "Nb_Bonus")
d_bonus%>%
  group_by(Bonus,Session)%>%
  summarise(mean=mean(Nb_Bonus))

d_RT=subset(data_wide,select=c("Group","DeltaD14P1D5","DeltaD14P1D5Fortress"))
d_RT=melt(d_RT,id.vars = "Group")
colnames(d_RT)=c("Group","Delta","Value")
d_RT$Delta=as.factor(d_RT$Delta)
p_shot_points_bonus = ggplot(d_bonus,aes(Group,Nb_Bonus,color=Bonus))+theme_pubr()+
  #geom_bar(stat="identity",position = position_dodge())+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=0.5 ,show.legend = FALSE,geom="errorbar",width=0.1 ,position=position_dodge(width=1))+
  stat_summary(geom="point",fun="mean",size=2 ,position=position_dodge(width=1))+
  scale_color_manual(values=couleurs)+facet_grid(~Session)+
  stat_compare_means(method = "anova")

ancova_shot_bonus= d_bonus%>%
  filter(Bonus=="ShotBonus")%>%
  filter(Session=="D14P2")%>%
  anova_test(Nb_Bonus~Group)  
ancova_shot_bonus

p_shot_points_bonus
d_bonus$Bonus=factor(d_bonus$Bonus)
d_bonus$Group=factor(d_bonus$Group)
p_bonus = ggplot(d_bonus,aes(Group,Nb_Bonus,color=Bonus))+theme_pubr()+
scale_color_manual(values=couleurs)+facet_grid(~Session)+scale_x_discrete(labels=c("SHAM" = "Sham", "STIMHD" = "HD"))+
stat_summary(geom="point",fun="mean",size=2 ,position=position_dodge(width=0.5))+
stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=0.5 ,show.legend = FALSE,geom="errorbar",width=0.1 ,position=position_dodge(width=0.5))
p_bonus

couleurs=c("#868686FF","#0073C2FF","#A73030FF")
couleurs_alpha=c("#86868666","#0073C266","#A7303099")
p_bonus = ggplot(d_bonus,aes(Session,Nb_Bonus,color=Group,fill=Group,shape=Bonus))+theme_pubr()+geom_half_violin()+
  geom_half_point(show.legend = T,alpha=0.6)+stat_summary(aes(Session,Nb_Bonus),fun.data = "mean_se", fun.args = list(mult = 1),size=1 ,show.legend = FALSE,geom="errorbar",width=0.1,position = position_dodge(width=0.6))+
  stat_summary(geom="point",fun="mean",size=2,position = position_dodge(width=0.6),color="black",show.legend = F)+
  scale_color_manual(values=couleurs)+
  scale_fill_manual(values=couleurs_alpha)+
  scale_shape_manual(values=c(19,17,12))
p_bonus

#ggsave(plot=p_shot_points_bonus,"Poster\\SousScores\\p_shot_points_bonus.pdf",device="pdf",width=10,height=6)

#MINES
p_time_destroying_mines = ggplot(filter(data_long,Session=="D14P2"|Session=="D01P1"),aes(Group,TimeDestroyingMines,color= Group))+theme_pubr()+
  scale_colour_manual(values=couleurs)+facet_grid(~Session)+scale_x_discrete(labels=c("SHAM" = "Sham", "STIMHD" = "HD"))+
  stat_summary(geom="point",fun="mean",size=2 ,position=position_dodge(width=0.5))+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=0.5 ,show.legend = FALSE,geom="errorbar",width=0.1 ,position=position_dodge(width=0.5))

p_time_destroying_mines
#ggsave(plot=p_time_destroying_mines,"Poster\\SousScores\\p_time_destroying_mines.pdf",device="pdf",width=10,height=6)

#####
#####STATS
#ANOVA on TimeDestroyingMines

data_D1D14 = data_long%>%
  subset(Session=="D01P1"|Session == "D14P2")%>%
  arrange(Session)
data_D1D14$Pseudo = factor(data_D1D14$Pseudo)

data_D1D05 = data_long%>%
  subset(Session=="D01P1"|Session == "D05P2")%>%
  arrange(Session)
data_D1D05$Pseudo = factor(data_D1D14$Pseudo)

data_D5D14 = data_long%>%
  subset(Session=="D05P2"|Session == "D14P2")%>%
  arrange(Session)
data_D5D14$Pseudo = factor(data_D5D14$Pseudo)
anova_Mines=anova_test(data=data_D5D14,TimeDestroyingMines ~Session*Group, wid = Pseudo)

p_anova_Mines=ggplot(data_D1D14,aes(Group,TimeDestroyingMines,color=Group,fill=Group,shape=Group))+facet_wrap(~Session)+
  theme_pubr()+scale_fill_manual(values=couleurs_alpha)+
  scale_color_manual(values=couleurs)+
  geom_half_violin(width=0.3, position = position_nudge(x=-0.2,y=0))+geom_jitter(width=0.1,alpha=0.6)+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=1 ,show.legend = FALSE,geom="errorbar",width=0.1)+
  stat_summary(fun=mean, geom="point",size=4)+
  ylab("TimeDestroyingMines")+rremove("legend")+scale_x_discrete(labels=c("Sham","HD-tRNS"))+
  stat_compare_means(method = "anova") #add pvalue of anova on each
  #stat_pvalue_manual(anova, tip.length = 0, hide.ns = TRUE)+
  # add_pvalue(ph_LR,y.position=c(7500,8700,8100),
  # label = "p = {round(p.adj,3)} {p.adj.signif}", inherit.aes = FALSE)+
  theme(axis.title=element_text(margin=0.1,size=12),text =element_text(size=12) )
p_anova_Mines

#ANOVA on Flight Bad Events
anova_Flight = anova_test(data=data_D5D14,FlightBadEvents ~Session*Group, wid = Pseudo)
anova_Flight
#ANOVA on destroyedFortress
anova_Fortress = anova_test(data=data_D1D14,DestroyedFortress ~Session*Group+GameLevel, wid = Pseudo)
anova_Fortress
#####
d_subscores = data_long%>%
  select(Pseudo,Session,Group,DestroyedFortress,FlightBadEvents,TimeDestroyingMines,ShotBonus,PointBonus,FailedBonus,TotalScore)%>%
  arrange(Pseudo)
  
#write.csv2(d_subscores,"SuppSousScores.csv",row.names = FALSE)

#####
#ANCOVA on linear learning Rate of Fortress Score (lm(fortress~Day))
#ANCOVA LR
ancova_LR_Fortress=data_wide%>%
  anova_test(LRFortress~Group+GameLevelLog) #EFFET DU GROUP p = 0.091

grob_LR_Fortress=grobTree(textGrob(paste0("Group effect: p = ",toString(round(ancova_LR_Fortress$p[1],3)),ancova_LR_Fortress$`p<.05`[1]),
                          x=0.25,y=0.9,hjust=0,vjust=0),
                 text_grob(paste0("Gaming Experience effect: p = ",toString(round(ancova_LR_Fortress$p[2],3))),
                           x=0.25,y=0.85,hjust=0,vjust=0,face="bold")
                 ,gp=gpar(fontsize=10))


p_ancovaLR_Fortress=ggplot(data_wide,aes(Group,LRFortress,color=Group,fill=Group,shape=Group))+
  theme_pubr()+scale_fill_manual(values=couleurs_alpha)+
  scale_color_manual(values=couleurs)+
  geom_half_violin(width=0.3, position = position_nudge(x=-0.2,y=0))+geom_jitter(width=0.1,alpha=0.6)+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=1 ,show.legend = FALSE,geom="errorbar",width=0.1)+
  stat_summary(fun=mean, geom="point",size=4)+
  ylab("Learning Rate Fortress")+rremove("legend")+scale_x_discrete(labels=c("Sham","SD-tRNS","HD-tRNS"))+
  scale_y_continuous( breaks=seq(-4000,8000,1000))+
  annotation_custom(grob_LR_Fortress)+
  theme(axis.title=element_text(margin=0.1,size=12),text =element_text(size=12) )
p_ancovaLR_Fortress  


#####
#DELTA ON TOTAL SCORE
###DELTA S11-S9 (Retention) on Total Score

ancova_S11S9_Total=data_wide%>%
  anova_test(DeltaD14D5~Group+GameLevelLog) #EFFET DU GROUP p = 0.018
ph_S11S9_Total=data_wide%>%
  emmeans_test(DeltaD14D5~Group,covariate = GameLevelLog,p.adjust.method = "holm")

grob_S11S9_Total=grobTree(text_grob(paste0("Group effect: p = ",toString(round(ancova_S11S9_Total$p[1],3))),
                                       x=0.1,y=0.95,hjust=0,vjust=0,face="bold"),
                             text_grob(paste0("\nGaming Experience effect: p = ",toString(round(ancova_S11S9_Total$p[2],3))),
                                       x=0.1,y=0.87,hjust=0,vjust=0),
                             gp=gpar(fontsize=10))

ph_S11S9_Total$p.adj.signif[ph_S11S9_Total$p.adj.signif=="ns"]=""
text_S11S9=paste0("Group effect: p = ",toString(round(ancova_S11S9_Total$p[1],3)),ancova_S11S9_Total$`p<.05`[1],
                  "\nGaming Experience effect: p = ",toString(round(ancova_S11S9_Total$p[2],3)),ancova_S11S9_Total$`p<.05`[2])

p_S11S9_Total=ggplot(data_wide,aes(Group,DeltaD14D5,color=Group,fill=Group,shape=Group))+
  theme_pubr()+scale_fill_manual(values=couleurs_alpha)+
  scale_color_manual(values=couleurs)+
  geom_half_violin(width=0.3, position = position_nudge(x=-0.2,y=0))+geom_jitter(width=0.1,alpha=0.6)+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=0.5,color="black" ,show.legend = FALSE,geom="errorbar",width=0.1)+
  stat_summary(fun=mean, geom="point",size=2,color="black")+
  ylab("Delta Performance")+
  rremove("legend")+scale_x_discrete(labels=c("Sham","SD-tRNS","HD-tRNS"))+
  scale_y_continuous( breaks=seq(-4000,8000,2000),limits=c(-4000,12000))+
  add_pvalue(ph_S11S9_Total[1,],y.position=6500,
             label = "p = {round(p.adj,3)} ", inherit.aes = FALSE,label.size=3)+
  add_pvalue(ph_S11S9_Total[2,],y.position=8500,
             label = "p = {round(p.adj,3)}", inherit.aes = FALSE,label.size=3,fontface="bold")+
  add_pvalue(ph_S11S9_Total[3,],y.position=7500,
             label = "p = {round(p.adj,3)}", inherit.aes = FALSE,label.size=3,fontface="bold")+
  annotation_custom(grob_S11S9_Total)+ggtitle("Total Retention")+
   theme(axis.title=element_text(size=12,margin=0.1),text =element_text(size=12),plot.title = element_text(hjust = 0.5))
  

###DELTA S11-S1 (long term) on Total Score
ancova_S11S1_Total=data_wide%>%
  anova_test(DeltaD1D14~Group+GameLevelLog) #EFFET DU GROUP p = 0.018
ph_S11S1_Total=data_wide%>%
  emmeans_test(DeltaD1D14~Group,covariate = GameLevelLog,p.adjust.method = "holm")

grob_S11S1_Total=grobTree(text_grob(paste0("Group effect: p = ",toString(round(ancova_S11S1_Total$p[1],3))),
                                       x=0.1,y=0.95,hjust=0,vjust=0,face="bold"),
                             text_grob(paste0("\nGaming Experience effect: p = ",toString(round(ancova_S11S1_Total$p[2],3))),
                                       x=0.1,y=0.87,hjust=0,vjust=0),
                             gp=gpar(fontsize=10))

ph_S11S1_Total$p.adj.signif[ph_S11S1_Total$p.adj.signif=="ns"]=""
text_S11S1=paste0("Group effect: p = ",toString(round(ancova_S11S1_Total$p[1],3)),ancova_S11S1_Total$`p<.05`[1],
                  "\nGaming Experience effect: p = ",toString(round(ancova_S11S1_Total$p[2],3)),ancova_S11S1_Total$`p<.05`[2])

p_S11S1_Total=ggplot(data_wide,aes(Group,DeltaD1D14,color=Group,fill=Group,shape=Group))+
  theme_pubr()+scale_fill_manual(values=couleurs_alpha)+
  scale_color_manual(values=couleurs)+
  geom_half_violin(width=0.3, position = position_nudge(x=-0.2,y=0))+geom_jitter(width=0.1,alpha=0.6)+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=0.5,color="black" ,show.legend = FALSE,geom="errorbar",width=0.1)+
  stat_summary(fun=mean, geom="point",size=2,color="black")+
  ylab("Delta Performance")+
  rremove("legend")+scale_x_discrete(labels=c("Sham","SD-tRNS","HD-tRNS"))+
  scale_y_continuous( breaks=seq(0,22000,5000),limits=c(0,24000))+
  add_pvalue(ph_S11S1_Total[1,],y.position=17000,
             label = "p = {round(p.adj,3)} ", inherit.aes = FALSE,label.size=3)+
  add_pvalue(ph_S11S1_Total[2,],y.position=20000,
             label = "p = {round(p.adj,3)}", inherit.aes = FALSE,label.size=3,fontface="bold")+
  add_pvalue(ph_S11S1_Total[3,],y.position=18500,
             label = "p = {round(p.adj,3)}", inherit.aes = FALSE,label.size=3,fontface="bold")+
  annotation_custom(grob_S11S1_Total)+ggtitle("Total Long Term")+
   theme(axis.title=element_text(size=12,margin=0.1),text =element_text(size=12),plot.title = element_text(hjust = 0.5))


###DELTA S9-S1 (short term) on Total Score

ancova_S9S1_Total=data_wide%>%
  anova_test(DeltaD1D5~Group+GameLevelLog) #EFFET DU GROUP p = 0.018
ph_S9S1_Total=data_wide%>%
  emmeans_test(DeltaD1D5~Group,covariate = GameLevelLog,p.adjust.method = "holm")

grob_S9S1_Total=grobTree(text_grob(paste0("Group effect: p = ",toString(round(ancova_S9S1_Total$p[1],3))),
                                      x=0.1,y=0.95,hjust=0,vjust=0),
                            text_grob(paste0("\nGaming Experience effect: p = ",toString(round(ancova_S9S1_Total$p[2],3))),
                                      x=0.1,y=0.87,hjust=0,vjust=0),
                            gp=gpar(fontsize=10))

ph_S9S1_Total$p.adj.signif[ph_S9S1_Total$p.adj.signif=="ns"]=""
text_S9S1=paste0("Group effect: p = ",toString(round(ancova_S9S1_Total$p[1],3)),ancova_S9S1_Total$`p<.05`[1],
                 "\nGaming Experience effect: p = ",toString(round(ancova_S9S1_Total$p[2],3)),ancova_S9S1_Total$`p<.05`[2])

p_S9S1_Total=ggplot(data_wide,aes(Group,DeltaD1D5,color=Group,fill=Group,shape=Group))+
  theme_pubr()+scale_fill_manual(values=couleurs_alpha)+
  scale_color_manual(values=couleurs)+
  geom_half_violin(width=0.3, position = position_nudge(x=-0.2,y=0))+geom_jitter(width=0.1,alpha=0.6)+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=0.5,color="black" ,show.legend = FALSE,geom="errorbar",width=0.1)+
  stat_summary(fun=mean, geom="point",size=2,color="black")+
  ylab("Delta Performance")+
  rremove("legend")+scale_x_discrete(labels=c("Sham","SD-tRNS","HD-tRNS"))+
  scale_y_continuous( breaks=seq(0,22000,5000),limits=c(0,24000))+
  add_pvalue(ph_S9S1_Total[1,],y.position=17000,
             label = "p = {round(p.adj,3)} ", inherit.aes = FALSE,label.size=3)+
  add_pvalue(ph_S9S1_Total[2,],y.position=20000,
             label = "p = {round(p.adj,3)}", inherit.aes = FALSE,label.size=3,fontface="bold")+
  add_pvalue(ph_S9S1_Total[3,],y.position=18500,
             label = "p = {round(p.adj,3)}", inherit.aes = FALSE,label.size=3,fontface="bold")+
  annotation_custom(grob_S9S1_Total)+ggtitle("Total Short Term")+
   theme(axis.title=element_text(size=12,margin=0.1),text =element_text(size=12),plot.title = element_text(hjust = 0.5))

#####
#DELTA RETENTION SOUS SCORES
###DELTA S11-S19 (retention) on Fortress Score

ancova_S11S9_Fortress=data_wide%>%
  anova_test(DeltaD14D5Fortress~Group+GameLevelLog) #EFFET DU GROUP p = 0.018
ph_S11S9_Fortress=data_wide%>%
  emmeans_test(DeltaD14D5Fortress~Group,covariate = GameLevelLog,p.adjust.method = "holm")

grob_S11S9_Fortress=grobTree(text_grob(paste0("Group effect: p = ",toString(round(ancova_S11S9_Fortress$p[1],3))),
                              x=0.1,y=0.95,hjust=0,vjust=0,face="bold"),
                    text_grob(paste0("\nGaming Experience effect: p = ",toString(round(ancova_S11S9_Fortress$p[2],3))),
                              x=0.1,y=0.88,hjust=0,vjust=0),
                    gp=gpar(fontsize=10))

ph_S11S9_Fortress$p.adj.signif[ph_S11S9_Fortress$p.adj.signif=="ns"]=""
text_S11S9=paste0("Group effect: p = ",toString(round(ancova_S11S9_Fortress$p[1],3)),ancova_S11S9_Fortress$`p<.05`[1],
                  "\nGaming Experience effect: p = ",toString(round(ancova_S11S9_Fortress$p[2],3)),ancova_S11S9_Fortress$`p<.05`[2])

p_S11S9_Fortress=ggplot(data_wide,aes(Group,DeltaD14D5Fortress,color=Group,fill=Group,shape=Group))+
  theme_pubr()+scale_fill_manual(values=couleurs_alpha)+
  scale_color_manual(values=couleurs)+
  geom_half_violin(width=0.3, position = position_nudge(x=-0.2,y=0))+geom_jitter(width=0.1,alpha=0.6)+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=0.5,color="black" ,show.legend = FALSE,geom="errorbar",width=0.1)+
  stat_summary(fun=mean, geom="point",size=2,color="black")+
  ylab("Delta Performance")+
  rremove("legend")+scale_x_discrete(labels=c("Sham","SD-tRNS","HD-tRNS"))+
  scale_y_continuous( breaks=seq(-3000,8000,2000),limits=c(-3000,11000))+
  add_pvalue(ph_S11S9_Fortress[1,],y.position=6500,
             label = "p = {round(p.adj,3)} ", inherit.aes = FALSE,label.size=3)+
  add_pvalue(ph_S11S9_Fortress[2,],y.position=8500,
             label = "p = {round(p.adj,3)}", inherit.aes = FALSE,label.size=3,fontface="bold")+
  add_pvalue(ph_S11S9_Fortress[3,],y.position=7500,
             label = "p = {round(p.adj,3)}", inherit.aes = FALSE,label.size=3,fontface="bold")+
  annotation_custom(grob_S11S9_Fortress)+ggtitle("Fortress Retention")+
   theme(axis.title=element_text(size=12,margin=0.1),text =element_text(size=12),plot.title = element_text(hjust = 0.5))

###DELTA S11-S19 (retention) on Flight Score

ancova_S11S9_Flight=data_wide%>%
  anova_test(DeltaD14D5Flight~Group+GameLevelLog) #EFFET DU GROUP p = 0.018
ph_S11S9_Flight=data_wide%>%
  emmeans_test(DeltaD14D5Flight~Group,covariate = GameLevelLog,p.adjust.method = "holm")

grob_S11S9_Flight=grobTree(text_grob(paste0("Group effect: p = ",toString(round(ancova_S11S9_Flight$p[1],3))),
                                       x=0.1,y=0.95,hjust=0,vjust=0),
                             text_grob(paste0("\nGaming Experience effect: p = ",toString(round(ancova_S11S9_Flight$p[2],3))),
                                       x=0.1,y=0.88,hjust=0,vjust=0),
                             gp=gpar(fontsize=10))

ph_S11S9_Flight$p.adj.signif[ph_S11S9_Flight$p.adj.signif=="ns"]=""
text_S11S9=paste0("Group effect: p = ",toString(round(ancova_S11S9_Flight$p[1],3)),ancova_S11S9_Flight$`p<.05`[1],
                  "\nGaming Experience effect: p = ",toString(round(ancova_S11S9_Flight$p[2],3)),ancova_S11S9_Flight$`p<.05`[2])

p_S11S9_Flight=ggplot(data_wide,aes(Group,DeltaD14D5Flight,color=Group,fill=Group,shape=Group))+
  theme_pubr()+scale_fill_manual(values=couleurs_alpha)+
  scale_color_manual(values=couleurs)+
  geom_half_violin(width=0.3, position = position_nudge(x=-0.2,y=0))+geom_jitter(width=0.1,alpha=0.6)+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=0.5,color="black" ,show.legend = FALSE,geom="errorbar",width=0.1)+
  stat_summary(fun=mean, geom="point",size=2,color="black")+
  ylab("Delta Performance")+
  rremove("legend")+scale_x_discrete(labels=c("Sham","SD-tRNS","HD-tRNS"))+
  scale_y_continuous( breaks=seq(-2000,2000,1000),limits=c(-2000,3500))+
  #add_pvalue(ph_S11S9_Flight[1,],y.position=1500,
             #label = "p = {round(p.adj,3)} ", inherit.aes = FALSE,label.size=3)+
  #add_pvalue(ph_S11S9_Flight[2,],y.position=2500,
             #label = "p = {round(p.adj,3)}", inherit.aes = FALSE,label.size=3,fontface="bold")+
  #add_pvalue(ph_S11S9_Flight[3,],y.position=2000,
             #label = "p = {round(p.adj,3)}", inherit.aes = FALSE,label.size=3,fontface="bold")+
  annotation_custom(grob_S11S9_Flight)+ggtitle("Flight Retention")+
   theme(axis.title=element_text(size=12,margin=0.1),text =element_text(size=12),plot.title = element_text(hjust = 0.5))

###DELTA S11-S19 (retention) on Bonus Score

ancova_S11S9_Bonus=data_wide%>%
  anova_test(DeltaD14D5Bonus~Group+GameLevelLog) #EFFET DU GROUP p = 0.018
ph_S11S9_Bonus=data_wide%>%
  emmeans_test(DeltaD14D5Bonus~Group,covariate = GameLevelLog,p.adjust.method = "holm")

grob_S11S9_Bonus=grobTree(text_grob(paste0("Group effect: p = ",toString(round(ancova_S11S9_Bonus$p[1],3))),
                                       x=0.1,y=0.95,hjust=0,vjust=0),
                             text_grob(paste0("\nGaming Experience effect: p = ",toString(round(ancova_S11S9_Bonus$p[2],3))),
                                       x=0.1,y=0.88,hjust=0,vjust=0),
                             gp=gpar(fontsize=10))

ph_S11S9_Bonus$p.adj.signif[ph_S11S9_Bonus$p.adj.signif=="ns"]=""
text_S11S9=paste0("Group effect: p = ",toString(round(ancova_S11S9_Bonus$p[1],3)),ancova_S11S9_Bonus$`p<.05`[1],
                  "\nGaming Experience effect: p = ",toString(round(ancova_S11S9_Bonus$p[2],3)),ancova_S11S9_Bonus$`p<.05`[2])


p_S11S9_Bonus=ggplot(data_wide,aes(Group,DeltaD14D5Bonus,color=Group,fill=Group,shape=Group))+
  theme_pubr()+scale_fill_manual(values=couleurs_alpha)+
  scale_color_manual(values=couleurs)+
  geom_half_violin(width=0.3, position = position_nudge(x=-0.2,y=0))+geom_jitter(width=0.1,alpha=0.6)+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=0.5,color="black" ,show.legend = FALSE,geom="errorbar",width=0.1)+
  stat_summary(fun=mean, geom="point",size=2,color="black")+
  ylab("Delta Performance")+
  rremove("legend")+scale_x_discrete(labels=c("Sham","SD-tRNS","HD-tRNS"))+
  scale_y_continuous( breaks=seq(-2000,3000,1000),limits=c(-2000,4500))+
  #add_pvalue(ph_S11S9_Bonus[1,],y.position=2500,
             #label = "p = {round(p.adj,3)} ", inherit.aes = FALSE,label.size=3)+
  #add_pvalue(ph_S11S9_Bonus[2,],y.position=3500,
             #label = "p = {round(p.adj,3)}", inherit.aes = FALSE,label.size=3,fontface="bold")+
  #add_pvalue(ph_S11S9_Bonus[3,],y.position=3000,
             #label = "p = {round(p.adj,3)}", inherit.aes = FALSE,label.size=3,fontface="bold")+
  annotation_custom(grob_S11S9_Bonus)+ggtitle("Bonus Retention")+
   theme(axis.title=element_text(size=12,margin=0.1),text =element_text(size=12),plot.title = element_text(hjust = 0.5))

###DELTA S11-S19 (retention) on Mine Score

ancova_S11S9_Mine=data_wide%>%
  anova_test(DeltaD14D5Mine~Group+GameLevelLog) #EFFET DU GROUP p = 0.018
ph_S11S9_Mine=data_wide%>%
  emmeans_test(DeltaD14D5Mine~Group,covariate = GameLevelLog,p.adjust.method = "holm")

grob_S11S9_Mine=grobTree(text_grob(paste0("Group effect: p = ",toString(round(ancova_S11S9_Mine$p[1],3))),
                                       x=0.1,y=0.95,hjust=0,vjust=0),
                             text_grob(paste0("\nGaming Experience effect: p = ",toString(round(ancova_S11S9_Mine$p[2],3))),
                                       x=0.1,y=0.88,hjust=0,vjust=0),
                             gp=gpar(fontsize=10))

ph_S11S9_Mine$p.adj.signif[ph_S11S9_Mine$p.adj.signif=="ns"]=""
text_S11S9=paste0("Group effect: p = ",toString(round(ancova_S11S9_Mine$p[1],3)),ancova_S11S9_Mine$`p<.05`[1],
                  "\nGaming Experience effect: p = ",toString(round(ancova_S11S9_Mine$p[2],3)),ancova_S11S9_Mine$`p<.05`[2])

p_S11S9_Mine=ggplot(data_wide,aes(Group,DeltaD14D5Mine,color=Group,fill=Group,shape=Group))+
  theme_pubr()+scale_fill_manual(values=couleurs_alpha)+
  scale_color_manual(values=couleurs)+
  geom_half_violin(width=0.3, position = position_nudge(x=-0.2,y=0))+geom_jitter(width=0.1,alpha=0.6)+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=0.5,color="black" ,show.legend = FALSE,geom="errorbar",width=0.1)+
  stat_summary(fun=mean, geom="point",size=2,color="black")+
  ylab("Delta Performance")+
  rremove("legend")+scale_x_discrete(labels=c("Sham","SD-tRNS","HD-tRNS"))+
  scale_y_continuous( breaks=seq(-2000,2000,1000),limits=c(-2000,3500))+
  #add_pvalue(ph_S11S9_Mine[1,],y.position=1500,
             #label = "p = {round(p.adj,3)} ", inherit.aes = FALSE,label.size=3)+
  #add_pvalue(ph_S11S9_Mine[2,],y.position=2500,
             #label = "p = {round(p.adj,3)}", inherit.aes = FALSE,label.size=3,fontface="bold")+
  #add_pvalue(ph_S11S9_Mine[3,],y.position=2000,
             #label = "p = {round(p.adj,3)}", inherit.aes = FALSE,label.size=3,fontface="bold")+
  annotation_custom(grob_S11S9_Mine)+ggtitle("Mine Retention")+
   theme(axis.title=element_text(size=12,margin=0.1),text =element_text(size=12),plot.title = element_text(hjust = 0.5))

#####
#DELTA LONG TERM SOUS SCORES
###DELTA S11-S1 (long term) on Fortress Score
ancova_S11S1_Fortress=data_wide%>%
  anova_test(DeltaD1D14Fortress~Group+GameLevelLog) #EFFET DU GROUP p = 0.018
ph_S11S1_Fortress=data_wide%>%
  emmeans_test(DeltaD1D14Fortress~Group,covariate = GameLevelLog,p.adjust.method = "holm")

grob_S11S1_Fortress=grobTree(text_grob(paste0("Group effect: p = ",toString(round(ancova_S11S1_Fortress$p[1],3))),
                                       x=0.1,y=0.95,hjust=0,vjust=0,face="bold"),
                             text_grob(paste0("\nGaming Experience effect: p = ",toString(round(ancova_S11S1_Fortress$p[2],3))),
                                       x=0.1,y=0.88,hjust=0,vjust=0),
                             gp=gpar(fontsize=10))

ph_S11S1_Fortress$p.adj.signif[ph_S11S1_Fortress$p.adj.signif=="ns"]=""
text_S11S1=paste0("Group effect: p = ",toString(round(ancova_S11S1_Fortress$p[1],3)),ancova_S11S1_Fortress$`p<.05`[1],
                  "\nGaming Experience effect: p = ",toString(round(ancova_S11S1_Fortress$p[2],3)),ancova_S11S1_Fortress$`p<.05`[2])

p_S11S1_Fortress=ggplot(data_wide,aes(Group,DeltaD1D14Fortress,color=Group,fill=Group,shape=Group))+
  theme_pubr()+scale_fill_manual(values=couleurs_alpha)+
  scale_color_manual(values=couleurs)+
  geom_half_violin(width=0.3, position = position_nudge(x=-0.2,y=0))+geom_jitter(width=0.1,alpha=0.6)+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=0.5,color="black" ,show.legend = FALSE,geom="errorbar",width=0.1)+
  stat_summary(fun=mean, geom="point",size=2,color="black")+
  ylab("Delta Performance")+
  rremove("legend")+scale_x_discrete(labels=c("Sham","SD-tRNS","HD-tRNS"))+
  scale_y_continuous( breaks=seq(0,20000,5000),limits=c(0,20000))+
  add_pvalue(ph_S11S1_Fortress[1,],y.position=14000,
             label = "p = {round(p.adj,3)} ", inherit.aes = FALSE,label.size=3)+
  add_pvalue(ph_S11S1_Fortress[2,],y.position=17000,
             label = "p = {round(p.adj,3)}", inherit.aes = FALSE,label.size=3,fontface="bold")+
  add_pvalue(ph_S11S1_Fortress[3,],y.position=15500,
             label = "p = {round(p.adj,3)}", inherit.aes = FALSE,label.size=3,fontface="bold")+
  annotation_custom(grob_S11S1_Fortress)+ggtitle("Fortress Long Term")+
   theme(axis.title=element_text(size=12,margin=0.1),text =element_text(size=12),plot.title = element_text(hjust = 0.5))


###DELTA S11-S1 (long term) on Flight Score

ancova_S11S1_Flight=data_wide%>%
  anova_test(DeltaD1D14Flight~Group+GameLevelLog) #EFFET DU GROUP p = 0.018
ph_S11S1_Flight=data_wide%>%
  emmeans_test(DeltaD1D14Flight~Group,covariate = GameLevelLog,p.adjust.method = "holm")

grob_S11S1_Flight=grobTree(text_grob(paste0("Group effect: p = ",toString(round(ancova_S11S1_Flight$p[1],3))),
                                     x=0.1,y=0.95,hjust=0,vjust=0),
                           text_grob(paste0("\nGaming Experience effect: p = ",toString(round(ancova_S11S1_Flight$p[2],3))),
                                     x=0.1,y=0.88,hjust=0,vjust=0),
                           gp=gpar(fontsize=10))

ph_S11S1_Flight$p.adj.signif[ph_S11S1_Flight$p.adj.signif=="ns"]=""
text_S11S1=paste0("Group effect: p = ",toString(round(ancova_S11S1_Flight$p[1],3)),ancova_S11S1_Flight$`p<.05`[1],
                  "\nGaming Experience effect: p = ",toString(round(ancova_S11S1_Flight$p[2],3)),ancova_S11S1_Flight$`p<.05`[2])

p_S11S1_Flight=ggplot(data_wide,aes(Group,DeltaD1D14Flight,color=Group,fill=Group,shape=Group))+
  theme_pubr()+scale_fill_manual(values=couleurs_alpha)+
  scale_color_manual(values=couleurs)+
  geom_half_violin(width=0.3, position = position_nudge(x=-0.2,y=0))+geom_jitter(width=0.1,alpha=0.6)+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=0.5,color="black" ,show.legend = FALSE,geom="errorbar",width=0.1)+
  stat_summary(fun=mean, geom="point",size=2,color="black")+
  ylab("Delta Performance")+
  rremove("legend")+scale_x_discrete(labels=c("Sham","SD-tRNS","HD-tRNS"))+
  scale_y_continuous( breaks=seq(-2000,6000,2000),limits=c(-2000,8000))+
  #add_pvalue(ph_S11S1_Flight[1,],y.position=4500,
             #label = "p = {round(p.adj,3)} ", inherit.aes = FALSE,label.size=3)+
  #add_pvalue(ph_S11S1_Flight[2,],y.position=6500,
             #label = "p = {round(p.adj,3)}", inherit.aes = FALSE,label.size=3,fontface="bold")+
  #add_pvalue(ph_S11S1_Flight[3,],y.position=5500,
             #label = "p = {round(p.adj,3)}", inherit.aes = FALSE,label.size=3,fontface="bold")+
  annotation_custom(grob_S11S1_Flight)+ggtitle("Flight Long Term")+
   theme(axis.title=element_text(size=12,margin=0.1),text =element_text(size=12),plot.title = element_text(hjust = 0.5))


###DELTA S11-S1 (long term) on Bonus Score

ancova_S11S1_Bonus=data_wide%>%
  anova_test(DeltaD1D14Bonus~Group+GameLevelLog) #EFFET DU GROUP p = 0.018
ph_S11S1_Bonus=data_wide%>%
  emmeans_test(DeltaD1D14Bonus~Group,covariate = GameLevelLog,p.adjust.method = "holm")

grob_S11S1_Bonus=grobTree(text_grob(paste0("Group effect: p = ",toString(round(ancova_S11S1_Bonus$p[1],3))),
                                    x=0.1,y=0.95,hjust=0,vjust=0,face="bold"),
                          text_grob(paste0("\nGaming Experience effect: p = ",toString(round(ancova_S11S1_Bonus$p[2],3))),
                                    x=0.1,y=0.88,hjust=0,vjust=0),
                          gp=gpar(fontsize=10))

ph_S11S1_Bonus$p.adj.signif[ph_S11S1_Bonus$p.adj.signif=="ns"]=""
text_S11S1=paste0("Group effect: p = ",toString(round(ancova_S11S1_Bonus$p[1],3)),ancova_S11S1_Bonus$`p<.05`[1],
                  "\nGaming Experience effect: p = ",toString(round(ancova_S11S1_Bonus$p[2],3)),ancova_S11S1_Bonus$`p<.05`[2])

p_S11S1_Bonus=ggplot(data_wide,aes(Group,DeltaD1D14Bonus,color=Group,fill=Group,shape=Group))+
  theme_pubr()+scale_fill_manual(values=couleurs_alpha)+
  scale_color_manual(values=couleurs)+
  geom_half_violin(width=0.3, position = position_nudge(x=-0.2,y=0))+geom_jitter(width=0.1,alpha=0.6)+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=0.5,color="black" ,show.legend = FALSE,geom="errorbar",width=0.1)+
  stat_summary(fun=mean, geom="point",size=2,color="black")+
  ylab("Delta Performance")+
  rremove("legend")+scale_x_discrete(labels=c("Sham","SD-tRNS","HD-tRNS"))+
  scale_y_continuous( breaks=seq(-2000,6000,2000),limits=c(-2000,8000))+
  add_pvalue(ph_S11S1_Bonus[1,],y.position=4500,
             label = "p = {round(p.adj,3)} ", inherit.aes = FALSE,label.size=3)+
  add_pvalue(ph_S11S1_Bonus[2,],y.position=6500,
             label = "p = {round(p.adj,3)}", inherit.aes = FALSE,label.size=3,fontface="bold")+
  add_pvalue(ph_S11S1_Bonus[3,],y.position=5500,
             label = "p = {round(p.adj,3)}", inherit.aes = FALSE,label.size=3,fontface="bold")+
  annotation_custom(grob_S11S1_Bonus)+ggtitle("Bonus Long Term")+
   theme(axis.title=element_text(size=12,margin=0.1),text =element_text(size=12),plot.title = element_text(hjust = 0.5))

###DELTA S11-S1 (long term) on Mine Score

ancova_S11S1_Mine=data_wide%>%
  anova_test(DeltaD1D14Mine~Group+GameLevelLog) #EFFET DU GROUP p = 0.018
ph_S11S1_Mine=data_wide%>%
  emmeans_test(DeltaD1D14Mine~Group,covariate = GameLevelLog,p.adjust.method = "holm")

grob_S11S1_Mine=grobTree(text_grob(paste0("Group effect: p = ",toString(round(ancova_S11S1_Mine$p[1],3))),
                                   x=0.1,y=0.95,hjust=0,vjust=0),
                         text_grob(paste0("\nGaming Experience effect: p = ",toString(round(ancova_S11S1_Mine$p[2],3))),
                                   x=0.1,y=0.88,hjust=0,vjust=0),
                         gp=gpar(fontsize=10))

ph_S11S1_Mine$p.adj.signif[ph_S11S1_Mine$p.adj.signif=="ns"]=""
text_S11S1=paste0("Group effect: p = ",toString(round(ancova_S11S1_Mine$p[1],3)),ancova_S11S1_Mine$`p<.05`[1],
                  "\nGaming Experience effect: p = ",toString(round(ancova_S11S1_Mine$p[2],3)),ancova_S11S1_Mine$`p<.05`[2])

p_S11S1_Mine=ggplot(data_wide,aes(Group,DeltaD1D14Mine,color=Group,fill=Group,shape=Group))+
  theme_pubr()+scale_fill_manual(values=couleurs_alpha)+
  scale_color_manual(values=couleurs)+
  geom_half_violin(width=0.3, position = position_nudge(x=-0.2,y=0))+geom_jitter(width=0.1,alpha=0.6)+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=0.5,color="black" ,show.legend = FALSE,geom="errorbar",width=0.1)+
  stat_summary(fun=mean, geom="point",size=2,color="black")+
  ylab("Delta Performance")+
  rremove("legend")+scale_x_discrete(labels=c("Sham","SD-tRNS","HD-tRNS"))+
  scale_y_continuous( breaks=seq(-2000,6000,2000),limits=c(-2000,8000))+
  # add_pvalue(ph_S11S1_Mine[1,],y.position=4500,
             #label = "p = {round(p.adj,3)} ", inherit.aes = FALSE,label.size=3)+
 # add_pvalue(ph_S11S1_Mine[2,],y.position=6500,
             #label = "p = {round(p.adj,3)}", inherit.aes = FALSE,label.size=3,fontface="bold")+
  #add_pvalue(ph_S11S1_Mine[3,],y.position=5500,
             #label = "p = {round(p.adj,3)}", inherit.aes = FALSE,label.size=3,fontface="bold")+
  annotation_custom(grob_S11S1_Mine)+ggtitle("Mine Long Term")+
   theme(axis.title=element_text(size=12,margin=0.1),text =element_text(size=12),plot.title = element_text(hjust = 0.5))


#####
#DELTA SHORT TERM SOUS SCORES
###DELTA S9-S1 (short term) on Fortress Score

ancova_S9S1_Fortress=data_wide%>%
  anova_test(DeltaD5D1Fortress~Group+GameLevelLog) #EFFET DU GROUP p = 0.018
ph_S9S1_Fortress=data_wide%>%
  emmeans_test(DeltaD5D1Fortress~Group,covariate = GameLevelLog,p.adjust.method = "holm")

grob_S9S1_Fortress=grobTree(text_grob(paste0("Group effect: p = ",toString(round(ancova_S9S1_Fortress$p[1],3))),
                                       x=0.1,y=0.95,hjust=0,vjust=0),
                             text_grob(paste0("\nGaming Experience effect: p = ",toString(round(ancova_S9S1_Fortress$p[2],3))),
                                       x=0.1,y=0.88,hjust=0,vjust=0),
                             gp=gpar(fontsize=10))

ph_S9S1_Fortress$p.adj.signif[ph_S9S1_Fortress$p.adj.signif=="ns"]=""
text_S9S1=paste0("Group effect: p = ",toString(round(ancova_S9S1_Fortress$p[1],3)),ancova_S9S1_Fortress$`p<.05`[1],
                  "\nGaming Experience effect: p = ",toString(round(ancova_S9S1_Fortress$p[2],3)),ancova_S9S1_Fortress$`p<.05`[2])

p_S9S1_Fortress=ggplot(data_wide,aes(Group,DeltaD5D1Fortress,color=Group,fill=Group,shape=Group))+
  theme_pubr()+scale_fill_manual(values=couleurs_alpha)+
  scale_color_manual(values=couleurs)+
  geom_half_violin(width=0.3, position = position_nudge(x=-0.2,y=0))+geom_jitter(width=0.1,alpha=0.6)+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=0.5,color="black" ,show.legend = FALSE,geom="errorbar",width=0.1)+
  stat_summary(fun=mean, geom="point",size=2,color="black")+
  ylab("Delta Performance")+
  rremove("legend")+scale_x_discrete(labels=c("Sham","SD-tRNS","HD-tRNS"))+
  scale_y_continuous( breaks=seq(0,20000,5000),limits=c(0,20000))+
  add_pvalue(ph_S9S1_Fortress[1,],y.position=14000,
             label = "p = {round(p.adj,3)} ", inherit.aes = FALSE,label.size=3)+
  add_pvalue(ph_S9S1_Fortress[2,],y.position=17000,
             label = "p = {round(p.adj,3)}", inherit.aes = FALSE,label.size=3,fontface="bold")+
  add_pvalue(ph_S9S1_Fortress[3,],y.position=15500,
             label = "p = {round(p.adj,3)}", inherit.aes = FALSE,label.size=3,fontface="bold")+
  annotation_custom(grob_S9S1_Fortress)+ggtitle("Fortress Short Term")+
   theme(axis.title=element_text(size=12,margin=0.1),text =element_text(size=12),plot.title = element_text(hjust = 0.5))


###DELTA S11-S1 (long term) on Flight Score

ancova_S9S1_Flight=data_wide%>%
  anova_test(DeltaD5D1Flight~Group+GameLevelLog) #EFFET DU GROUP p = 0.018
ph_S9S1_Flight=data_wide%>%
  emmeans_test(DeltaD5D1Flight~Group,covariate = GameLevelLog,p.adjust.method = "holm")

grob_S9S1_Flight=grobTree(text_grob(paste0("Group effect: p = ",toString(round(ancova_S9S1_Flight$p[1],3))),
                                     x=0.1,y=0.95,hjust=0,vjust=0),
                           text_grob(paste0("\nGaming Experience effect: p = ",toString(round(ancova_S9S1_Flight$p[2],3))),
                                     x=0.1,y=0.88,hjust=0,vjust=0),
                           gp=gpar(fontsize=10))

ph_S9S1_Flight$p.adj.signif[ph_S9S1_Flight$p.adj.signif=="ns"]=""
text_S9S1=paste0("Group effect: p = ",toString(round(ancova_S9S1_Flight$p[1],3)),ancova_S9S1_Flight$`p<.05`[1],
                  "\nGaming Experience effect: p = ",toString(round(ancova_S9S1_Flight$p[2],3)),ancova_S9S1_Flight$`p<.05`[2])

p_S9S1_Flight=ggplot(data_wide,aes(Group,DeltaD5D1Flight,color=Group,fill=Group,shape=Group))+
  theme_pubr()+scale_fill_manual(values=couleurs_alpha)+
  scale_color_manual(values=couleurs)+
  geom_half_violin(width=0.3, position = position_nudge(x=-0.2,y=0))+geom_jitter(width=0.1,alpha=0.6)+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=0.5,color="black" ,show.legend = FALSE,geom="errorbar",width=0.1)+
  stat_summary(fun=mean, geom="point",size=2,color="black")+
  ylab("Delta Performance ")+
  rremove("legend")+scale_x_discrete(labels=c("Sham","SD-tRNS","HD-tRNS"))+
  scale_y_continuous( breaks=seq(-2000,6000,2000),limits=c(-2000,8000))+
  #add_pvalue(ph_S9S1_Flight[1,],y.position=4500,
            #label = "p = {round(p.adj,3)} ", inherit.aes = FALSE,label.size=3)+
  #add_pvalue(ph_S9S1_Flight[2,],y.position=6500,
             #label = "p = {round(p.adj,3)}", inherit.aes = FALSE,label.size=3,fontface="bold")+
  #add_pvalue(ph_S9S1_Flight[3,],y.position=5500,
             #label = "p = {round(p.adj,3)}", inherit.aes = FALSE,label.size=3,fontface="bold")+
  annotation_custom(grob_S9S1_Flight)+ggtitle("Flight Short Term")+
   theme(axis.title=element_text(size=12,margin=0.1),text =element_text(size=12),plot.title = element_text(hjust = 0.5))

###DELTA S11-S1 (long term) on Bonus Score

ancova_S9S1_Bonus=data_wide%>%
  anova_test(DeltaD5D1Bonus~Group+GameLevelLog) #EFFET DU GROUP p = 0.018
ph_S9S1_Bonus=data_wide%>%
  emmeans_test(DeltaD5D1Bonus~Group,covariate = GameLevelLog,p.adjust.method = "holm")

grob_S9S1_Bonus=grobTree(text_grob(paste0("Group effect: p = ",toString(round(ancova_S9S1_Bonus$p[1],3))),
                                    x=0.1,y=0.95,hjust=0,vjust=0,face="bold"),
                          text_grob(paste0("\nGaming Experience effect: p = ",toString(round(ancova_S9S1_Bonus$p[2],3))),
                                    x=0.1,y=0.88,hjust=0,vjust=0),
                          gp=gpar(fontsize=10))

ph_S9S1_Bonus$p.adj.signif[ph_S9S1_Bonus$p.adj.signif=="ns"]=""
text_S9S1=paste0("Group effect: p = ",toString(round(ancova_S9S1_Bonus$p[1],3)),ancova_S9S1_Bonus$`p<.05`[1],
                  "\nGaming Experience effect: p = ",toString(round(ancova_S9S1_Bonus$p[2],3)),ancova_S9S1_Bonus$`p<.05`[2])

p_S9S1_Bonus=ggplot(data_wide,aes(Group,DeltaD5D1Bonus,color=Group,fill=Group,shape=Group))+
  theme_pubr()+scale_fill_manual(values=couleurs_alpha)+
  scale_color_manual(values=couleurs)+
  geom_half_violin(width=0.3, position = position_nudge(x=-0.2,y=0))+geom_jitter(width=0.1,alpha=0.6)+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=0.5,color="black" ,show.legend = FALSE,geom="errorbar",width=0.1)+
  stat_summary(fun=mean, geom="point",size=2,color="black")+
  ylab("Delta Performance ")+
  rremove("legend")+scale_x_discrete(labels=c("Sham","SD-tRNS","HD-tRNS"))+
  scale_y_continuous( breaks=seq(-2000,6000,2000),limits=c(-2000,8000))+
  add_pvalue(ph_S9S1_Bonus[1,],y.position=4500,
             label = "p = {round(p.adj,3)} ", inherit.aes = FALSE,label.size=3)+
  add_pvalue(ph_S9S1_Bonus[2,],y.position=6500,
             label = "p = {round(p.adj,3)}", inherit.aes = FALSE,label.size=3,fontface="bold")+
  add_pvalue(ph_S9S1_Bonus[3,],y.position=5500,
             label = "p = {round(p.adj,3)}", inherit.aes = FALSE,label.size=3,fontface="bold")+
  annotation_custom(grob_S9S1_Bonus)+ggtitle("Bonus Short Term")+
   theme(axis.title=element_text(size=12,margin=0.1),text =element_text(size=12),plot.title = element_text(hjust = 0.5))

###DELTA S11-S1 (shortterm) on Mine Score

ancova_S9S1_Mine=data_wide%>%
  anova_test(DeltaD5D1Mine~Group+GameLevelLog) #EFFET DU GROUP p = 0.018
ph_S9S1_Mine=data_wide%>%
  emmeans_test(DeltaD5D1Mine~Group,covariate = GameLevelLog,p.adjust.method = "holm")

grob_S9S1_Mine=grobTree(text_grob(paste0("Group effect: p = ",toString(round(ancova_S9S1_Mine$p[1],3))),
                                   x=0.1,y=0.95,hjust=0,vjust=0),
                         text_grob(paste0("\nGaming Experience effect: p = ",toString(round(ancova_S9S1_Mine$p[2],3))),
                                   x=0.1,y=0.88,hjust=0,vjust=0),
                         gp=gpar(fontsize=10))

ph_S9S1_Mine$p.adj.signif[ph_S9S1_Mine$p.adj.signif=="ns"]=""
text_S9S1=paste0("Group effect: p = ",toString(round(ancova_S9S1_Mine$p[1],3)),ancova_S9S1_Mine$`p<.05`[1],
                  "\nGaming Experience effect: p = ",toString(round(ancova_S9S1_Mine$p[2],3)),ancova_S9S1_Mine$`p<.05`[2])

p_S9S1_Mine=ggplot(data_wide,aes(Group,DeltaD5D1Mine,color=Group,fill=Group,shape=Group))+
  theme_pubr()+scale_fill_manual(values=couleurs_alpha)+
  scale_color_manual(values=couleurs)+
  geom_half_violin(width=0.3, position = position_nudge(x=-0.2,y=0))+geom_jitter(width=0.1,alpha=0.6)+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=0.5,color="black" ,show.legend = FALSE,geom="errorbar",width=0.1)+
  stat_summary(fun=mean, geom="point",size=2,color="black")+
  ylab("Delta Performance")+
  rremove("legend")+scale_x_discrete(labels=c("Sham","SD-tRNS","HD-tRNS"))+
  scale_y_continuous( breaks=seq(-2000,6000,2000),limits=c(-2000,8000))+
  #add_pvalue(ph_S9S1_Mine[1,],y.position=4500,
             #label = "p = {round(p.adj,3)} ", inherit.aes = FALSE,label.size=3)+
  #add_pvalue(ph_S9S1_Mine[2,],y.position=6500,
             #label = "p = {round(p.adj,3)}", inherit.aes = FALSE,label.size=3,fontface="bold")+
  #add_pvalue(ph_S9S1_Mine[3,],y.position=5500,
             #label = "p = {round(p.adj,3)}", inherit.aes = FALSE,label.size=3,fontface="bold")+
  annotation_custom(grob_S9S1_Mine)+ggtitle("Mine Short Term")+
  theme(axis.title=element_text(size=12,margin=0.1),text =element_text(size=12),plot.title = element_text(hjust = 0.5))
  
 

#####
#FIGURES
#Figure ST
figure_ST=(plot_spacer() + p_S9S1_Total + plot_spacer() + plot_layout(widths = c(1,2,1)))/( p_S9S1_Fortress|p_S9S1_Flight)/( p_S9S1_Bonus|p_S9S1_Mine)+
  plot_annotation(tag_levels = "A")&theme(plot.tag.position = c(0.01,0.95),plot.tag = element_text(size=8,face = 'bold'))
figure_ST
ggsave(plot=figure_ST,"E:\\SpaceFortress\\Poster\\ModexVisu\\DeltaST_STIMHDSD.pdf",width=10,height=10)

#Figure LT
figure_LT=(plot_spacer() + p_S11S1_Total + plot_spacer() + plot_layout(widths = c(1,2,1)))/( p_S11S1_Fortress|p_S11S1_Flight)/( p_S11S1_Bonus|p_S11S1_Mine)+
  plot_annotation(tag_levels = "A")&theme(plot.tag.position = c(0.01,0.95),plot.tag = element_text(size=8,face = 'bold'))
figure_LT
ggsave(plot=figure_LT,"E:\\SpaceFortress\\Poster\\ModexVisu\\DeltaLT_STIMHDSD.pdf",width=10,height=10)

#Figure RT
figure_RT=(plot_spacer() + p_S11S9_Total + plot_spacer() + plot_layout(widths = c(1,2,1)))/( p_S11S9_Fortress|p_S11S9_Flight)/( p_S11S9_Bonus|p_S11S9_Mine)+
  plot_annotation(tag_levels = "A")&theme(plot.tag.position = c(0.01,0.95),plot.tag = element_text(size=8,face = 'bold'))
figure_RT
ggsave(plot=figure_RT,"E:\\SpaceFortress\\Poster\\ModexVisu\\DeltaRT_STIMHDSD.pdf",width=10,height=10)

