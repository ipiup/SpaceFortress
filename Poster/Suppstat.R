### Supp Statistics for Poster
#####DATA & librairies

data_wide = read.csv("E:\\SpaceFortress\\Paper\\CleanData\\data_wide.csv") #here insert the data_long path
data_long = read.csv("E:\\SpaceFortress\\Paper\\CleanData\\data_long.csv") #here insert the data_long path
couleurs=c("#868686FF","#0073C2FF","#A73030FF")
couleurs_alpha=c("#86868666","#0073C266","#A7303099")
data_wide$Group=factor(data_wide$Group,levels=c("SHAM","STIMSD","STIMHD"))
data_long$Group=factor(data_long$Group,levels=c("SHAM","STIMSD","STIMHD"))
data_long$D=1:11

#####
#Nb destroyed fortress
destroyed_fortress<-function(file){return(sum(file$Type=="FortressDestruction"))}
#Flight bad events
flight_bad_event<-function(file){return(sum(file$Type=="BorderCrossing"|file$Type=="FortessCollision"|file$Type=="ShipDamage"))}
#Temp passé à détruire des mines
mine_kill_time <- function(file){
  df_mine=subset(file,Group=="Mine"|Type=="NewMine"|(Type=="ShipDamage" & e1=="collide" & e2 == "mine_0"))
  j=1
  killtime=0#l_rt=c()
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
          killtime=killtime+(t_destrmine - t_newmine)
          #l_rt = append(l_rt,t_destrmine - t_newmine)
          tmp=TRUE
        }else if (df_mine[j,"Type"]=="NewMine"|df_mine[j,"Type"]=="MineExtinction"|df_mine[j,"Type"]=="ShipDamage"){
          tmp=TRUE
        }
      }
    }
  }
  print(killtime)
  return(killtime)
}
#Proportion Bonus
shot_bonus<-function(file){return(sum(file$Type=="ShotsBonusCapture"))}
point_bonus<-function(file){return(sum(file$Type=="PointsBonusCapture"))}
bonus_failure<-function(file){return(sum(file$Type=="BonusFailure"))}


path_clean="E:\\ISAE-2021\\Alldata\\Data_clean\\"
files_data=list.files(path=path_clean,recursive = T) 
d=foreach(i=1:length(files_data),.combine=rbind)%do%{
  print(i)
  file=read.table(paste0(path_clean,"/",files_data[i]), header=TRUE, sep="\t",dec=".",fill=TRUE)
  data.frame(file$Session[1],file$Pseudo[1],destroyed_fortress(file),mine_kill_time(file),flight_bad_event(file),shot_bonus(file),point_bonus(file),bonus_failure(file))
}
colnames(d)=c("Session","Pseudo","DestroyedFortress","TimeDestroyingMines","FlightBadEvents","ShotBonus","PointBonus","FailedBonus")
d$Pseudo[d$Pseudo=="SL2804"]="SL0804"
d=subset(d,Pseudo!="LM2411"&Pseudo!="EC1603"&Pseudo!="TB0301"&Pseudo!="CP1809"&Pseudo!="MM0301"&Pseudo!="SP0801"&Pseudo!="CH0205")#outliers on long format
data_long=merge(d,data_long,by=c("Session","Pseudo"))


###PLOTS

#FORTRESS
p_destroyed_fortress = ggplot(data_long,aes(Group,DestroyedFortress,color=Group))+theme_pubr()+
  scale_colour_manual(values=couleurs)+facet_grid(~Session)+
  stat_summary(geom="point",fun="mean",size=2 ,position=position_dodge(width=0.5))+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=1 ,show.legend = FALSE,geom="errorbar",width=0.1 ,position=position_dodge(width=0.5))
p_destroed_fortress

#FLIGHT
p_flight_bad_event = ggplot(data_long,aes(Group,FlightBadEvents,color=Group))+theme_pubr()+
  scale_colour_manual(values=couleurs)+facet_grid(~Session)+
  stat_summary(geom="point",fun="mean",size=2 ,position=position_dodge(width=0.5))+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=1 ,show.legend = FALSE,geom="errorbar",width=0.1 ,position=position_dodge(width=0.5))
p_flight_bad_event

#BONUS
library(tidyr)
d_bonus=data_long%>%
  select(Session,Pseudo,ShotBonus,PointBonus,Group)%>%
  filter(Session=="D01P1"|Session == "D05P2"|Session=="D14P2")%>%
  pivot_longer(cols=c("ShotBonus","PointBonus"),names_to = "Bonus",values_to = "Nb_Bonus")
d_bonus%>%
  group_by(Bonus,Session)%>%
  summarise(mean=mean(Nb_Bonus))

p_shot_points_bonus = ggplot(d_bonus,aes(Group,Nb_Bonus,fill=Bonus))+theme_pubr()+
  geom_bar(stat="identity",position = position_dodge())+
  scale_fill_manual(values=couleurs)+facet_grid(~Session)
p_shot_points_bonus

#MINES
p_time_destroying_mines = ggplot(filter(data_long,grepl("P2",Session)|Session=="D01P1"),aes(Group,TimeDestroyingMines,color= Group))+theme_pubr()+
  scale_colour_manual(values=couleurs)+facet_grid(~Session)+scale_x_discrete(labels=c("SHAM" = "Sham", "STIMSD" = "SD", "STIMHD" = "HD"))+
  stat_summary(geom="point",fun="mean",size=2 ,position=position_dodge(width=0.5))+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=1 ,show.legend = FALSE,geom="errorbar",width=0.1 ,position=position_dodge(width=0.5))

p_time_destroying_mines
