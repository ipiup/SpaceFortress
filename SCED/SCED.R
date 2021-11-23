#SCED
require(Rcmdr)
require(ggplot2)
require(ggpubr)
require(dplyr)
require(lme4)

path="E:/SpaceFortress/SCED/" #choisir path contenant les fichiers xlsx
fil=list.files(path=paste0(path,"Data/All/"),recursive = T)

SCED_figure<-function(filename){
  plot=plot_sced_AllObj(filename)
  ggsave(paste0(path,"Plots/Plot_",substr(filename,1,3),".pdf"),device="pdf",width=8,height=3)
}

#### plot sced/participant avec toutes les tâches
plot_sced_AllObj<-function(filename){
  data <- readXL(paste0(path,"Data/All/",filename), rownames=FALSE, header=TRUE, sheet=1, stringsAsFactors=TRUE)
  data$Phase=factor(data$Phase,levels=c("A1","B","A2")) #change l'ordre de facteurs
  data=data%>%
    mutate(Dodge = case_when(
      Objective==levels(data$Objective)[1] ~ +0.15,
      Objective==levels(data$Objective)[2] ~ -0.15,
      Objective==levels(data$Objective)[3] ~ 0
    ))
  min_max<-data%>%
    group_by(Phase)%>%
    summarise(min=min(Measure,na.rm = TRUE),max=max(Measure,na.rm = TRUE))
  #plot
  p_sced=ggplot(data,aes(Measure,GAS_score,color=Objective,shape=Objective))+theme_pubr()+
    scale_y_continuous(breaks=-2:2,limits=c(-2.2,3))+scale_x_continuous(breaks=seq(0,24,by=2))+
    geom_point(aes(y=GAS_score+Dodge))+geom_line(show.legend=FALSE,aes(y=GAS_score+Dodge,shape=Phase))+
    geom_vline(xintercept = min_max$max[-3]+0.5)+
    annotate("text",x=min_max$min+0.5,y=3,label=min_max$Phase,fontface="bold",hjust=0)+
    guides(colours=TRUE,shape=FALSE,color=guide_legend(override.aes = list(shape=c(19,17,15))))+
    scale_shape_manual(values=c(1,1,1,19,17,15))+
    labs(color="")+ylab("GAS Score")+xlab("Time Point Measurement")
  p_sced
  return(p_sced)
}

#application sur tous les participants 
sapply(fil,SCED_figure)


#### plot sced/participant & /Tache
plot_sced<-function(filename,sheetnb){
  # data <- readXL(paste0(path,"Data/",filename), rownames=FALSE, header=TRUE,
  #                na="", sheet=sheetnb, stringsAsFactors=TRUE)
  data <- readXL(paste0(path,"Data/All/",filename), rownames=FALSE, header=TRUE, sheet=1, stringsAsFactors=TRUE)
  data$Phase=factor(data$Phase,levels=c("A1","B","A2"))
  min_max<-data%>%
    group_by(Phase)%>%
    summarise(min=min(GAS_score,na.rm = TRUE),max=max(GAS_score,na.rm = TRUE))
  min_max$xmin=c(min(data$Measure[data$Phase=="A1"]),min(data$Measure[data$Phase=="B"]),min(data$Measure[data$Phase=="A2"]))
  min_max$xmax=c(max(data$Measure[data$Phase=="A1"]),max(data$Measure[data$Phase=="B"]),max(data$Measure[data$Phase=="A2"]))
  coef=lmList(Measure~GAS_score|Phase,data=data)
  coef=coef(coef)$GAS_score
  coef[is.na(coef)]=0

  p_sced=ggplot(data,aes(Measure,GAS_score,color=Phase))+theme_pubr()+rremove("legend")+scale_y_continuous(breaks=-2:2,limits=c(-3,3))+
    scale_x_continuous(breaks=seq(0,24,by=2))+
    geom_line(linetype="dotted")+
    geom_point()+geom_smooth(method=lm,se=FALSE)+
    geom_vline(xintercept = min_max$xmax[-3]+0.5)+
    geom_segment(data=min_max,aes(x=xmin,xend=xmax,y =min,yend=min),linetype="dashed")+
    geom_segment(data=min_max,aes(x=xmin,xend=xmax,y =max,yend=max),linetype="dashed")+
    annotate("text",x=min_max$xmin,y=3,label=min_max$Phase,fontface="bold",hjust=0)+
    annotate("text",x= min_max$xmin+0.5,y=-3, label= "Slope = ",hjust=0)+
    annotate("text",x= min_max$xmin+3,y=-3, label= round(coef,2),hjust=0)+
    ylab("GAS Score")+xlab("Time Point Measurement")+ggtitle(data$Objective[1])
  p_sced

  return(p_sced)
}

