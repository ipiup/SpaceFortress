library(ggplot2)
library(ggpubr)
library(irr)
library(rmgb)
data_sham=subset(data_wide,Group=="SHAM")

cor(data_sham$D01P1,data_sham$D02P1,method="spearman")
cor(data_sham$D05P1,data_sham$D14P1,method="spearman")

plot_J1J2=ggplot(data_sham,aes(D01P1,D02P1))+theme_pubr()+
  xlab("Day 1")+ylab("Day 2")+
  geom_point()+geom_smooth(method=lm,fill="lightgrey",color="black")+
  stat_cor(method="spearman",p.accuracy = 0.001)+theme(axis.title=element_text(face="bold"))

plot_J5J14=ggplot(data_sham,aes(D05P1,D14P1))+theme_pubr()+
  xlab("Day 5")+ylab("Day 14")+
  geom_point()+geom_smooth(method=lm,fill="lightgrey",color="black")+
  stat_cor(method="spearman",p.accuracy = 0.001)+theme(axis.title=element_text(face="bold"))

ggarrange(plot_J1J2,plot_J5J14,nrow=1,labels=c("A","B"))+theme(axis.title=element_text('face="bold'))

#ICC D01P16D02P1
data_sham=select(data_sham,c("D01P1","D02P1"))
icc(data_sham,model="oneway")

#ICC D01P16D02P1
data_sham=select(data_sham,c("D05P1","D14P1"))
icc(data_sham,model="oneway")


#Cause of fatal accident by decade
library(dplyr)
library(tidyr)
library(Hmisc)
data_acc=read.csv("FatalAccidents_Per_Decade.csv",header=TRUE,sep=";",check.names = FALSE,dec=",")

data=data_acc%>%
  pivot_longer(.,cols=c("1950s","1960s","1970s","1980s","1990s","2000s","2010s"),names_to = "Decade") #passage en format long

data$value=100*data$value
data$ERROR=as.factor(data$ERROR)
ggplot(data,aes(Decade,value,fill=ERROR,color=ERROR))+theme_pubr()+
  geom_bar(position="dodge",stat="identity")+fill_palette("jco")+color_palette("jco")+
  labs(fill=" ",color=" ",y="Percentage (%)")

ggplot(data,aes(Decade,value,fill=ERROR,color=ERROR,group=ERROR))+theme_pubr()+
  geom_point()+geom_line(show.legend = FALSE)+
  fill_palette("jco")+color_palette("jco")+
  labs(fill=" ",color=" ",y="Percentage (%)")+theme(legend.position = c(0.9,0.65))

data$ERROR=factor(data$ERROR,levels=c(" Mechanical"," Pilot Error"," Sabotage"," Weather"," Other"))
