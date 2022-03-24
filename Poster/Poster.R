library(ggExtra)

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
  xlab("Score per Minute")+ylab("Action per Minute")+theme_pubr()+facet_grid(~Session)

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
  scale_y_continuous(breaks =seq(0, 15000, by = 2500))+
  theme(legend.position = c(0.815,0.15),legend.background = element_rect(fill=NA),legend.title = element_blank(),
        axis.title = element_text(size=18,margin=0.1),legend.text = element_text(size=16),text=element_text(size=16))+
  annotate("text",x=c(1,2.5,4.5,6.5,8.5,10.5),y=Inf,vjust=1.5,label=c("Jour 1","Jour 2","Jour 3","Jour 4","Jour 5","Jour 15"),size=5)

ggsave(plot=p_APM,"Poster\\APM_SHAM_STIMHD_allSession.pdf",device="pdf",width=10,height=6)


p_ScM=ggplot(mean_d,aes(day,m_ScM,color=Group,shape=Group))+theme_pubr()+
  scale_x_continuous(sec.axis=sec_axis(~.,breaks=c(1,4.5,8.5,10.5),labels=c("Référence","Entraînement & stimulation","Court terme","Long terme")),breaks=1:11)+
  geom_rect(data=data_long,aes(xmin=1.5,xmax=7.5,ymin=-Inf,ymax=+Inf),fill="grey",alpha=0.01,inherit.aes = FALSE)+
  geom_vline(xintercept = seq(1.5,7.5,2),linetype="dotted",alpha=0.5)+geom_vline(xintercept =9.5,alpha=0.3,linetype="solid",size=0.5)+
  stat_smooth(method=lm,formula=y~ln(x),se=FALSE,show.legend = FALSE )+
  stat_summary(geom="point",fun="mean",size=3 ,position=position_dodge(width=0.5))+
  labs(x="Session de Jeu",y="Score par Minute")+
  stat_summary(fun.data = "mean_se", fun.args = list(mult = 1),size=0.5 ,show.legend = FALSE,geom="errorbar",width=0.1 ,position=position_dodge(width=0.5))+
  scale_y_continuous(breaks =seq(0, 15000, by = 2500))+
  theme(legend.position = c(0.815,0.15),legend.background = element_rect(fill=NA),legend.title = element_blank(),
        axis.title = element_text(size=18,margin=0.1),legend.text = element_text(size=16),text=element_text(size=16))+
  annotate("text",x=c(1,2.5,4.5,6.5,8.5,10.5),y=Inf,vjust=1.5,label=c("Jour 1","Jour 2","Jour 3","Jour 4","Jour 5","Jour 15"),size=5)

ggsave(plot=p_ScM,"Poster\\ScM_SHAM_STIMHD_allSession.pdf",device="pdf",width=10,height=6)
