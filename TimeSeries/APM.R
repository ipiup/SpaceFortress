#APM
df_APM=data.frame(Treatment=rep(df_APM_ScM$Treatment,each=540),Session=rep(df_APM_ScM$Session,each=540),ScM=unlist(df_APM_ScM$ScoresMin),APM=unlist(df_APM_ScM$APM))
df_APM=subset(df_APM, !grepl("P1",Session)|Session=="D01P1") #keep second session only
df_APM$Session=substr(df_APM$Session,1,3)

df_APM_mean=df_APM%>%
  group_by(Treatment,Session)%>%
  summarise(m=mezn)
cor(df_APM$ScM,df_APM$APM)

q=ggplot(df_APM,aes(ScM,APM,color=Treatment))+labs(x="Scores Per Minute",y="Action Per Minute")+geom_point(data=transform(df_APM,Session=NULL),color="grey85")+geom_point()+theme_classic()
q+facet_wrap(vars(Session),ncol=2)+stat_mean(aes(color=Treatment),color="red",size=5)

hexbin_chart=ggplot(df_APM,aes(x=ScM,y=APM))+geom_hex()+theme_bw()
hexbin_chart

df_APM$Treatment=as.factor(df_APM$Treatment)
ggscatter(df_APM,x="ScM",y="APM",color="Treatment",size=0.2,ellipse=FALSE,ellipse.alpha=0.1,alpha=0.5)+stat_mean(aes(color=Treatment),size=8)+facet_wrap(vars(Session),ncol=2)
