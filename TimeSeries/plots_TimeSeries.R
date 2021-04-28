
library("ggplot2")
library("reshape2")
library("ggpubr")
library("RColorBrewer")
#install.packages("wesanderson")
library("wesanderson")
library("ggsci")
mean_list<-function(l,col){
  if(col=="Scores"){
    return(mean(unlist(l[3])))
  }else{
    return(mean(unlist(l[4])))
  }
}

df_plot_participant<-function(df,col,violin=FALSE){
  head_df_plot=c("Pseudo","T","D1","D2","D3","D4","D5","D6")
  df_plot=data.frame(matrix(vector(),nrow=(540),ncol=length(head_df_plot)),stringsAsFactors=FALSE)
  colnames(df_plot)=head_df_plot
  df_plot$Pseudo=df$Pseudo[1]
  df_plot$T=c(1:540) #time series sec
  df_plot$D1=unlist(df[1,col]) #J1 ,only one session
  j=4
  means_= apply(df,1,mean_list,col)
  for(i in seq(2,nrow(df),by=2)){
    if(any(is.na(unlist(df[i,"APM"]))) || any(is.na(unlist(df[i+1,"APM"])))){ #no NA in the APM = no pause
      df_plot[j]=unlist(df[i+1,col])
    }else{
      if((means_[i]>means_[i+1]) ){
        df_plot[j]=unlist(df[i,col])
      }else{
        df_plot[j]=unlist(df[i+1,col])
      }
    }
    j=j+1
  }
  
  df_plot2 = melt(df_plot,  id.vars = c('T','Pseudo'), variable.name = 'Days')
  plot_=ggplot(df_plot2, aes(T, value)) +geom_line(aes(colour = Days))+geom_smooth(aes(colour = Days),method="loess",span=0.1,level=0.95)+labs(x="Time (s)",y=col)+scale_x_continuous(limits=c(0,540),breaks=seq(0,540,100))
  violin_plot=ggplot(df_plot2,aes(Days,value))+geom_violin()+geom_jitter(width=0.15)
  return(plot_)
  if(violin){
    return(violin_plot)
  }
}
# 
# for(st_pseudo in unique(final_df$Pseudo)){
#   plot_score = df_plot_participant(subset(final_df,Pseudo==st_pseudo),col="ScoresMin")
#   plot_apm = df_plot_participant(subset(final_df,Pseudo==st_pseudo),col="APM")
#   figure_=ggarrange(plot_score,plot_apm,labels=c("A","B"),ncol=1,nrow=2,common.legend = TRUE,legend="right",align="v")
#   figure_=annotate_figure(figure_,fig.lab=st_pseudo,fig.lab.pos="bottom.right")
#   ggsave(paste("E:\\ISAE-2021\\Alldata\\APM_ScM_Plots\\",st_pseudo,"_APM_Scores_Plot.pdf"),figure_,width=15,height=8)
# }

concat_plot<-function(str_pseudo,df,col="ScM"){ #for one participant
  head_df_plot=c("Pseudo","T",col)
  df_plot=data.frame(matrix(vector(),nrow=(540*11),ncol=length(head_df_plot)),stringsAsFactors=FALSE)
  colnames(df_plot)=head_df_plot
  df_plot$Pseudo=str_pseudo
  df_plot$T=c(1:(540*11))
  if(col=="ScM"){
    df_plot$col=unlist(df$ScoresMin[df$Pseudo==str_pseudo])
  }else if(col=="APM"){
    df_plot$col=unlist(df$APM[df$Pseudo==str_pseudo])
  }else if(col=="RollSD"){
    df_plot$col=unlist(df$RollSD[df$Pseudo==str_pseudo])
  }else if(col=="Zscore"){
    df_plot$col=unlist(df$Zscore[df$Pseudo==str_pseudo])
  }
  vertc_line=seq(540,5400,by=540)
  plot_=ggplot(df_plot,aes(T,col))+geom_line(aes(colour=cut(T,c(0,(seq(540,5940,by=540)),Inf))),na.rm = TRUE)+geom_vline(xintercept =vertc_line,linetype="dotted")
  color_pal=wes_palette("Zissou1",n=5)
  col_values=c("forestgreen",color_pal[3],color_pal[3],color_pal[4],color_pal[4],color_pal[5],color_pal[5],color_pal[1],color_pal[1],"cornflowerblue","cornflowerblue")
  plot_=plot_+scale_color_manual(values=col_values)
  if(col=="ScM"){
    for(i in seq(1,5540,by=540)){
      #Add Mean
      col_mean=mean(df_plot$col[i:(i+539)])
      plot_=plot_+annotate("point",x=i+269,y=col_mean,size=2)
      #Add SD
      col_SD=sd(df_plot$col[i:(i+539)])
      plot_=plot_+geom_segment(x=i+269,xend=i+269,y=col_mean-col_SD,yend=col_mean+col_SD,size=0.5)
    }
    plot_=plot_+ylim(c(min(df_plot$col),min(df_plot$col)+2500))+labs(title="Score per Minute of each SF Session")
    plot_=plot_+ylab("Score Per Minute")
    #+geom_smooth(method=lm,colour="darkgrey",se=F,linetype="solid")
  }else if(col=="APM"){
    plot_=plot_+labs(title="APM of each SF Session")+ylab("APM")
    plot_=plot_+ylim(c(min(df_plot$col),min(df_plot$col)+200))
    
  }else if(col=="RollSD"){
    plot_=plot_+labs(title="RollSD of each SF Session")+ylab("RollSD")
    plot_=plot_+ylim(c(min(df_plot$col),min(df_plot$col)+200))
  }else if(col=="Zscore"){
    plot_=plot_+labs(title="Zscore of each SF Session")+ylab("Zscore")
    plot_=plot_+ylim(c(min(df_plot$col),max(df_plot$col)))
  }
  plot_=plot_+guides(color=FALSE)+xlab("Session")
  plot_=plot_+scale_x_continuous(expand=c(0,0),breaks =seq(270,5810,by=540), labels=c("D1","D2P1","D2P2","D3P1","D3P2","D4P1","D4P2","D5P1","D5P2","D14P1","D14P2"))
  plot_ =plot_+theme_bw()+theme(axis.line = element_line(colour = "black"),panel.border=element_blank()) 
  return(plot_)
}


mean_sd_plot<-function(str_pseudo,df){ #not concat df
  head_df=c("Session","SD","Mean")
  df_plot=data.frame(matrix(vector(),nrow=(11),ncol=length(head_df)),stringsAsFactors=FALSE)
  colnames(df_plot)=head_df
  df_plot$Session=df$Session[df$Pseudo==str_pseudo]
  df_plot$Session=as.factor(unlist(df_plot$Session))
  df_plot$SD=mapply(sd,df$ScoresMin[df$Pseudo==str_pseudo])
  df_plot$Mean=mapply(mean,df$ScoresMin[df$Pseudo==str_pseudo])
  plot_=ggplot(df_plot,aes(Session,Mean,color=Session))+geom_point(size=3)+geom_errorbar(aes(ymin=Mean-SD,ymax=Mean+SD),width=0.2,size=1)
  plot_ =plot_+theme_bw()+theme(axis.line = element_line(colour = "black"),panel.border=element_blank())
  color_pal=wes_palette("Zissou1",n=5)
  col_values=c("forestgreen",color_pal[3],color_pal[3],color_pal[4],color_pal[4],color_pal[5],color_pal[5],color_pal[1],color_pal[1],"cornflowerblue","cornflowerblue")
  plot_=plot_+scale_color_manual(values=col_values)+labs(title="SD of Scores Per Min ofeach SF Session")
  plot_=plot_+ylab("Mean")+guides(color=FALSE)
  plot_=plot_+geom_text(y=df_plot$Mean,color="darkgrey",label=round(df_plot$Mean,2),hjust=-0.2,size=3)
  plot_=plot_+geom_text(y=(df_plot$Mean+df_plot$SD),color="black",label=round(df_plot$SD,2),hjust=-0.4,size=3)
  plot_
  return(plot_)
  }

# for(str_pseudo in unique(conc_df$Pseudo)){
#     plot_score =concat_plot(str_pseudo,conc_df,"ScM",SD=TRUE)
#     plot_apm = concat_plot(str_pseudo,conc_df,"APM",SD=FALSE)
#     figure_=ggarrange(plot_score,plot_apm,labels=c("A","B"),ncol=1,nrow=2,common.legend = TRUE,legend="right",align="v",heights=c(4,2))
#     figure_=annotate_figure(figure_,fig.lab=str_pseudo,fig.lab.pos="bottom.right")
#     ggsave(paste("E:\\ISAE-2021\\Alldata\\APM_ScM_SDRoll_SDFix_Concat_Plots\\",str_pseudo,"_APM_Scores_Concat_Plot.pdf"),figure_,width=15,height=8)
# }

for(str_pseudo in unique(conc_df$Pseudo)){
  plot_score =concat_plot(str_pseudo,conc_df,"ScM")
  plot_rollSD = concat_plot(str_pseudo,conc_df,"RollSD")
  plot_mean=mean_sd_plot(str_pseudo,df_APM_ScM)
  figure_=ggarrange(plot_score,plot_mean,plot_rollSD,labels=c("A","B","C"),ncol=1,nrow=3,common.legend = TRUE,legend="right",align="v",heights=c(4,2.5,2))
  figure_=annotate_figure(figure_,fig.lab=str_pseudo,fig.lab.pos="bottom.right")
  figure_
  ggsave(paste("E:\\ISAE-2021\\Alldata\\APM_ScM_Figures\\",str_pseudo,"_Figure.pdf"),figure_,width=15,height=8)
}

subScore_plot<-function(str_pseudo,df){
  head_df_plot=c("T","Flight","Bonus","Mine","Fortress")
  df_plot=data.frame(matrix(vector(),nrow=(540*11),ncol=length(head_df_plot)),stringsAsFactors=FALSE)
  colnames(df_plot)=head_df_plot
  df_plot$T=c(1:(540*11))
  #df_plot$ScM=unlist(df$ScoresMin[df$Pseudo==str_pseudo])
  df_plot$Flight=unlist(df$FlightScore[df$Pseudo==str_pseudo])
  df_plot$Bonus=unlist(df$BonusScore[df$Pseudo==str_pseudo])
  df_plot$Mine=unlist(df$MineScore[df$Pseudo==str_pseudo])
  df_plot$Fortress=unlist(df$FortressScore[df$Pseudo==str_pseudo])
  df_plot_melt = melt(df_plot,  id.vars = c('T'), variable.name = 'SubScores')
#  color_pal=wes_palette("Zissou1",n=5)
  plot=ggplot(df_plot_melt,aes(x=T,y=value,color=SubScores))+geom_line(size=0.7)+facet_grid(SubScores ~.,scales="free")+scale_color_tron()
  plot=plot+theme_classic2()+theme(legend.position = "none")
  return(plot)
  }
subScore_plot(str_pseudo,conc_df)

violin_plot_group<-function(df){
  head_df_plot=c("Session","GROUP1","GROUP2")
  df_plot=data.frame(matrix(vector(),nrow=(11),ncol=length(head_df_plot)),stringsAsFactors=FALSE)
  colnames(df_plot)=head_df_plot
  df_plot$Session=c("D01P1","D02P1","D02P2","D03P1","D03P2","D04P1","D04P2","D05P1","D05P2","D14P1","D14P2")
  i=1
  for(session in unique(df$Session)){
    subdf=subset(df,df$Session==session)
    unique_sess_1=c(0)
    unique_sess_2=c(0)
    for(j in 1:length(subdf)){
      if(subdf$Treatment[j]==1){
        unique_sess_1=mapply(c,unique_sess_1,subdf$ScoresMin[j],SIMPLIFY = FALSE)
      }else{
        unique_sess_2=mapply(c,unique_sess_2,subdf$ScoresMin[j],SIMPLIFY = FALSE)
      }
    }
    unique_sess_1=unique_sess_1[[1]][-1]
    unique_sess_2=unique_sess_2[[1]][-1]
    df_plot$GROUP1[i]=list(unique_sess_1)
    df_plot$GROUP2[i]=list(unique_sess_2)
    i=i+1
  }
  df_plot_melt = melt(df_plot,  id.vars = c('Session'), variable.name = 'groups')
  
  violin_plot
  ggplot(df_plot,aes(Session,GROUP1))+geom_violin()
}

df_test=data.frame(GROUP1=unlist(df_plot_melt$GROUP1),GROUP2=unlist(df_plot_melt$GROUP2))
df_test$Session=rep(c(1:11),each=3240)
df_test$Session=as.factor(df_test$Session)

df_test_melt=melt(df_test,  id.vars = c('Session'), variable.name = 'Group')
df_test_melt$Group=as.factor(df_test_melt$Group)
library(plyr)
df_test_melt$Session=mapvalues(df_test_melt$Session, from = c(1:11), to = c("D01P1","D02P1","D02P2","D03P1","D03P2","D04P1","D04P2","D05P1","D05P2","D14P1","D14P2"))

data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}

color_pal=wes_palette("Zissou1",n=5)
#color_pal_2=wes_palette()
col_values=c("forestgreen",color_pal[3],color_pal[3],color_pal[4],color_pal[4],color_pal[5],color_pal[5],color_pal[1],color_pal[1],"royalblue4","royalblue4")
plot_=plot_+scale_color_manual(values=col_values)

violin_plot=ggplot(df_test_melt,aes(Session,value,fill=Group))+geom_violin()+theme_classic()+ylab("Score Per Minute")+xlab("Sessions")
violin_plot=violin_plot+stat_summary(fun.data = data_summary,position = position_dodge(width = 0.9))
violin_plot=violin_plot+scale_fill_manual(values=c(col_values[1],col_values[4]))

cor(unlist(conc_df$ScoresMin),unlist(conc_df$APM))
df_APM=data.frame(ScM=unlist(conc_df$ScoresMin),APM=unlist(conc_df$APM))
q=qplot(unlist(conc_df$ScoresMin),unlist(conc_df$APM))+labs(x="Scores Per Minute",y="Action Per Minute")+geom_smooth()+theme_classic()
q
hexbin_chart=ggplot(df_APM,aes(x=ScM,y=APM))+geom_hex()+theme_bw()
hexbin_chart


df_t=df_test_melt
df_t$Day=NA
df_t$Day[df_t$Day=="D5"]="D"


violin_plot=ggplot(df_t,aes(Day,value,fill=Group))+geom_violin()+theme_classic()+ylab("Score Per Minute")+xlab("Sessions")
violin_plot=violin_plot+stat_summary(fun.data = data_summary,position = position_dodge(width = 0.9))
violin_plot=violin_plot+scale_fill_manual(values=c(col_values[1],col_values[4]))
violin_plot



