# Total ScoreRaw data plot on gen_data

library(ggplot2)
library(ggpubr)
library(wesanderson)

gen_data$Treatment=as.factor(gen_data$Treatment)
gen_data_P2$Treatment=as.factor(gen_data_P2$Treatment)

#Total Score by group
Total_Score_plot=ggplot(gen_data,aes(Session,TotalScore,group=Pseudo,color=Treatment))+geom_line()+theme_classic2()+geom_point()

#Sub Scores by group
Flight_Score_plot=ggplot(gen_data,aes(Session,Flight,group=Pseudo,color=Treatment))+geom_line()+theme_classic2()+geom_point()
Bonus_Score_plot=ggplot(gen_data,aes(Session,Bonus,group=Pseudo,color=Treatment))+geom_line()+theme_classic2()+geom_point()
Mine_Score_plot=ggplot(gen_data,aes(Session,Mine,group=Pseudo,color=Treatment))+geom_line()+theme_classic2()+geom_point()
Fortress_Score_plot=ggplot(gen_data,aes(Session,Fortress,group=Pseudo,color=Treatment))+geom_line()+theme_classic2()+geom_point()

#Zscore plot
ZScore_plot=ggplot(gen_data_P2,aes(Day,Zscore,group=Pseudo,color=Treatment))+geom_line()+theme_classic2()+geom_point()

#Figure
figure=ggarrange(Total_Score_plot,Flight_Score_plot,Fortress_Score_plot,Bonus_Score_plot,Mine_Score_plot,ncol=2,nrow=3,common.legend = TRUE)
figure



#Total Score by group
Total_Score_plot=ggplot(gen_data_P2,aes(Day,TotalScore,color=Treatment))+geom_boxplot(outlier.color = "white")+theme_classic2()+geom_point(aes(color=Treatment),position=position_dodge(width = 0.75))

#Sub Scores by group
Flight_Score_plot=ggplot(gen_data_P2,aes(Day,Flight,color=Treatment))+geom_boxplot(outlier.color = "white")+theme_classic2()+geom_point(aes(color=Treatment),position=position_dodge(width = 0.75))

Bonus_Score_plot=ggplot(gen_data_P2,aes(Day,Bonus,color=Treatment))+geom_boxplot(outlier.color = "white")+theme_classic2()+geom_point(aes(color=Treatment),position=position_dodge(width = 0.75))

Mine_Score_plot=ggplot(gen_data_P2,aes(Day,Mine,color=Treatment))+geom_boxplot(outlier.color = "white")+theme_classic2()+geom_point(aes(color=Treatment),position=position_dodge(width = 0.75))

Fortress_Score_plot=ggplot(gen_data_P2,aes(Day,Fortress,color=Treatment))+geom_boxplot(outlier.color = "white")+theme_classic2()+geom_point(aes(color=Treatment),position=position_dodge(width = 0.75))


#Zscore plot
ZScore_plot=ggplot(gen_data_P2,aes(Day,Zscore,color=Treatment))+geom_boxplot(outlier.color = "white")+theme_classic2()+geom_point(aes(color=Treatment),position=position_dodge(width = 0.75))

#Figure
figure=ggarrange(Total_Score_plot,ZScore_plot,Flight_Score_plot,Fortress_Score_plot,Bonus_Score_plot,Mine_Score_plot,ncol=2,nrow=3,common.legend = TRUE)
figure

mean_TotalScore=final_df%>%
  group_by(Treatment,Session)%>%
  summarise(mean(TotalScore))
colnames(mean_TotalScore)=c("GROUP","Session","TotalScoreMean")
mean_TotalScore$GROUP=as.factor(mean_TotalScore$GROUP)

for(i in unique(mean_TotalScore$GROUP)){
  mean_TotalScore$TotalScoreMean[mean_TotalScore$GROUP==i]= mean_TotalScore$TotalScoreMean[mean_TotalScore$GROUP==i]-mean_TotalScore$TotalScoreMean[mean_TotalScore$GROUP==i&mean_TotalScore$Session=="D01P1"]
}

for(str_pseudo in unique(final_df$Pseudo)){
  final_df$TotalScoreSub[final_df$Pseudo==str_pseudo]=final_df$TotalScore[final_df$Pseudo==str_pseudo]-final_df$TotalScore[final_df$Pseudo==str_pseudo&final_df$Session=="D01P1"]
  }

final_mean=ggplot(mean_TotalScore,aes(Session,TotalScoreMean,color=GROUP,group=GROUP))+geom_point()+theme_classic2()+geom_line(aes(color=mean_TotalScore$GROUP))

final_df$Treatment=as.factor(final_df$Treatment)

#1: STIM SD, 2 SHAM, 3 STIM HD
final_df$GROUP[final_df$Treatment==1]="02SD"
final_df$GROUP[final_df$Treatment==2]="01SHAM"
final_df$GROUP[final_df$Treatment==3]="03HD"
boxplot_final
ggplot(final_df,aes(Session,TotalScore,color=GROUP))+geom_boxplot()+theme_classic2()+geom_point(aes(color=GROUP),position=position_dodge(width = 0.75))+scale_color_discrete(name="Groups",labels=c("SHAM","SD","HD"))
boxplot_final

