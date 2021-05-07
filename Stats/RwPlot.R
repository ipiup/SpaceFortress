# Total ScoreRaw data plot on gen_data

library(ggplot2)
library(ggpubr)
library(wesanderson)

gen_data$Treatment=as.factor(gen_data$Treatment)
gen_data_P2$Treatment=as.factor(gen_data_P2$Treatment)

#Total Score by group
Total_Score_plot=ggplot(gen_data_P2,aes(Day,TotalScore,group=Pseudo,color=Treatment))+geom_line()+theme_classic2()+geom_point()

#Sub Scores by group
Flight_Score_plot=ggplot(gen_data_P2,aes(Day,Flight,group=Pseudo,color=Treatment))+geom_line()+theme_classic2()+geom_point()
Bonus_Score_plot=ggplot(gen_data_P2,aes(Day,Bonus,group=Pseudo,color=Treatment))+geom_line()+theme_classic2()+geom_point()
Mine_Score_plot=ggplot(gen_data_P2,aes(Day,Mine,group=Pseudo,color=Treatment))+geom_line()+theme_classic2()+geom_point()
Fortress_Score_plot=ggplot(gen_data_P2,aes(Day,Fortress,group=Pseudo,color=Treatment))+geom_line()+theme_classic2()+geom_point()

#Zscore plot
ZScore_plot=ggplot(gen_data_P2,aes(Day,Zscore,group=Pseudo,color=Treatment))+geom_line()+theme_classic2()+geom_point()

#Figure
figure=ggarrange(Total_Score_plot,ZScore_plot,Flight_Score_plot,Fortress_Score_plot,Bonus_Score_plot,Mine_Score_plot,ncol=2,nrow=3,common.legend = TRUE)
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