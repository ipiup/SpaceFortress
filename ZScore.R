library("ggplot2")
library("ggpubr")
#Zscore on Score Per Min
p1=ggdensity(unlist(df_APM_ScM$ScoresMin))+geom_histogram(binwidth =40)+xlab("ScoreMin")
z_score=scale(unlist(df_APM_ScM$ScoresMin))
p2=ggdensity(z_score)+geom_histogram(binwidth = 0.1)+xlab("Z-Score")
ggarrange(p1,p2,nrow=2,ncol=1)

df_ScM_ZSc=subset(df_APM_ScM,select=c(Pseudo,Session,Treatment,ScoresMin))
z_score_Sess=split(z_score,ceiling(seq_along(z_score)/540))
df_ScM_ZSc$Zscore=z_score_Sess
df_ScM_ZSc$MeanZscore=unlist(lapply(df_ScM_ZSc$Zscore,mean))
df_ScM_ZSc$SDZscore=unlist(lapply(df_ScM_ZSc$Zscore,sd))

data_zscore=subset(df_ScM_ZSc,grepl("P2",Session)|Session=="D01P1",select=c("Pseudo","Treatment","Session","MeanZscore","SDZscore"))
data_zscore_unmelt=dcast(data_zscore,Treatment+Pseudo~Session,value.var ="Zscore")
data_mean_zscore_unmelt=dcast(data_zscore,Treatment+Pseudo~Session,value.var ="MeanZscore")
data_sd_zscore_unmelt=dcast(data_zscore,Treatment+Pseudo~Session,value.var ="SDZscore")


#on concat
df_ScM_ZsC_concat=subset(conc_df,select=c(Pseudo,ScoresMin))
df_ScM_ZsC_concat$Zscore=split(z_score,ceiling(seq_along(z_score)/(540*11))) 


concat_plot(str_pseudo,df_ScM_ZsC_concat,col="Zscore")


library("psych")
df_ScM_ZSc$MSSD=lapply(df_ScM_ZSc$Zscore,mssd,lag=1)

#####################
library(tidyverse)
library(ggpubr)
library(rstatix)
#anova on mssd
df_mssd=subset(df_ScM_ZSc,select=c(Pseudo,Session,Treatment,MSSD))

df_mssd$MSSD=as.numeric(df_mssd$MSSD)

#Descriptive stats
df_mssd=df_mssd%>%
  convert_as_factor(Pseudo,Session,Treatment)
  
  
df_mssd%>%
  group_by(Treatment,Session)%>%
  get_summary_stats(MSSD,type="mean_sd")
#visualisation
bxp=ggboxplot(df_mssd, x ="Session", y = "MSSD",color="Treatment",palette="jco",add="jitter")
bxp

#Outliers
df_mssd%>%
  group_by(Treatment,Session)%>%
  identify_outliers(MSSD)

#Normality
df_mssd%>%
  group_by(Treatment,Session)%>%
  shapiro_test(MSSD)

sh_plot=ggqqplot(df_mssd, "MSSD",ggtheme=theme_bw())+facet_grid(Session~Treatment,labeller="label_both")
sh_plot

#ANOVA
#one way
# one.way =df_mssd %>%
#   group_by(Session) %>%
#   anova_test(dv=MSSD,wid=Pseudo,within =Treatment) %>%
#   get_anova_table() %>%
#   adjust_pvalue(method = "bonferroni")
# one.way
anova_file="E:\\ISAE-2021\\Alldata\\ANOVA_MSSD.txt"

res.aov=anova_test(data=df_mssd,dv=MSSD,wid=Pseudo,within=Session,between=Treatment)
anova_mssd=get_anova_table(res.aov)

#EZANOVA
library(ez)
ez.aov=ezANOVA(data=df_mssd,dv=MSSD,wid=Pseudo,within=Session,between=Treatment)

write("ANOVA MSSD by groups \n\n rstatix ANOVA \n",anova_file,append=TRUE)
capture.output(anova_mssd,file=anova_file,append=TRUE)
write("\n \n EZ ANOVA \n",anova_file,append=TRUE)
capture.output(ez.aov,file=anova_file,append=TRUE)

#ZSCORE CORRELATION BTW MEAN AND SD
p_mean=ggqqplot(data_zscore$MeanZscore,title = "Mean")
p_sd=ggqqplot(data_zscore$SDZscore,title ="SD")

res_cor=cor.test(data_zscore$MeanZscore,data_zscore$SDZscore,method="pearson")

p_cor=ggplot(data_zscore,aes(MeanZscore,SDZscore))+geom_point()+theme_classic2()+geom_smooth()+stat_cor(method="pearson")

figure_cor=ggarrange(ggarrange(p_mean,p_sd,ncol=2,labels=c("A","B")),p_cor,labels=c("","C"),nrow = 2)
figure_cor

# 
# 
# df_res_mssd=data.frame(matrix(nrow=2,ncol=7))
# colnames(df_res_mssd)=c("Group","D01","D02","D03","D04","D05","D06")
# 
# mean_D1_Gr1=mean(gen_data$TotalScore[gen_data$Session=="D01P1"&gen_data$Treatment==1])
# mean_D1_Gr2=mean(gen_data$TotalScore[gen_data$Session=="D01P1"&gen_data$Treatment==2])
# 
# mean_D2_Gr1=mean(gen_data$TotalScore[(gen_data$Session=="D02P1"|gen_data$Session=="D02P2")&gen_data$Treatment==1])
# mean_D2_Gr2=mean(gen_data$TotalScore[(gen_data$Session=="D02P1"|gen_data$Session=="D02P2")&gen_data$Treatment==2])
# 
# mean_D3_Gr1=mean(gen_data$TotalScore[(gen_data$Session=="D03P1"|gen_data$Session=="D03P2")&gen_data$Treatment==1])
# mean_D3_Gr2=mean(gen_data$TotalScore[(gen_data$Session=="D03P1"|gen_data$Session=="D03P2")&gen_data$Treatment==2])
# 
# mean_D4_Gr1=mean(gen_data$TotalScore[(gen_data$Session=="D04P1"|gen_data$Session=="D04P2")&gen_data$Treatment==1])
# mean_D4_Gr2=mean(gen_data$TotalScore[(gen_data$Session=="D04P1"|gen_data$Session=="D04P2")&gen_data$Treatment==2])
# 
# mean_D5_Gr1=mean(gen_data$TotalScore[(gen_data$Session=="D05P1"|gen_data$Session=="D05P2")&gen_data$Treatment==1])
# mean_D5_Gr2=mean(gen_data$TotalScore[(gen_data$Session=="D05P1"|gen_data$Session=="D05P2")&gen_data$Treatment==2])
# 
# mean_D6_Gr1=mean(gen_data$TotalScore[(gen_data$Session=="D14P1"|gen_data$Session=="D14P2")&gen_data$Treatment==1])
# mean_D6_Gr2=mean(gen_data$TotalScore[(gen_data$Session=="D14P1"|gen_data$Session=="D14P2")&gen_data$Treatment==2])
# 
# 
# df_res_mssd[1,1]="Group1"
# df_res_mssd[2,1]="Group2"
# 
# df_res_mssd[1,2]=mean_D1_Gr1
# df_res_mssd[2,2]=mean_D1_Gr2
# df_res_mssd[1,3]=mean_D2_Gr1
# df_res_mssd[2,3]=mean_D2_Gr2
# df_res_mssd[1,4]=mean_D3_Gr1
# df_res_mssd[2,4]=mean_D3_Gr2
# df_res_mssd[1,5]=mean_D4_Gr1
# df_res_mssd[2,5]=mean_D4_Gr2
# df_res_mssd[1,6]=mean_D5_Gr1
# df_res_mssd[2,6]=mean_D5_Gr2
# df_res_mssd[1,7]=mean_D6_Gr1
# df_res_mssd[2,7]=mean_D6_Gr2
# df_res_mssd=data.frame(matrix(nrow=2,ncol=7))
# colnames(df_res_mssd)=c("Group","D01","D02","D03","D04","D05","D06")
# 
# sd_D1_Gr1=sd(gen_data$TotalScore[gen_data$Session=="D01P1"&gen_data$Treatment==1])
# sd_D1_Gr2=sd(gen_data$TotalScore[gen_data$Session=="D01P1"&gen_data$Treatment==2])
# 
# sd_D2_Gr1=sd(gen_data$TotalScore[(gen_data$Session=="D02P1"|gen_data$Session=="D02P2")&gen_data$Treatment==1])
# sd_D2_Gr2=sd(gen_data$TotalScore[(gen_data$Session=="D02P1"|gen_data$Session=="D02P2")&gen_data$Treatment==2])
# 
# sd_D3_Gr1=sd(gen_data$TotalScore[(gen_data$Session=="D03P1"|gen_data$Session=="D03P2")&gen_data$Treatment==1])
# sd_D3_Gr2=sd(gen_data$TotalScore[(gen_data$Session=="D03P1"|gen_data$Session=="D03P2")&gen_data$Treatment==2])
# 
# sd_D4_Gr1=sd(gen_data$TotalScore[(gen_data$Session=="D04P1"|gen_data$Session=="D04P2")&gen_data$Treatment==1])
# sd_D4_Gr2=sd(gen_data$TotalScore[(gen_data$Session=="D04P1"|gen_data$Session=="D04P2")&gen_data$Treatment==2])
# 
# sd_D5_Gr1=sd(gen_data$TotalScore[(gen_data$Session=="D05P1"|gen_data$Session=="D05P2")&gen_data$Treatment==1])
# sd_D5_Gr2=sd(gen_data$TotalScore[(gen_data$Session=="D05P1"|gen_data$Session=="D05P2")&gen_data$Treatment==2])
# 
# sd_D6_Gr1=sd(gen_data$TotalScore[(gen_data$Session=="D14P1"|gen_data$Session=="D14P2")&gen_data$Treatment==1])
# sd_D6_Gr2=sd(gen_data$TotalScore[(gen_data$Session=="D14P1"|gen_data$Session=="D14P2")&gen_data$Treatment==2])
# 
# 
# df_res_mssd[1,1]="Group1"
# df_res_mssd[2,1]="Group2"
# 
# df_res_mssd[1,2]=sd_D1_Gr1
# df_res_mssd[2,2]=sd_D1_Gr2
# df_res_mssd[1,3]=sd_D2_Gr1
# df_res_mssd[2,3]=sd_D2_Gr2
# df_res_mssd[1,4]=sd_D3_Gr1
# df_res_mssd[2,4]=sd_D3_Gr2
# df_res_mssd[1,5]=sd_D4_Gr1
# df_res_mssd[2,5]=sd_D4_Gr2
# df_res_mssd[1,6]=sd_D5_Gr1
# df_res_mssd[2,6]=sd_D5_Gr2
# df_res_mssd[1,7]=sd_D6_Gr1
# df_res_mssd[2,7]=sd_D6_Gr2
# 
# 
# gen_data%>%
#   group_by(Treatment,Session)%>%
#   summarize(mean=mean(gen_data$TotalScore),sd=sd(gen_data$TotalScore))
# 
# scores_min_mean=df_APM_ScM%>%
#   group_by(Treatment,Session)%>%
#   summarize(mean=round(mean(TotalScore),2),sd=round(sd(TotalScore),2))
# 
# mssd_mean=df_mssd%>%
#   group_by(Treatment,Session)%>%
#   summarize(mssd=round(mean(MSSD),5))
# scores_min_mean$mean_mssd=mssd_mean$mssd
# write.table(scores_min_mean,"E:\\ISAE-2021\\STATS\\means.txt",sep="\t",quote=FALSE)
# 
# library(maditr)
# sc_unmelt_mean=dcast(data=scores_min_mean,formula=Treatment~Session,fun.aggregate=sum,value.var="mean")
# sc_unmelt_sd=dcast(data=scores_min_mean,formula=Treatment~Session,fun.aggregate=sum,value.var="sd")
# sc_unmelt_mssd=dcast(data=mssd_mean,formula=Treatment~Session,fun.aggregate=sum,value.var="mssd")
# write.table(sc_unmelt_mean,"E:\\ISAE-2021\\STATS\\means.txt",sep="\t",quote=FALSE)
# write.table(sc_unmelt_sd,"E:\\ISAE-2021\\STATS\\means.txt",sep="\t",quote=FALSE,append=TRUE)
# write.table(sc_unmelt_mssd,"E:\\ISAE-2021\\STATS\\means.txt",sep="\t",quote=FALSE,append=TRUE)
# 
