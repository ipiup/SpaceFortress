library("SciViews")
library("broom")
library("Metrics")
#Model
#y=a+r*ln(x) 
#y=perf zscore (D01), a=baseline perf zscore, r=learning rate, x=session

########################
#generating the data
#gen_data_P2 : data with 2nd session only
gen_data_P2$D=1:6 #adds the day number in a numerical way

#plot(gen_data_P2$Zscore~gen_data_P2$D)

fit=lm(Zscore~log(D),data=gen_data_P2)

summary(fit)
coef(fit)
a1=coef(fit)[1]
a2=coef(fit)[2]
x0=exp(a1)
mu=a2

eq=paste0("Equation: Zscore= ",round(a1,4)," + ",round(a2,4),"*log(D)")

plot_log=ggplot(gen_data_P2,aes(log(D),Zscore))+geom_point()+theme_classic2()+annotate("text",label=eq,x=0.3,y=2)+
  geom_abline(intercept=coef(fit)[1],slope=coef(fit)[2],col="blue")+stat_summary(geom="point",col="red",fun="mean",size=3)+labs(x="log(Day)")

plot_logreg=ggplot(gen_data_P2,aes(D,Zscore))+geom_point()+theme_classic2()+annotate("text",label=paste("Equation :","y~log(x)"),x=2,y=2)+
  stat_smooth(method=lm,formula=y~log(x))+stat_summary(geom="point",col="red",fun="mean",size=3)+labs(x="Day")+
  scale_x_continuous(breaks=1:6)

plot_linreg=ggplot(gen_data_P2,aes(D,Zscore))+geom_point()+theme_classic2()+annotate("text",label=paste("Equation :","y~x"),x=2,y=2)+
  stat_smooth(method=lm,formula=y~x)+stat_summary(geom="point",col="red",fun="mean",size=3)+labs(x="Day")+
  scale_x_continuous(breaks=1:6)

figure_reg=ggarrange(plot_log,ggarrange(plot_linreg,plot_logreg,ncol=2,labels=c("B","C")),labels=c("A"),nrow=2)
figure_reg

###############
#PAREIL MAIS SUR TOUTES LES SESSIONS
gen_data$D=1:11 #adds the day number in a numerical way
gen_data_noJ14=subset(gen_data,Day!="D14")
fit_all=lm(Zscore~log(D),data=gen_data)
fit_all_lin=lm(Zscore~D,data=gen_data)
coef(fit_all_lin)
summary(fit_all)
coef(fit_all)
a1=coef(fit_all)[1]
a2=coef(fit_all)[2]
x0=exp(a1)
mu=a2
eq=paste0("Equation: Zscore= ",round(a1,4)," + ",round(a2,4),"*log(D)")

plot_log=ggplot(gen_data,aes(log(D),Zscore))+geom_point()+theme_classic2()+annotate("text",label=eq,x=0.5,y=2)+
  geom_abline(intercept=coef(fit_all)[1],slope=coef(fit_all)[2],col="blue")+stat_summary(geom="point",col="red",fun="mean",size=3)+labs(x="log(Day)")

plot_logreg=ggplot(gen_data,aes(D,Zscore))+geom_point()+theme_classic2()+annotate("text",label=paste("Equation :","y~log(x)"),x=3,y=2)+
  stat_smooth(method=lm,formula=y~log(x))+stat_summary(geom="point",col="red",fun="mean",size=3)+labs(x="Day")+
  scale_x_continuous(breaks=1:11)

plot_linreg=ggplot(gen_data,aes(D,Zscore))+geom_point()+theme_classic2()+annotate("text",label=paste("Equation :","y~x"),x=3,y=2)+
  stat_smooth(method=lm,formula=y~x)+stat_summary(geom="point",col="red",fun="mean",size=3)+labs(x="Day")+
  scale_x_continuous(breaks=1:11)

figure_reg=ggarrange(plot_log,ggarrange(plot_linreg,plot_logreg,ncol=2,labels=c("B","C")),labels=c("A"),nrow=2)
figure_reg

by_pseudo=gen_data%>%
  group_by(Pseudo)


by_pseudo_noJ14=subset(by_pseudo,Day!="D14")

###### Learning rate and Initial Zscore
library(broom)
logreg_pseudo=do(by_pseudo,tidy(lm(Zscore~log(D),data=.)))

intercept_pseudo=logreg_pseudo$estimate[logreg_pseudo$term=="(Intercept)"]
slope_pseudo=logreg_pseudo$estimate[logreg_pseudo$term=="log(D)"]

df=data.frame(Pseudo=gen_data$Pseudo[gen_data$Day=="D01"],Treatment=gen_data$Treatment[gen_data$Day=="D01"],ZscoreD1=gen_data$Zscore[gen_data$Day=="D01"],LearningRate=slope_pseudo)
cor(df$ZscoreD1,df$LearningRate)
ggplot(df,aes(ZscoreD1,LearningRate),add="reg.line")+geom_point()+theme_classic2()+stat_cor(method="pearson")+
  geom_smooth(method='lm',formula=y~x, se = FALSE)+labs(title="Correlation between Learning Rate & Initial Zscore")

fit_zscoreinitial=lm(LearningRate~ZscoreD1,data=df)
summary(fit_zscoreinitial)
coef(fit_zscoreinitial)

#t-test of learning rate between group
t_test_learningrate=t.test(df$LearningRate[df$Treatment==1],df$LearningRate[df$Treatment==2])

bxp_LearningRate=ggboxplot(df,x="Treatment",y="LearningRate",color="Treatment",palette="jco",add="jitter")+labs(x="Group",y="LearningRate",title="LearningRate")+stat_compare_means(method="t.test",label.x = 1.35, label.y = 0.9*max(df$LearningRate))+rremove("legend")

######################
# ON ZMEAN (mean of the ZSubscores)
#SAME BUT N0J14
gen_data$D=1:11
gen_data_noJ14=subset(gen_data,Day!="D14")
#fit_all=lm(ZMean~log(D),data=gen_data_noJ14)
fit_all=lm(ZMean~ln(D),data=gen_data_noJ14)
fit_all_lin=lm(ZMean~D,data=gen_data_noJ14)
coef(fit_all_lin)
summary(fit_all)
coef(fit_all)
a1=coef(fit_all)[1]
a2=coef(fit_all)[2]
x0=exp(a1)
mu=a2
eq=paste0("Equation: ZMean= ",round(a1,4)," + ",round(a2,4),"*ln(D)")

plot_ln=ggplot(gen_data_noJ14,aes(ln(D),ZMean))+geom_point()+theme_classic2()+annotate("text",label=eq,x=0.5,y=2)+
  geom_abline(intercept=coef(fit_all)[1],slope=coef(fit_all)[2],col="blue")+stat_summary(geom="point",col="red",fun="mean",size=3)+labs(x="ln(Day)")

plot_lnreg=ggplot(gen_data_noJ14,aes(D,ZMean))+geom_point()+theme_classic2()+annotate("text",label=paste("Equation :","y~ln(x)"),x=3,y=2)+
  stat_smooth(method=lm,formula=y~ln(x))+stat_summary(geom="point",col="red",fun="mean",size=3)+labs(x="Day")+
  scale_x_continuous(breaks=1:11)

plot_linreg=ggplot(gen_data_noJ14,aes(D,ZMean))+geom_point()+theme_classic2()+annotate("text",label=paste("Equation :","y~x"),x=3,y=2)+
  stat_smooth(method=lm,formula=y~x)+stat_summary(geom="point",col="red",fun="mean",size=3)+labs(x="Day")+
  scale_x_continuous(breaks=1:11)

figure_reg=ggarrange(plot_ln,ggarrange(plot_linreg,plot_lnreg,ncol=2,labels=c("B","C")),labels=c("A"),nrow=2)
figure_reg

#Akaike Information Criterion
AIC(fit_all)
AIC(fit_all_lin)

by_pseudo=gen_data_noJ14%>%
  group_by(Pseudo)
by_pseudo_noJ14=subset(by_pseudo,Day!="D14")

lnreg_pseudo=do(by_pseudo_noJ14,tidy(lm(ZMean~ln(D),data=.)))

intercept_pseudo=lnreg_pseudo$estimate[lnreg_pseudo$term=="(Intercept)"]
slope_pseudo=lnreg_pseudo$estimate[lnreg_pseudo$term=="ln(D)"]

df_Zmean=data.frame(Pseudo=gen_data_noJ14$Pseudo[gen_data_noJ14$Day=="D01"],ZMeanD1=gen_data_noJ14$ZMean[gen_data_noJ14$Day=="D01"],LearningRate=slope_pseudo)

cor(df_Zmean$ZMeanD1,df_Zmean$LearningRate)
ggplot(df_Zmean,aes(ZMeanD1,LearningRate),add="reg.line")+geom_point()+theme_classic2()+stat_cor(method="pearson")+
  geom_smooth(method='lm',formula=y~x, se = FALSE)+labs(title="Correlation between Learning Rate & Initial ZMean")

##RMSE
rmse(gen_data_noJ14$ZMean,predict(lm(ZMean~ln(D),data=gen_data_noJ14)))

rmse=do(by_pseudo_noJ14,tidy(rmse(by_pseudo_noJ14$ZMean,predict(lm(ZMean~ln(D),data=by_pseudo_noJ14)))))

rmse$rmse=NA
rootMean=NULL
for(str_pseudo in unique(gen_data_noJ14$Pseudo)){
     #rmse["Pseudo"==str_pseudo]$rmse
  rootMean=append(rootMean,rmse(subset(gen_data_noJ14,Pseudo==str_pseudo)$ZMean,predict(lm(ZMean~ln(D),data=subset(gen_data_noJ14,Pseudo==str_pseudo)))))
}
rmse$rmse=rootMean
gen_data_noJ14$RMSE=rep(rootMean,each=9)
gen_data_noJ14$SLOPE=rep(slope_pseudo,each=9)

ggscatter(gen_data_noJ14,x="SLOPE",y="RMSE")

#####
fit_ZMeaninitial=lm(LearningRate~ZMeanD1,data=df_Zmean)
summary(fit_ZMeaninitial)
coef(fit_ZMeaninitial)

df_Zmean=data.frame(Pseudo=gen_data_noJ14$Pseudo[gen_data_noJ14$Day=="D01"],Treatment=gen_data_noJ14$Treatment[gen_data_noJ14$Day=="D01"],ZMeanD1=gen_data_noJ14$ZMean[gen_data_noJ14$Day=="D01"],LearningRate=slope_pseudo)
t_test_learningrate=t.test(df_Zmean$LearningRate[df_Zmean$Treatment==1],df_Zmean$LearningRate[df_Zmean$Treatment==2])
bxp_LearningRate=ggboxplot(df_Zmean,x="Treatment",y="LearningRate",color="Treatment",palette="jco",add="jitter")+labs(x="Group",y="LearningRate",title="LearningRate")+stat_compare_means(method="t.test",label.x = 1.35, label.y = 0.9*max(df_Zmean$LearningRate))+rremove("legend")
bxp_LearningRate

#######################
#SAME BUT N0J14 ON Zscore (of the total Score)
gen_data$D=1:11
gen_data_noJ14=subset(gen_data,Day!="D14")
#fit_all=lm(Zscore~log(D),data=gen_data_noJ14)
fit_all=lm(Zscore~ln(D),data=gen_data_noJ14)
fit_all_lin=lm(Zscore~D,data=gen_data_noJ14)
coef(fit_all_lin)
summary(fit_all)
coef(fit_all)
a1=coef(fit_all)[1]
a2=coef(fit_all)[2]
x0=exp(a1)
mu=a2
eq=paste0("Equation: Zscore= ",round(a1,4)," + ",round(a2,4),"*ln(D)")

plot_ln=ggplot(gen_data_noJ14,aes(ln(D),Zscore))+geom_point()+theme_classic2()+annotate("text",label=eq,x=0.5,y=2)+
  geom_abline(intercept=coef(fit_all)[1],slope=coef(fit_all)[2],col="blue")+stat_summary(geom="point",col="red",fun="mean",size=3)+labs(x="ln(Day)")

plot_lnreg=ggplot(gen_data_noJ14,aes(D,Zscore))+geom_point()+theme_classic2()+annotate("text",label=paste("Equation :","y~ln(x)"),x=3,y=2)+
  stat_smooth(method=lm,formula=y~ln(x))+stat_summary(geom="point",col="red",fun="mean",size=3)+labs(x="Day")+
  scale_x_continuous(breaks=1:11)

plot_linreg=ggplot(gen_data_noJ14,aes(D,Zscore))+geom_point()+theme_classic2()+annotate("text",label=paste("Equation :","y~x"),x=3,y=2)+
  stat_smooth(method=lm,formula=y~x)+stat_summary(geom="point",col="red",fun="mean",size=3)+labs(x="Day")+
  scale_x_continuous(breaks=1:11)

figure_reg=ggarrange(plot_ln,ggarrange(plot_linreg,plot_lnreg,ncol=2,labels=c("B","C")),labels=c("A"),nrow=2)
figure_reg

#Akaike Information Criterion
AIC(fit_all)
AIC(fit_all_lin)

by_pseudo=gen_data_noJ14%>%
  group_by(Pseudo)
by_pseudo_noJ14=subset(by_pseudo,Day!="D14")

lnreg_pseudo=do(by_pseudo_noJ14,tidy(lm(Zscore~ln(D),data=.)))

intercept_pseudo=lnreg_pseudo$estimate[lnreg_pseudo$term=="(Intercept)"]
slope_pseudo=lnreg_pseudo$estimate[lnreg_pseudo$term=="ln(D)"]

df=data.frame(ZscoreD1=gen_data_noJ14$Zscore[gen_data_noJ14$Day=="D01"],LearningRate=slope_pseudo)
cor(df$ZscoreD1,df$LearningRate)
ggplot(df,aes(ZscoreD1,LearningRate),add="reg.line")+geom_point()+theme_classic2()+stat_cor(method="pearson")+
  geom_smooth(method='lm',formula=y~x, se = FALSE)+labs(title="Correlation between Learning Rate & Initial Zscore")

fit_Zscoreinitial=lm(LearningRate~ZscoreD1,data=df)
summary(fit_Zscoreinitial)
coef(fit_Zscoreinitial)

df=data.frame(Pseudo=gen_data_noJ14$Pseudo[gen_data_noJ14$Day=="D01"],Treatment=gen_data_noJ14$Treatment[gen_data_noJ14$Day=="D01"],ZscoreD1=gen_data_noJ14$Zscore[gen_data_noJ14$Day=="D01"],LearningRate=slope_pseudo)
t_test_learningrate=t.test(df$LearningRate[df$Treatment==1],df$LearningRate[df$Treatment==2])
bxp_LearningRate=ggboxplot(df,x="Treatment",y="LearningRate",color="Treatment",palette="jco",add="jitter")+labs(x="Group",y="LearningRate",title="LearningRate")+stat_compare_means(method="t.test",label.x = 1.35, label.y = 0.9*max(df$LearningRate))+rremove("legend")
bxp_LearningRate

