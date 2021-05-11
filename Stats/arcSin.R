library(ggplot2)
library(ggpubr)
library(e1071)#for skewness  
library(dplyr)
library(BBmisc)

final_df$Zscore=scale(final_df$TotalScore)

#SKEWNESS
skewness(final_df$Flight)
skewness(final_df$Mine)
skewness(final_df$Bonus)
skewness(final_df$Fortress)

#QQPLOT
ggqqplot(final_df,"Flight")
ggqqplot(final_df,"Mine")
ggqqplot(final_df,"Bonus")
ggqqplot(final_df,"Fortress")


#Rescale Mine
final_df$Mine_Scale=final_df$Mine+(abs(min(final_df$Mine)))+1
#Inverse Flight
final_df$Flight_Inv=abs(final_df$Flight)+1

#ARCSIN TRANSFORMATION
final_df$Flight_ASin=asin(final_df$Flight_Inv/100000)
final_df$Mine_ASin=asin(final_df$Mine_Scale/100000)
final_df$Fortress_ASin=asin(final_df$Fortress/100000)

p1=ggdensity(final_df$Flight_ASin)+geom_histogram(binwidth=0.0005)+xlab("Flight Score")
p2=ggdensity(final_df$Mine_ASin)+geom_histogram(binwidth=0.0005)+xlab("Mine Score")
p3=ggdensity(final_df$Fortress_ASin)+geom_histogram(binwidth=0.0025)+xlab("Fortress Score")
q1=ggqqplot(final_df$Flight_ASin)+xlab("Flight Score")
q2=ggqqplot(final_df$Mine_ASin)+xlab("Mine Score")
q3=ggqqplot(final_df$Fortress_ASin)+xlab("Fortress Score")
figure_ASin=ggarrange(p1,q1,p2,q2,p3,q3,ncol=2,nrow=3)
figure_ASin

#ARCSIN(SQRT) TRANSFORMATION
final_df$Flight_ASinSqrt=asin(sqrt(final_df$Flight_Inv/100000))
final_df$Mine_ASinSqrt=asin(sqrt(final_df$Mine_Scale/100000))
final_df$Fortress_ASinSqrt=asin(sqrt(final_df$Fortress/100000))

p1=ggdensity(final_df$Flight_ASinSqrt)+geom_histogram(binwidth=0.005)+xlab("Flight Score")
p2=ggdensity(final_df$Mine_ASinSqrt)+geom_histogram(binwidth=0.005)+xlab("Mine Score")
p3=ggdensity(final_df$Fortress_ASinSqrt)+geom_histogram(binwidth=0.015)+xlab("Fortress Score")
q1=ggqqplot(final_df$Flight_ASinSqrt)+xlab("Flight Score")
q2=ggqqplot(final_df$Mine_ASinSqrt)+xlab("Mine Score")
q3=ggqqplot(final_df$Fortress_ASinSqrt)+xlab("Fortress Score")
figure_ASinSqrt=ggarrange(p1,q1,p2,q2,p3,q3,ncol=2,nrow=3)
figure_ASinSqrt


#SQUAREROOT Trasnformation
final_df$Flight_SQRT=sqrt(final_df$Flight_Inv)
final_df$Mine_SQRT=sqrt(final_df$Mine_Scale)
final_df$Fortress_SQRT=sqrt(final_df$Fortress)

p1=ggdensity(final_df$Flight_SQRT)+geom_histogram(binwidth=1)+xlab("Flight Score")
p2=ggdensity(final_df$Mine_SQRT)+geom_histogram(binwidth=1)+xlab("Mine Score")
p3=ggdensity(final_df$Fortress_SQRT)+geom_histogram(binwidth=3)+xlab("Fortress Score")
q1=ggqqplot(final_df$Flight_SQRT)+xlab("Flight Score")
q2=ggqqplot(final_df$Mine_SQRT)+xlab("Mine Score")
q3=ggqqplot(final_df$Fortress_SQRT)+xlab("Fortress Score")
figure_SQRT=ggarrange(p1,q1,p2,q2,p3,q3,ncol=2,nrow=3)
figure_SQRT

#LOG TRANSFORMATION
final_df$Flight_LOG=log10(final_df$Flight_Inv)
final_df$Mine_LOG=log10(final_df$Mine_Scale)
final_df$Fortress_LOG=log10(final_df$Fortress)

p1=ggdensity(final_df$Flight_LOG)+geom_histogram(binwidth=0.1)+xlab("Flight Score")
p2=ggdensity(final_df$Mine_LOG)+geom_histogram(binwidth=0.1)+xlab("Mine Score")
p3=ggdensity(final_df$Fortress_LOG)+geom_histogram(binwidth=0.05)+xlab("Fortress Score")
q1=ggqqplot(final_df$Flight_LOG)+xlab("Flight Score")
q2=ggqqplot(final_df$Mine_LOG)+xlab("Mine Score")
q3=ggqqplot(final_df$Fortress_LOG)+xlab("Fortress Score")
figure_LOG=ggarrange(p1,q1,p2,q2,p3,q3,ncol=2,nrow=3)
figure_LOG

#YEO JOHNSON
#first method
library(bestNormalize)
final_df$Flight_YeoJ=yeojohnson(final_df$Flight)$x.t
final_df$Bonus_YeoJ=yeojohnson(final_df$Bonus)$x.t
final_df$Mine_YeoJ=yeojohnson(final_df$Mine,standardize = TRUE)$x.t
final_df$Fortress_YeoJ=yeojohnson(final_df$Fortress,standardize = TRUE)$x.t

p1=ggdensity(final_df$Flight_YeoJ)+geom_histogram(binwidth=0.1)+xlab("Flight Score")
p2=ggdensity(final_df$Mine_YeoJ)+geom_histogram(binwidth=0.1)+xlab("Mine Score")
p3=ggdensity(final_df$Fortress_YeoJ)+geom_histogram(binwidth=0.15)+xlab("Fortress Score")
p4=ggdensity(final_df$Bonus_YeoJ)+geom_histogram(binwidth=0.15)+xlab("Bonus Score")
q1=ggqqplot(final_df$Flight_YeoJ)+xlab("Flight Score")
q2=ggqqplot(final_df$Mine_YeoJ)+xlab("Mine Score")
q3=ggqqplot(final_df$Fortress_YeoJ)+xlab("Fortress Score")
q4=ggqqplot(final_df$Bonus)+xlab("Bonus Score")
figure_YeoJ=ggarrange(p1,q1,p2,q2,p3,q3,p4,q4,ncol=2,nrow=4)
figure_YeoJ


#ZSubScore
final_df$ZFlight=scale(final_df$Flight_YeoJ)
final_df$ZMine=scale(final_df$Mine_YeoJ)
final_df$ZBonus=scale(final_df$Bonus_YeoJ)
final_df$ZFortress=scale(final_df$Fortress_YeoJ)

final_df$ZMean=rowMeans(subset(final_df,select=c("ZMine","ZFortress","ZBonus","ZFlight")))
cor(final_df$TotalScore,final_df$ZMean)
zmean_tot_plot=ggscatter(final_df,x="Zscore",y="ZMean",add="reg.line", add.params = list(color = "blue", fill = "lightgray"),conf.int = TRUE )+stat_cor(method="pearson")
zmean_tot_plot

p1=ggdensity(final_df$ZFlight)+geom_histogram(binwidth = 0.1)+xlab("Flight Score")
p2=ggdensity(final_df$ZBonus)+geom_histogram(binwidth = 0.1)+xlab("Bonus Score")
p3=ggdensity(final_df$ZMine)+geom_histogram(binwidth = 0.1)+xlab("Mine Score")
p4=ggdensity(final_df$ZFortress)+geom_histogram(binwidth=0.1)+xlab("Fortress Score")
figure_Z=ggarrange(p1,p2,p3,p4,ncol=2,nrow=2)
figure_Z

