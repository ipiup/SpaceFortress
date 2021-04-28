library("stats")
library("ggplot2")
library("ggpubr")
#Density of each SubScore
ggdensity(gen_data$TotalScore)+geom_histogram(binwidth =200)+xlab("Total Score")

p1=ggdensity(gen_data$Flight)+geom_histogram(binwidth = 100)+xlab("Flight Score")
p2=ggdensity(gen_data$Bonus)+geom_histogram(binwidth = 100)+xlab("Bonus Score")
p3=ggdensity(gen_data$Mine)+geom_histogram(binwidth = 100)+xlab("Mine Score")
p4=ggdensity(gen_data$Fortress)+geom_histogram(binwidth = 200)+xlab("Fortress Score")
figure=ggarrange(p1,p2,p3,p4,ncol=2,nrow=2)
figure

p0=ggqqplot(gen_data$TotalScore)+xlab("Total Score")
p1=ggqqplot(gen_data$Flight)+xlab("Flight Score")
p2=ggqqplot(gen_data$Bonus)+xlab("Bonus Score")
p3=ggqqplot(gen_data$Mine)+xlab("Mine Score")
p4=ggqqplot(gen_data$Fortress)+xlab("Fortress Score")
figure=ggarrange(p1,p2,p3,p4,ncol=2,nrow=2)
figure


shapiro.test(gen_data$TotalScore)
shapiro.test(gen_data$Flight)
shapiro.test(gen_data$Bonus)
shapiro.test(gen_data$Mine)
shapiro.test(gen_data$Fortress)

#Kolmogorov-Smirnov Test (for normality)
ks.test(gen_data$TotalScore,"punif",min(gen_data$TotalScore),max(gen_data$TotalScore))


#Zscore with scale (1st choice)
gen_data$Zscore_Flight=as.numeric(scale(gen_data$Flight))
gen_data$Zscore_Bonus=as.numeric(scale(gen_data$Bonus))
gen_data$Zscore_Mine=as.numeric(scale(gen_data$Mine))
gen_data$Zscore_Fortress=as.numeric(scale(gen_data$Fortress))



########################
#PCA on ZSubScores
#install.packages("factoextra")
library("factoextra")
gen_data_subZ=subset(gen_data,select=c("Zscore_Flight","Zscore_Bonus","Zscore_Mine","Zscore_Fortress"))
gen_data_sub=subset(gen_data,select=c("Flight","Bonus","Mine","Fortress"))
res.pca=prcomp(gen_data[,c("Zscore_Flight","Zscore_Bonus","Zscore_Mine","Zscore_Fortress")])
fviz_eig(res.pca)

library(FactoMineR)
pca=PCA(gen_data_sub,ncp=3)
pca$var$cor
summary(pca)
library(xtable)
xtable(pca$var$cor)
