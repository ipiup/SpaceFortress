library(ggplot2)
library(ggpubr)
library(ggsci)
library(reshape)
library(dplyr)
#MANOVA
gen_data_P2$Treatment=as.factor(gen_data_P2$Treatment)
gen_data_P2$Pseudo=as.factor(gen_data_P2$Pseudo)
gen_data_P2$Day=as.factor(gen_data_P2$Day)
gen_data_P2_noJ14$Treatment=as.factor(gen_data_P2_noJ14$Treatment)
gen_data_P2_noJ14$Pseudo=as.factor(gen_data_P2_noJ14$Pseudo)
gen_data_P2_noJ14$Day=as.factor(gen_data_P2_noJ14$Day)

#change dataframe shape
gen_data_P2_cast=cast(gen_data_P2,Pseudo+Treatment~Day,value="Zscore")

plot_vizu=ggplot(gen_data_P2,aes(Day,Zscore,color=Treatment))+geom_boxplot()+theme_classic2()+scale_colour_jco()
plot_vizu

plot_vizu=ggplot(gen_data_P2_noJ14,aes(Day,Zscore,colour=Treatment))+geom_boxplot()+theme_classic2()+scale_colour_jco()
plot_vizu
#MANOVA
#manova_model=lm(cbind()~Treatment,gen_data_P2)


outcome=cbind(gen_data_P2_cast$D01,gen_data_P2_cast$D02,gen_data_P2_cast$D03,gen_data_P2_cast$D04,gen_data_P2_cast$D05,gen_data_P2_cast$D14)
manova_model=manova(outcome~Treatment,data=gen_data_P2_cast)
manova_model

summary(manova_model,intercept = TRUE) #by default Pillai's test
summary(manova_model,intercept = TRUE,test="Wilks")
summary(manova_model,intercept = TRUE,test="Hotelling")
summary(manova_model,intercept = TRUE,test="Roy")

#follow-up analysis : univariate test
summary.aov(manova_model) #same as one way anova on each dependent variable

#RM Package
library(MANOVA.RM)
gen_data_P2_noJ14=subset(gen_data_P2,Day!="D14")
manova_RM=MANOVA.RM::RM(ZMean~Treatment*Day,data=gen_data_P2,subject="Pseudo")

manova_RM_noJ14=MANOVA.RM::RM(ZMean~Treatment*Day,data=gen_data_P2_noJ14,subject="Pseudo")
summary(manova_RM_noJ14)

manova_=MANOVA(D01*D02*D03*D04*D05*D14~Treatment,data=gen_data_P2_cast,subject="Pseudo")
summary(manova_)
manova_wide=MANOVA.wide(Zscore,Treatment*Day,data=gen_data_P2,subject="Pseudo")

library(rstatix)
#tukey
tukey_per_group=gen_data_P2%>%
  group_by(Day)%>%
  tukey_hsd(ZMean~Treatment)

tukey=tukey_hsd(gen_data_P2,Zscore~Day)
tukey_noJ14=tukey_hsd(gen_data_P2_noJ14,Zscore~Day)
library(xtable)
xtable(tukey_per_group)
xtable(tukey)

