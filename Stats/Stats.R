library("ggplot2")
library("ggpubr")

ggdensity(final_df$TotalScore)+geom_histogram(binwidth = 250,color="lightblue")
p1=ggdensity(final_df$Flight)+geom_histogram(binwidth = 100)

p1=ggdensity(final_df$Flight)+geom_histogram(binwidth = 100)+xlab("Flight Score")
p2=ggdensity(final_df$Bonus)+geom_histogram(binwidth = 100)+xlab("Bonus Score")
p3=ggdensity(final_df$Mine)+geom_histogram(binwidth = 100)+xlab("Mine Score")
p4=ggdensity(final_df$Fortress)+geom_histogram(binwidth=250)+xlab("Fortress Score")
figure=ggarrange(p1,p2,p3,p4,ncol=2,nrow=2)
figure

ggdensity(final_df$Mine_Scale)+geom_histogram(binwidth = 0.1)+xlab("Score")

library("tidyverse")

shapiro.test(df_APM_ScM$TotalScore[df_APM_ScM$Treatment==1])

shapiro.test(df_APM_ScM$TotalScore[])

df_APM_ScM%>%
  group_by(Treatment,Session)%>%
  summarise(statistic=shapiro.test(Bonus)$statistic,p.value=shapiro.test(Bonus)$p.value)

gen_data%>%
  group_by(Treatment)%>%
  summarise(statistic=shapiro.test(Bonus)$statistic,p.value=shapiro.test(Bonus)$p.value)



library(lme4)
#install.packages("lmerTest")
library(lmerTest)
mod_rep=lmerTest::lmer(TotalScore~Session+(1|Pseudo),data=df_APM_ScM)
summary(mod_rep)
Anova(mod_rep)
library(car)
qqPlot(residuals(mod_rep))
qqPlot(df_APM_ScM$TotalScore)
shapiro.test(residuals(mod_rep))



##J1-15
df_Short_Term=subset(df_APM_ScM,(Session!="D14P1"&Session!="D14P2"),select=c("Pseudo","Session","TotalScore","Flight","Bonus","Mine","Fortress","Treatment"))
##J5-14
df_Long_Term=subset(df_APM_ScM,(Session=="D14P1"|Session=="D14P2"|Session=="D05P1"|Session=="D05P2"),select=c("Pseudo","Session","TotalScore","Flight","Bonus","Mine","Fortress","Treatment"))

##ANOVA ASSUMPTIONS
#No significant outliers identify_outliers()
#Normality ggqplot()
#Homogeneity of variances levene_test()
#Assumption of sphericity mauchly's test of sphericity
#Homogeneity of covariances Box's M

library(tidyverse)
library(ggpubr)
library(rstatix)
library("wesanderson")
df_Short_Term=data.frame(matrix(ncol=8,nrow=414))
colnames(df_Short_Term)=c("Pseudo","Session","TotalScore","Flight","Bonus","Mine","Fortress","Treatment")
for(str_pseudo in unique(df_APM_ScM$Pseudo)){
  df_Short_Term$Pseudo=str_pseudo
  
}
#data preparation
df_Short_Term%>%
  convert_as_factor(Pseudo,Session)
#summary stat
df_Short_Term%>%
  group_by(Session,Treatment)%>%
  get_summary_stats(TotalScore,type="mean_sd")

#vizualisation
bxp=ggboxplot(df_Short_Term,x="Session",y="TotalScore",color="Treatment",palette=wes_palette("Zissou1",n=4))
bxp
#outliers
df_Short_Term%>%
  group_by(Session,Treatment)%>%
  identify_outliers(TotalScore)

#Normality 
df_Short_Term%>%
  group_by(Session,Treatment)%>%
  shapiro_test(TotalScore)

ggqqplot(df_Short_Term,"TotalScore",ggtheme=theme_bw())+facet_grid(Session~Treatment)


#Homogeneity of variance
df_Short_Term%>%
  group_by(Session)%>%
  levene_test(TotalScore~Treatment)

#Homogeneity of covariances assumption
box_m(df_APM_ScM[,"TotalScore",drop=FALSE],df_APM_ScM$Treatment)

#Assumption of sphericity & ANOVA
res.aov=anova_test(data=df_Short_Term,dv=TotalScore,wid=Pseudo,between=Treatment,within=Session)
res.aov

#Post_hoc test
# Effect of group at each time point
one.way=df_Short_Term%>%
  group_by(Session)%>%
  anova_test(dv=TotalScore,wid=Pseudo,between=Treatment)%>%
  get_anova_table()%>%
  adjust_pvalue(method="bonferroni")

one.way

# Pairwise comparisons between group levels
pwc=df_Short_Term%>%
  group_by(Session)%>%
  pairwise_t_test(TotalScore~Treatment,p.adjust.method = "bonferroni")

pwc

#vizualisation
pwc=pwc%>%add_xy_position(x="Session")
bxp+stat_pvalue_manual(pwc,tip.length=0,hide.ns=TRUE)+labs(subtitle=get_test_label(res.aov,detailed=TRUE),
                                                           caption=get_pwc_label(pwc))

#Cor Nb of Shots/ fortress Score
cor(final_df$Fortress,final_df$FortressShot)
ggscatter(final_df,x="Fortress",y="FortressShot",add="reg.line", add.params = list(color = "blue", fill = "lightgray"),conf.int = TRUE )+stat_cor(method="pearson")
#ratio
final_df$FortressRatio=((final_df$Fortress/250)/final_df$FortressShot)*10
ggscatter(final_df,x="FortressRatio",y="Fortress",add="reg.line", add.params = list(color = "blue", fill = "lightgray"),conf.int = TRUE )+stat_cor(method="pearson")
ggdensity(final_df$FortressRatio)+geom_histogram(binwidth = 0.01)


#####
final_df_D14=subset(final_df,Session=="D14P1"|Session=="D14P2")
anova_test(final_df_D14,dv=TotalScoreSub,wid=Pseudo,within=Session,between = Treatment)

