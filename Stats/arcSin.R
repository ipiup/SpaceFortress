library(ggplot2)
library(ggpubr)
library(e1071)#for skewness  
library(dplyr)

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

#ARCSIN TRANSFORMATION
final_df$Flight_ASin=asin(sqrt(final_df$Mine/10000))
final_df$Bonus_ASin=asin(final_df$Bonus/10000)

ggdensity(final_df,"Bonus_ASin")+geom_histogram(binwidth = 0.005)+xlab("Bonus Score")
ggqqplot(final_df,"Bonus_ASin")

ggdensity(final_df,"Flight_ASin")+geom_histogram(binwidth = 0.005)+xlab("Flight Score")

#SQUAREROOT Trasnformation
final_df$Mine_SQT=sqrt(final_df$Mine)
#not good: negativ value NaN

#LOG TRANSFORMATION
final_df$Mine_LOG=log10(final_df$Mine)
#not good:  negativ value NaN


#YEO JOHNSON
install.packages("bestNormalize")
library(bestNormalize)
mine_yeo=yeojohnson(final_df$Mine)
final_df$Mine_YeoJ=predict(yeojohnson(final_df$Mine))
ggdensity(final_df,"Mine_YeoJ")
