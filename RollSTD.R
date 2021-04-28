#RollSTD


#One solution : with Zoo package
library("zoo")

#number of moving points
mov_point= 60
vec=unlist(df_APM_ScM$ScoresMin[1])
moving_sd<-function(vec,mov_point){
  return(rollapply(vec,width=mov_point,FUN=sd,fill=NA))} #mov_point-1 NA will occur

#Other solution : with TTR package
library("TTR")
moving_sd_ttr<- function(vec,mov_point){
  return(runSD(vec,mov_point))
  }

moving_mean<-function(vec,mov_point){
  return(rollapply(vec,mov_point,FUN=mean))
}

df_APM_ScM$RollSD=lapply(df_APM_ScM$ScoresMin,moving_sd_ttr, mov_point=60)

#SD fixe du score glissant
df_APM_ScM$ScMSD=lapply(df_APM_ScM$ScoresMin,sd)
#mean du rollSD
df_APM_ScM$RollSDMean=lapply(df_APM_ScM$RollSD,mean,na.rm=TRUE)

#SD glissant du score fixe
#ajouter score cumulé au tableau
mutate(group_by(df_data),cumsum=cumsum(Point))
df_data$CumSum=NA
df_data$CumSum[1]
length(cumsum(unlist(df_data$Point[1])))
length(unlist(df_data$Point[1]))
df_test=subset(df_test,Type!="Press")

#histogram of 1 participant points
ggplot(df_CA2209,aes(y=Point))+geom_histogram(binwidth = 10)+theme_classic()+scale_y_continuous(breaks=c(-100,-50,-35,50,60,100,250))
df_data$RollSD=lapply(df_data$Point,moving_sd_ttr,mov_point=60)



