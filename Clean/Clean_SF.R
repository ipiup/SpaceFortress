##CLEANING SF TXT FILES & POINT COMPUTATION

date <- function(file_name){
    return(as.Date(substr(file_name, 34, 43), format="%Y-%m-%d"))
}

date_hour<-function(file_name,clean=FALSE){
  if(clean==TRUE){
    return(as.POSIXct(str_sub(file_name,22,-8),format="%Y-%m-%d_%H-%M-%S"))
  }else{
    return(as.POSIXct(str_sub(file_name,34,-5),format="%Y-%m-%d_%H-%M-%S"))
  }
}

ID<- function(file_name,clean=FALSE){
  if(clean==TRUE){
    return(toupper(substr(file_name, 15,20)))
  }else{
    return(toupper(substr(file_name, 27, 32)))
  }
}

split_MTP<-function(file_to_split){
  row_split=which(file_to_split$e1=="display_game"&file_to_split$e2=="2", arr.ind=TRUE)
  game1_file=subset(file_to_split[c(1:(row_split-1)),])
  game2_file=subset(file_to_split[c(row_split:nrow(file_to_split)),])
  return(list(game1_file,game2_file))
  }

clean_file <- function(file_to_clean){ 
  #remove all empty rows
  clean_file=subset(file_to_clean,(e3!="user"&e1!="release"&system_time!=""&system_time!="n"&system_time!="y"),select=c("system_time","e1","e2","e3"))
  #clean_file=subset(file_to_clean,file_to_clean$e1=="display_game"|file_to_clean$e1=="game"|file_to_clean$e1=="press"|file_to_clean$e1=="collide"|e1=="destroyed"|e1=="warp"|e1=="score+"|e1=="timeout"|e1=="pnts_bonus_capture"|e1=="pnts_bonus_failure"|e1=="shots_bonus_capture"|e1=="shots_bonus_failure",select=c("system_time","e1","e2","e3"))
  clean_file=clean_file[complete.cases(clean_file),]
  #time trunc
  #clean_file$system_time=trunc(as.numeric(as.character(clean_file$system_time)))
  clean_file$system_time=round(as.numeric(as.character(clean_file$system_time)),digits=3)
  #file begins at game start and ends game over
  row_begin=which(clean_file$e1 == "game"& clean_file$e2=="start", arr.ind=TRUE)
  row_end=which(clean_file$e1 == "game"& clean_file$e2=="over", arr.ind=TRUE)
  clean_file=clean_file[c(row_begin:row_end),]
  #PAUSE DELETION
  if(any(clean_file$e2=="pause")){
    row_pause=which(clean_file$e2=="pause", arr.ind=TRUE)
    row_unpause=which(clean_file$e2=="unpause", arr.ind=TRUE)
    time_pause=clean_file$system_time[row_pause]
    time_unpause=clean_file$system_time[row_unpause]
    pause_duration=time_unpause-time_pause+1
    clean_file=subset(clean_file[-c(row_pause:row_unpause),])
    clean_file[(clean_file$system_time)>=time_unpause,"system_time"]=clean_file[(clean_file$system_time)>=time_unpause,"system_time"]-pause_duration
  }
  return(clean_file)
}

#write files containing the clean dataframe of each session of each participant 
write_file<-function(file){
  file_to_write=read.table(paste0(path,"\\",file), header=TRUE, sep="\t",dec=".",fill=TRUE)
  id=ID(file) #ID of the participant
  dat=date_hour(file) #date of the session
  
  #end_file_name=str_sub(file,34,-5)
  if(!is.na(any(file_to_write$e1=="display_game"&file_to_write$e2=="2"))){ # if there is 2 sessions in the file: MTP
    for( i in 1:2){
      session=paste0(get_day_session(file,path),"P",i)
      file_to_save=clean_file(as.data.frame(split_MTP(file_to_write)[i]))#session i is cleaned
      file_to_save$Pseudo=id #add pseudo
      file_to_save$Date=dat #add date
      file_to_save$Session=session
      file_to_save=file_to_save[c(5,6,7,1,2,3,4)] #change order or the columns
      file_to_save=compute_points(file_to_save) #compute points of the session and add columns including points details
      write.table(file_to_save, paste0(path_clean,"SpaceFortress_",id,"_",session,".txt"), append = FALSE, sep = "\t", dec = ".",row.names = TRUE, col.names = TRUE,quote=FALSE)
    }
  }else{#TLS or MTP J1
    session=get_day_session(file,path)
    file_to_save=clean_file(file_to_write)
    file_to_save$Pseudo=id#add pseudo
    file_to_save$Date=dat#add date
    file_to_save$Session=session
    file_to_save=file_to_save[c(5,6,7,1,2,3,4)] #change order or the columns
    file_to_save=compute_points(file_to_save) #compute points of the session and add columns including points details
    write.table(file_to_save, paste0(path_clean,"SpaceFortress_",id,"_",session,".txt"),append = FALSE, sep = "\t", dec = ".",row.names = TRUE, col.names = TRUE,quote=FALSE)
  }
  return(id)
}

#compute points of one session of one participant and return a dataframe (or the points only if point_only=TRUE)
compute_points<-function(file_to_read, point_only=FALSE){# if df_return = False, return the total score
  e1=file_to_read$e1
  dp=subset(file_to_read,e1=="press"|e1=="collide"|e1=="destroyed"|e1=="warp"|e1=="score+"|e1=="timeout"|e1=="pnts_bonus_capture"|e1=="pnts_bonus_failure"|e1=="shots_bonus_capture"|e1=="shots_bonus_failure"
            ,select=c("Date","Session","Pseudo","system_time","e1","e2","e3"))
  dp$Type=NA
  dp$Point=0
  dp$Group=NA
  #Ship collision
  dp$Type[dp$e1=="collide"& (dp$e2=="shell"|dp$e2=="mine_0")]="ShipDamage"
  dp$Point[dp$e1=="collide"& (dp$e2=="shell"|dp$e2=="mine_0")]=-50
  #Ship destruction
  dp$Type[dp$e1=="destroyed"& dp$e2=="ship"]="ShipDestruction"
  dp$Point[dp$e1=="destroyed"& dp$e2=="ship"]=-100
  #Border crossing
  dp$Type[dp$e1=="warp"]="BorderCrossing"
  dp$Point[dp$e1=="warp"]=-35
  #Fortress collision
  dp$Type[dp$e1=="collide"& dp$e2=="small_hex" & dp$e3=="ship"]="FortressCollision"
  dp$Point[dp$e1=="collide"& dp$e2=="small_hex" & dp$e3=="ship"]=-35
  #Fortress destruction
  dp$Type[dp$e1=="destroyed"&dp$e2=="fortress"]="FortressDestruction"
  dp$Point[dp$e1=="destroyed"&dp$e2=="fortress"]=250
  #Friends Mines destruction
  dp$Type[dp$e1=="score+"&dp$e2=="mines"&dp$e3=="50"]="FriendMineDestruction"
  dp$Point[dp$e1=="score+"&dp$e2=="mines"&dp$e3=="50"]=60
  #Foes Mines destruction
  dp$Type[dp$e1=="score+"&dp$e2=="mines"&dp$e3=="60"]="FoeMineDestruction"
  dp$Point[dp$e1=="score+"&dp$e2=="mines"&dp$e3=="60"]=50 #INVERSION?
  #Mine disappears
  dp$Type[dp$e1=="timeout"&dp$e2=="mine"]="MineExtinction"
  dp$Point[dp$e1=="timeout"&dp$e2=="mine"]=-50
  #Points Bonus capture
  dp$Type[dp$e1=="pnts_bonus_capture"]="PointsBonusCapture"
  dp$Point[dp$e1=="pnts_bonus_capture"]=100
  #Shots Bonus capture
  dp$Type[dp$e1=="shots_bonus_capture"]="ShotsBonusCapture"
  dp$Point[dp$e1=="shots_bonus_capture"]=50
  #Bonus failure
  dp$Type[dp$e1=="pnts_bonus_failure"|dp$e1=="shots_bonus_failure"]="BonusFailure"
  dp$Point[dp$e1=="pnts_bonus_failure"|dp$e1=="shots_bonus_failure"]=-50
  #Group of Scores
  dp$Group[dp$Type=="ShipDamage"|dp$Type=="ShipDestruction"|dp$Type=="BorderCrossing"|dp$Type=="FortressCollision"]="Flight"
  dp$Group[dp$Type=="FortressDestruction"]="Fortress"
  dp$Group[dp$Type=="FriendMineDestruction"|dp$Type=="FoeMineDestruction"|dp$Type=="MineExtinction"]="Mine"
  dp$Group[dp$Type=="PointsBonusCapture"|dp$Type=="ShotsBonusCapture"|dp$Type=="BonusFailure"]="Bonus"
  dp$Group[dp$e1=="press"]="Press"
  dp$Type[dp$e1=="press"]="Press"
  dp=subset(dp,!is.na(Type),select=c("Date","Session","Pseudo","system_time","e1","e2","e3","Type","Point","Group"))
  if(point_only==TRUE){
    #return(dp$Point[e1!="press"]) #returns the points only
    return(subset(dp,e1!="press",select=c("Group","Point")))
  }else{
    return(dp)
  }
}

#returns list of two vectors : ScM & APM for one session file of one participant
compute_APM_ScoreMin<-function(file_APM_ScM){  
  #All files system time are length()=1:599
  time_begin=file_APM_ScM$system_time[1] #TIME in sec for begin of the game
  #time_end=file_APM_ScM$system_time[nrow(file_APM_ScM)] #TIME in sec for end of the game
  #create the vectors that will contain the required information for this file
  scores_vector=list()
  score_flight_vector=list()
  score_bonus_vector=list()
  score_mine_vector=list()
  score_fortress_vector=list()
  apm_vector=list()
    for(i in time_begin:(time_begin+539)){ #until 60s before end
      window_data=subset(file_APM_ScM,(system_time>=i & system_time<(i+60)))
      #scores per min
      points_df=compute_points(window_data,point_only = TRUE)
      scores_minute=sum(points_df$Point) #compute_points returns a dataframe 
      score_flight=sum(points_df$Point[points_df$Group=="Flight"])
      score_bonus=sum(points_df$Point[points_df$Group=="Bonus"])
      score_mine=sum(points_df$Point[points_df$Group=="Mine"])
      score_fortress=sum(points_df$Point[points_df$Group=="Fortress"])
      #SCORES LISTS
      scores_vector[[length(scores_vector)+1]]=scores_minute
      score_flight_vector[[length(score_flight_vector)+1]]=score_flight
      score_bonus_vector[[length(score_bonus_vector)+1]]=score_bonus
      score_mine_vector[[length(score_mine_vector)+1]]=score_mine
      score_fortress_vector[[length(score_fortress_vector)+1]]=score_fortress
      #APM
      press_min = as.numeric(table(window_data$e1)["press"])
      apm_vector=append(apm_vector,press_min)
    }
  list_vect=list(scores_vector,score_flight_vector,score_bonus_vector,score_mine_vector,score_fortress_vector,apm_vector)
  # LIST ORDER : SCORE, FLIGHT, BONUS, MINE, FORTRESS, APM
  return(list_vect)
}

#write txt file with APM ScM for each session of each participant
write_APM_ScM <- function(clean_file_to_read){
  #df_APM_ScM=foreach(i=1:length(files_to_read),.combine=rbind)%do%{
  foreach(i=1:length(clean_file_to_read))%do%{
      file_APM_ScM=read.table(paste0(path_clean,"/",clean_file_to_read[i]), header=TRUE, sep="\t",dec=".",fill=TRUE)
      l_v=compute_APM_ScoreMin(file_APM_ScM)
      file_to_save=data.frame(unlist(l_v[1]),unlist(l_v[2]),unlist(l_v[3]),unlist(l_v[4]),unlist(l_v[5]),unlist(l_v[6]))
      file_to_save$TotalScore=sum(file_APM_ScM$Point)
      #SubScores
      file_to_save$Flight=sum(file_APM_ScM$Point[file_APM_ScM$Group=="Flight"])
      file_to_save$Bonus=sum(file_APM_ScM$Point[file_APM_ScM$Group=="Bonus"])
      file_to_save$Mine=sum(file_APM_ScM$Point[file_APM_ScM$Group=="Mine"])
      file_to_save$Fortress=sum(file_APM_ScM$Point[file_APM_ScM$Group=="Fortress"])
      file_to_save$Pseudo=file_APM_ScM$Pseudo[1]
      file_to_save$Date=file_APM_ScM$Date[1]
      file_to_save$Session=file_APM_ScM$Session[1]
      colnames(file_to_save)=c("ScoresMin","FlightScore","BonusScore","MineScore","FortressScore","APM","TotalScore","Flight","Bonus","Mine","Fortress","Pseudo","Date","Session")
      write.table(file_to_save, paste0(path_ScM_APM,"ScM_APM_",clean_file_to_read[i]),append = FALSE, sep = "\t", dec = ".",row.names = TRUE, col.names = TRUE,quote=FALSE)
      #c(file_APM_ScM$Date[1],file_APM_ScM$Pseudo[1],l_v[1],l_v[2],sum(file_APM_ScM$Point),round(mean(unlist(l_v[1])),0))
  }
  return("write_APM_ScM done")
}

#read all txt file with APM and ScM
read_APM_ScM<-function(files_APM_ScM){
  df_APM_ScM=foreach(i=1:length(files_APM_ScM),.combine=rbind)%do%{
    file_APM_ScM=read.table(paste0(path_ScM_APM,files_APM_ScM[i]),header=TRUE,sep="\t")
    list(Date=file_APM_ScM$Date[1],Session=file_APM_ScM$Session[1],Pseudo=file_APM_ScM$Pseudo[1]
      ,ScoresMin=file_APM_ScM$ScoresMin,FlightScore=file_APM_ScM$FlightScore,BonusScore=file_APM_ScM$BonusScore
      ,MineScore=file_APM_ScM$MineScore,FortressScore=file_APM_ScM$FortressScore,APM=file_APM_ScM$APM,
      TotalScore=file_APM_ScM$TotalScore[1],Flight=file_APM_ScM$Flight[1],Bonus=file_APM_ScM$Bonus[1],Mine=file_APM_ScM$Mine[1]
      ,Fortress=file_APM_ScM$Fortress[1],MeanScore=round(mean(unlist(file_APM_ScM$ScoresMin)),0))  
    }
  #colnames(df_APM_ScM)=c("Date","Session","Pseudo","ScoresMin","FlightScore","BonusScore","MineScore","FortressScore","APM","TotalScore","Flight","Bonus","Mine","Fortress","MeanScore")
  #rownames(df_APM_ScM)=1:nrow(df_APM_ScM)
  df_APM_ScM=as.data.frame(df_APM_ScM)
  df_APM_ScM$Date=unlist(df_APM_ScM$Date)
  df_APM_ScM$Session=unlist(df_APM_ScM$Session)
  df_APM_ScM$Pseudo=unlist(df_APM_ScM$Pseudo)
  df_APM_ScM$TotalScore=unlist(df_APM_ScM$TotalScore)
  df_APM_ScM$Flight=unlist(df_APM_ScM$Flight)
  df_APM_ScM$Bonus=unlist(df_APM_ScM$Bonus)
  df_APM_ScM$Mine=unlist(df_APM_ScM$Mine)
  df_APM_ScM$Fortress=unlist(df_APM_ScM$Fortress)
  df_APM_ScM$MeanScore=unlist(df_APM_ScM$MeanScore)
  
  return(df_APM_ScM)
}

get_day_session<-function(file_to_date,path_to_date,c=FALSE){
  list_file=list.files(path=path_to_date,recursive = T) 
  sublist=list_file[lapply(list_file,function(x,clean=c) ID(x,clean=c)==ID(file_to_date,clean=c))==TRUE]
  sublist=sublist[order(date_hour(sublist,clean=c))]#order the sublist by date
  i=match(sublist[date_hour(sublist,clean=c)==date_hour(file_to_date,clean=c)],sublist)
  if(length(sublist)==11){
    return(switch(i,"D01P1","D02P1","D02P2","D03P1","D03P2","D04P1","D04P2","D05P1","D05P2","D14P1","D14P2"))
  }else{
    return(switch(i,"D01P1","D02","D03","D04","D05","D14"))
  }
}

#CONCATENATION OF ScM & AMP /PARTICIPANT
concatenate<-function(df_to_concatenate){
  concatenate_df=data.frame(matrix(ncol=ncol(df_to_concatenate),nrow=length(unique(df_to_concatenate$Pseudo))))
  colnames(concatenate_df)=colnames(df_to_concatenate)
  i=1
  for(str_pseudo in unique(df_to_concatenate$Pseudo)){
    subdf=subset(df_to_concatenate,df_to_concatenate$Pseudo==str_pseudo)
    subdf=subdf[order(as.POSIXct(as.character(subdf$Date))),] #reorder (if necessary) the df by date
    unique_ScM=subdf$ScoresMin[1]
    unique_ScFlight=subdf$FlightScore[1]
    unique_ScBonus=subdf$BonusScore[1]
    unique_ScMine=subdf$MineScore[1]
    unique_ScFortress=subdf$FortressScore[1]
    unique_APM=subdf$APM[1]
    #unique_RollSD=subdf$RollSD[1]
    for(j in 2:11){
      unique_ScM=mapply(c,unique_ScM,subdf$ScoresMin[j],SIMPLIFY = FALSE)
      unique_ScFlight=mapply(c,unique_ScFlight,subdf$FlightScore[j],SIMPLIFY = FALSE)
      unique_ScBonus=mapply(c,unique_ScBonus,subdf$BonusScore[j],SIMPLIFY = FALSE)
      unique_ScMine=mapply(c,unique_ScMine,subdf$MineScore[j],SIMPLIFY = FALSE)
      unique_ScFortress=mapply(c,unique_ScFortress,subdf$FortressScore[j],SIMPLIFY = FALSE)
      unique_APM=mapply(c,unique_APM,subdf$APM[j],SIMPLIFY = FALSE)
     # unique_RollSD=mapply(c,unique_RollSD,subdf$RollSD[j],SIMPLIFY = FALSE)
    }
    concatenate_df[i,]=subdf[1,]
    concatenate_df$ScoresMin[i]=unique_ScM
    concatenate_df$FlightScore[i]=unique_ScFlight
    concatenate_df$BonusScore[i]=unique_ScBonus
    concatenate_df$MineScore[i]=unique_ScMine
    concatenate_df$FortressScore[i]=unique_ScFortress
    concatenate_df$APM[i]=unique_APM
  #  concatenate_df$RollSD[i]=unique_RollSD
    i=i+1
  }
  #concatenate_df=subset(concatenate_df,select=c("Pseudo","ScoresMin","FlightScore","BonusScore","MineScore","FortressScore","APM","RollSD"))
  concatenate_df=subset(concatenate_df,select=c("Pseudo","ScoresMin","FlightScore","BonusScore","MineScore","FortressScore","APM"))
  return(concatenate_df)
}

read_data_score<-function(files_data,add_press=FALSE){
   df_data=foreach(i=1:length(files_data),.combine=rbind)%do%{
     file_d=read.table(paste0(path_clean,files_data[i]),header=TRUE,sep="\t")
     if(!add_press){
       file_d=subset(file_d,Type!="Press")
     }
     c(file_d$Date[1],file_d$Session[1],file_d$Pseudo[1],list(file_d$system_time),list(file_d$Point),list(file_d$Point[file_d$Group=="Flight"]),list(file_d$Point[file_d$Group=="Bonus"]),list(file_d$Point[file_d$Group=="Mine"]),list(file_d$Point[file_d$Group=="Fortress"]),list(as.numeric(cumsum(file_d$Point))))
   }
   #,list(cumsum(file_d$Point)),"CumScore"
  colnames(df_data)=c("Date","Session","Pseudo","System_time","Point","FlightPoint","BonusPoint","MinePoint","FortressPoint","CumulScore")
  rownames(df_data)=1:nrow(df_data)
  df_data=as.data.frame(df_data)
  return(df_data)
}