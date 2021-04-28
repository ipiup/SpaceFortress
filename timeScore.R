for(i in 1:length(fil)){print(i)
vectors_fct( session_file=read.table(paste0(path,"/",fil[i]), header=TRUE, sep="",dec=".",fill=TRUE))}
#Time frame scores

#required library
library(doParallel)
library(foreach)

source("Functions.R") #calls all functions written in functions.R
#source("Chronologie_Keys.R")

scores_apm_sessions_fct<-function(fil,session_nb=TRUE){
  if(session_nb==TRUE){
    df_=scores_apm_fct(fil,1,path)
  }else if(session_nb==FALSE){
    #files separation (in two sessons)
    J1_files=fil[1:(length(fil)/6)] #J1 files, no double session
    df_J1=scores_apm_fct(J1_files,1,path)
    #rest of the files with double session
    files_rest=fil[((length(fil)/6)+1):length(fil)]
    df_rest=scores_apm_fct(files_rest,2,path)
    df_=rbind(df_J1,df_rest)
  }
  return(df_)
}

scores_apm_fct <- function(fil,session,path=path){ # return the list of scores on a time frame for 1 txt file ; session : TRUE for Toulouse FALSE for Montpellier

  head_df=c("Date","Pseudo","Scores","APM")
  df_scores_apm=data.frame(matrix(vector(),nrow=(session*length(fil)),ncol=length(head_df)),stringsAsFactors=FALSE)
  colnames(df_scores_apm)=head_df
  
  if(session==1){
    print("ok")
    # cl=parallel::makeCluster(detectCores())
    # registerDoParallel(cl)
    # parallel_df=foreach(i=1:length(fil),.combine=rbind)%dopar%{
    #   source("Functions.R")
    #   id=ID(fil[i])
    #   dat= as.character(date(fil[i]))
    #   session_file=read.table(paste0(path,"/",fil[i]), header=TRUE, sep="",dec=".",fill=TRUE)
    #   l_v=vectors_fct(session_file)
    #   c(id,dat,l_v[1],l_v[2])
    # }
    #parallel::stopCluster(cl)
    #colnames(parallel_df)=head_df
    #df_scores_apm=as.data.frame(parallel_df)
    for(i in 1:length(fil)){
      id=ID(fil[i])
      dat= as.character(date(fil[i]))
      session_file=read.table(paste0(path,"/",fil[i]), header=TRUE, sep="",dec=".",fill=TRUE)
      l_v=vectors_fct(session_file)
      df_scores_apm[i,]=c(dat,id)
      df_scores_apm$Scores[i]=l_v[1]
      df_scores_apm$APM[i]=l_v[2]
      print(paste0(i,"/",length(fil)))
    }
  }else if (session==2){
    j=1
    for(i in 1:length(fil)){
      data=read.table(paste0(path,"/",fil[i]), header=TRUE, sep="",dec=".",fill=TRUE)
      #data=data[!(data$event_type=="n"),] #clean data
      data1=data.frame(split_data(data)[1]) #First Sesssion for the day
      data2=data.frame(split_data(data)[2]) #Second Session for the day
      id=ID(fil[i])
      dat= as.character(date(fil[i]))
      
      l_v_1=vectors_fct(data1)
      df_scores_apm[j,]=c(dat,id)
      df_scores_apm$Scores[j]=l_v_1[1]
      df_scores_apm$APM[j]=l_v_1[2]
      
      l_v_2=vectors_fct(data2)
      df_scores_apm[j+1,]=c(dat,id)
      df_scores_apm$Scores[j+1]=l_v_2[1]
      df_scores_apm$APM[j+1]=l_v_2[2]
      
      print(paste0(j,"/",2*length(fil)))
      j=j+2
    }
  }
  return(df_scores_apm)
}


path=choose.dir(default = "", caption = "Participants file")
fil=list.files(path=path,recursive = T) # files pattern : "^SpaceFortress-5.1.0(.*).txt$")
session_nb=as.logical(winDialog(type = c( "yesno"), message="Files contain 1 sessions per file? \n (Yes:Toulouse, No:Montpellier)")=="YES")
df=scores_apm_sessions_fct(fil,session_nb)

cl=parallel::makeCluster(detectCores())
registerDoParallel(cl)

a=invisible(foreach(i=1:length(fil),.combine=rbind)%dopar%{
  source("Functions.R")
  id=ID(fil[i])
  dat= as.character(date(fil[i]))
  session_file=read.table(paste0(path,"/",fil[i]), header=TRUE, sep="",dec=".",fill=TRUE)
  l_v=vectors_fct(session_file)
  c(id,dat,l_v[1],l_v[2])
})
stopCluster(cl)

colnames(a)=head_df
df_scores_apm=as.data.frame(a)

df2=date_change(df,FALSE,FALSE)
df$Date=df2$Date


