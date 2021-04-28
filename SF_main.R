#Data extraction : from raw text file to main information (txt as well)
#Warning: windialog requires a windows OS

path=choose.dir(default = "", caption = "Select Participants folder")
files=list.files(path=path,recursive = T) # files pattern : "^SpaceFortress-5.1.0(.*).txt$")
source("Functions.R") #calls all functions written in functions.R

#NUMBER OF SESSION
session_nb=as.logical(winDialog(type = c( "yesno"), message="Files contain 2 sessions per file? \n (Yes:Toulouse, No:Montpellier)")=="YES")
supplementary=as.logical(winDialog(type = c( "yesno"), message="Do you wish supplementary information? \n (details on game)")=="YES")
data_list=sessions(files,session_nb)
leader=as.data.frame(data_list[1])
if(supplementary==TRUE){
  supplementary_data=as.data.frame(data_list[2])
}

#CHOICE ABOUT MEAN BTW SESSIONS & ABOUT BASELINE (J1)
mean_day=as.logical(winDialog(type = c( "yesno"), message="Mean value between days?\n (If NO: 2 scores/day)")=="YES")
delta=as.logical(winDialog(type = c( "yesno"), message="Delta ? \n First day as baseline subtracted. ")=="YES")

#DATE
leader=date_change(leader=leader,mean=mean_day,base=delta)
if(supplementary==TRUE){
  supplementary_data=date_change(leader=supplementary_data,mean=mean_day,base=delta)
}

#GROUPS (DOUBLE BLIND OR NOT)
double_blinded=as.logical(winDialog(type = c( "yesno"), message="Show SHAM and STIM?\n (If NO: groups: 1 or 2)")=="YES")
winDialog(type=c("ok"),message="Choose the groups information data \n Select GROUPS csv File\n(df_leaderbord.csv)")
groups=read.csv(  choose.files(default = "", caption = "Select GROUPS csv File\n(df_leaderbord.csv)"),header=T,sep=",",fill=T)
leader=db_blinded(leader,double_blinded,groups)
if(supplementary==TRUE){
  supplementary_data=db_blinded(leader=supplementary_data,double_blinded,groups)
}
#DATA AS TXT FILE
data_name=winDialogString(message="File name",default = "SF_data_Info_Mean_Baseline.txt")
data_name_csv=winDialogString(message="CSV File name",default = "SF_data_Info_Mean_Baseline.csv")
write.table(leader,data_name,sep="\t",row.names = T,quote=F)
write.csv(leader,data_name_csv)
if(supplementary==TRUE){
  data_supp_name=winDialogString(message="File name",default = "SF_SUPPLEMENTARY_data_Info_Mean_Baseline.txt")
  data_supp_name_csv=winDialogString(message="File name",default = "SF_SUPPLEMENTARY_data_Info_Mean_Baseline.csv")
  write.table(supplementary_data,data_supp_name,sep="\t",row.names = T,quote=F)
  write.csv(supplementary_data,data_supp_name_csv)
}

#COMBINE TLS AND MTP files:

data_MTP=read.csv("SF_data_Info_Mean_MONTPELLIER.csv",row.names = 1)
data_TLS=read.csv("SF_data_Info_Mean_TOULOUSE.csv",row.names = 1)
data_ALL=rbind(data_MTP,data_TLS)
write.csv(data_ALL,"DATATRNS.csv",quote=F)
write.table(data_ALL,"DATATRNS.txt",sep="\t",row.names = T,quote=F)
