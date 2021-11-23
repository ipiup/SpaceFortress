#Reaction Time, plot TEA par thâche avec tous les participants

path="E:/SpaceFortress/SCED/BDD_TEA.xlsx" #path du fichier xlsx

#fonction pour un plot
sced_RT_plot<-function(path,sheetnb){
  data=readXL(path, rownames=FALSE, sheet=sheetnb,header=TRUE, stringsAsFactors=TRUE)
  p_sced_RT=ggplot(data,aes(time,RT,color=Patient,shape=Patient))+theme_pubr()+
    geom_point()+geom_line(aes(group=Patient),show.legend=FALSE)+
    ylab("Response Time")+xlab("Evaluation")+ggtitle(excel_sheets(path)[sheetnb])+
    theme(plot.title = element_text(hjust = 0.5))
  return(p_sced_RT)
}

#les 4 plots (un par tâche)
p_01=sced_RT_plot(path,1)
p_02=sced_RT_plot(path,2)
p_03=sced_RT_plot(path,3)
p_04=sced_RT_plot(path,4)

#création de la figure avec 2 plots par ligne et 2 par colonne
ggarrange(p_01,p_02,p_03,p_04,nrow=2,ncol=2,common.legend = TRUE)

#enregistrement de la figure, format 8*6
ggsave("E:/SpaceFortress/SCED/Plots/TEA_Results.pdf",device="pdf",width=8,height=6) 


