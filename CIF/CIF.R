#CIF Bubble tree with weigthed nodess


# create data:
links=data.frame(
  source=c("A","A", "A", "A", "A","J", "B", "B", "C", "C", "D","I"),
  target=c("B","B", "C", "D", "J","A","E", "F", "G", "H", "I","I")
)

# Turn it into igraph object
network <- graph_from_data_frame(d=links, directed=F) 

# Count the number of degree for each node:
deg <- degree(network, mode="all")

# Plot
plot(network, vertex.size=deg*6, vertex.color=rgb(0.1,0.7,0.8,0.5) )


# library
library(igraph)
library("readxl")
library("ggplot2")
library(tidyr)
library(dplyr)
library(readr)
data=read_excel("E:\\SpaceFortress\\CIF\\CIF_Data.xlsx",sheet="AP",col_names=FALSE)


#weight = number of iteration
network=graph_from_data_frame(d=data,directed=F)
deg=degree(network)
dist=deg;dist[dist!="D1"]=1.5
dist["B1"]=0;dist["D1"]=0;dist["D2"]=0;dist["D3"]=0;dist["D4"]=0;dist["D5"]=0;dist["D6"]=0;dist["D7"]=0;dist["D8"]=0;dist["D9"]=0
data_unique=unique(data)
data_unique=subset(data_unique,data_unique[1]!=data_unique[2])

network=graph_from_data_frame(d=data_unique,directed=F)

plot.igraph(network,edge.width=1.3,vertex.size=deg*1.5,
            vertex.label.cex=0.4,vertex.color=rgb(0.2,0.6,0.8,0.5),
            vertex.label.dist=dist,vertex.label.color="black",
            vertex.shape="circle",margin=-0.2,layout=layout.kamada.kawai) #layout.auto


#####
data=read_excel("E:\\SpaceFortress\\CIF\\CIF_Data.xlsx",sheet="All")
data=data[,-1] #First column

data= data[rowSums(is.na(data)) != ncol(data), ] #completely NA rows

data =t(data)
data[,1]=toupper(substring(data[,1],1,2))
data=as.data.frame(data)

data=data%>%
  pivot_longer(!V1)
data=data[,-2]
data=na.omit(data)
colnames(data)=c("Source","Target")

# split the contents by ";"
x <- strsplit(as.character(data$Target), ";", fixed = T)
# add new rows with each content:
data <- cbind(data[rep(1:nrow(data), lengths(x)), 1:2], content = unlist(x))
data=data[,-2]
colnames(data)=c("Source","Target")
data[,2]=str_replace_all(data[,2]," ","") #remove whitespaces


for( DB in unique(data$Source)){
  data_DB = data[data$Source==DB,]
  class=unique(data_DB[grepl("-",data_DB[,2]),2])
  n_DB=unique(data_DB[nchar(data_DB[,2])==4,2])
  for(x in n_DB){
    for(c in class_D6){
      c1=parse_number(str_split(c,"-")[[1]][1])
      c2=parse_number(str_split(c,"-")[[1]][2])
      n=parse_number(x)
      if(n>=c1 && n<=c2){
        data$Source[data$Target==x]=c
      }
    }
  }
}
data_D6=data[data$Source=="D6",]
class_D6=unique(data_D6[grepl("-",data_D6[,2]),2])
n_D6=unique(data_D6[nchar(data_D6[,2])==4,2])

for(x in n_D6){
  for(c in class_D6){
    c1=parse_number(str_split(c,"-")[[1]][1])
    c2=parse_number(str_split(c,"-")[[1]][2])
    n=parse_number(x)
    if(n>=c1 && n<=c2){
      data_D6$Source[data_D6$Target==x]=c
    }
  }
}
