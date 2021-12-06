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
