#SESOI on tRNS pre post data
require(bmbstats)
require(tidyverse)
require(cowplot)
require(ggridges)
require(kableExtra)
library(ggpubr)
library(ggplot2)

dataJ14=read.csv("E:/SpaceFortress/Stats/SupplementaryStats/Data_wide_PrePostJ14.csv",sep=";")
dataJ5=read.csv("E:/SpaceFortress/Stats/SupplementaryStats/Data_wide_PrePostJ5.csv",sep=";")
data=dataJ14
colnames(data)=c("Pseudo","Participant","Group","Pre-test","Post-test","Change")

typical_error <- 2.5
SESOI_lower <- -5
SESOI_upper <- 5

data<- data %>%
  mutate(
    Upper = Change + 2.09 * sqrt(2) * typical_error,
    Lower = Change - 2.09 * sqrt(2) * typical_error,
    `higher %` = round(100 * (1 - pt((SESOI_upper - Change) / (sqrt(2) * typical_error), df = 19)), 0),
    `lower %` = round(100 * pt((SESOI_lower - Change) / (sqrt(2) * typical_error), df = 19), 0),
    `equivalent %` = round(100 - (`higher %` + `lower %`), 0),
    label = paste("  [", `lower %`, "/", `equivalent %`, "/", `higher %`, "]", sep = "")
  )

data$Pseudo<- reorder(data$Pseudo, (data$Change))

measurement_error_data <- data.frame(
  Pseudo = numeric(0),
  Change = numeric(0),
  sample = numeric(0)
)

for (i in seq(1, nrow(data))) {
  measurement_error_data <- rbind(
    measurement_error_data,
    data.frame(
      Pseudo = data$Pseudo[i],
      Change = data$Change[i],
      sample = perfect_rnorm(
        n = 100,
        mean = data$Change[i],
        sd = typical_error * sqrt(2)
      )
    )
  )
}

couleurs=c("#868686FF","#0073C2FF","#A73030FF")
couleurs_alpha=c("#86868666","#0073C266","#A7303099")
data$Group=as.factor(data$Group)
data$Group=factor(data$Group,levels=c("SHAM","STIMSD","STIMHD"))

mean_sd=data.frame(
  Group=c("SHAM","STIMSD","STIMHD"),
  Mean=c(mean(data$Change[data$Group=="SHAM"]),mean(data$Change[data$Group=="STIMSD"]),mean(data$Change[data$Group=="STIMHD"])),
  SD=c(sd(data$Change[data$Group=="SHAM"]),sd(data$Change[data$Group=="STIMSD"]),sd(data$Change[data$Group=="STIMHD"]))
)
mean_sd$Group=factor(mean_sd$Group,levels=c("SHAM","STIMSD","STIMHD"))
gg_individual <- ggplot(data, aes(y = Pseudo, x = Change,color=Group,Group=Group)) +
  theme_cowplot(8) +
  geom_errorbarh(aes(xmax = Upper, xmin = Lower), height = 0) +
  ylab(NULL) +
  xlab("Delta Performance (GS11 - GS1)") +
  theme(legend.position = "none")+
  facet_grid(Group~.,scales="free")+
  geom_vline(data = mean_sd,aes(group=Group,xintercept=Mean,color=Group)) +
  geom_rect(data=mean_sd,aes(group=Group,xmin=Mean-SD,xmax=Mean+SD,ymin=-Inf,ymax=+Inf,fill=Group),inherit.aes=FALSE,alpha=0.2)+
  geom_point() +
  scale_colour_manual(values=couleurs)+scale_fill_manual(values=couleurs_alpha)
gg_individual

