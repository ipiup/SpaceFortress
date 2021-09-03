#SESOI on tRNS pre post data

library(ggpubr)
dataJ14=read.csv("E:/SpaceFortress/Data_wide_PrePostJ14.csv",sep=";")
dataJ5=read.csv("E:/SpaceFortress/Data_wide_PrePostJ5.csv",sep=";")
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


gg_meas_err <- ggplot(measurement_error_data, aes(y = Pseudo, x = sample)) +
  theme_cowplot(8) +
  annotate("rect", xmin = SESOI_lower, xmax = SESOI_upper, ymin = -Inf, ymax = Inf, alpha = 0.2, fill = "lightgrey") +
  geom_vline(xintercept = 0, color = "dark grey") +
  geom_density_ridges_gradient(
    aes(
      fill = ifelse(..x.. > SESOI_upper, "Higher", ifelse(..x.. < SESOI_lower, "Lower", "Equivalent")),
      group = Pseudo
    ),
    color = NA, vline_color = "white",
    scale = 2
  ) +
  ylab(NULL) +
  xlab("Change in Bench Press 1RM (kg)") +
  theme(legend.position = "none") +
  #xlim(-30, 40) +
  scale_fill_manual(values = c(user_grey_transparent, user_green_transparent, user_red_transparent)) +
  scale_discrete_manual("point_color", values = c(user_green_transparent, user_red_transparent, user_grey_transparent))
gg_meas_err

mean_Sham=mean(data$Change[data$Group=="SHAM"])
sd_Sham=sd(data$Change[data$Group=="SHAM"])

mean_StimHD=mean(data$Change[data$Group=="STIMHD"])
sd_StimHD=sd(data$Change[data$Group=="STIMHD"])

mean_StimSD=mean(data$Change[data$Group=="STIMSD"])
sd_StimSD=sd(data$Change[data$Group=="STIMSD"])

couleurs=c("#868686FF","#0073C2FF","#A73030FF")
couleurs_alpha=c("#86868666","#0073C266","#A7303099")
data$Group=as.factr(data$Group)
data$Group=factor(data$Group,levels=c("SHAM","STIMSD","STIMHD"))

mean_sd=data.frame(
  Group=c("SHAM","STIMSD","STIMHD"),
  Mean=c(mean_Sham,mean_StimSD,mean_StimHD),
  SD=c(sd_Sham,sd_StimSD,sd_StimHD)
)
mean_sd$Group=factor(mean_sd$Group,levels=c("SHAM","STIMSD","STIMHD"))
gg_individual <- ggplot(data, aes(y = Pseudo, x = Change,color=Group,Group=Group)) +
  theme_cowplot(8) +
  geom_errorbarh(aes(xmax = Upper, xmin = Lower), height = 0) +
  ylab(NULL) +
  xlab("Delta Performance (GS11 - GS1)") +
  theme(legend.position = "none")+
  facet_grid(factor(Group,levels=c("SHAM","STIMSD","STIMHD"))~.,scales="free")+
  geom_vline(data = mean_sd,aes(group=Group,xintercept=Mean,color=Group)) +
  geom_rect(data=mean_sd,aes(group=Group,xmin=Mean-SD,xmax=Mean+SD,ymin=-Inf,ymax=+Inf,fill=Group),inherit.aes=FALSE,alpha=0.2)+
  geom_point() +
  scale_colour_manual(values=couleurs)+scale_fill_manual(values=couleurs_alpha)
gg_individual

  geom_rect(data = data.frame(Group = "SHAM"), aes(xmin = mean_Sham-sd_Sham, xmax = mean_Sham+sd_Sham, ymin = -Inf, ymax = Inf), alpha = 0.1, fill="#86868666", inherit.aes = FALSE)+
  geom_rect(data = data.frame(Group = "STIMSD"), aes(xmin = mean_StimSD-sd_StimSD, xmax = mean_StimSD+sd_StimSD, ymin = -Inf, ymax = Inf), alpha = 0.1, fill="#0073C266", inherit.aes = FALSE)+
  geom_rect(data = data.frame(Group = "STIMHD"), aes(xmin = mean_StimHD-sd_StimHD, xmax = mean_StimHD+sd_StimHD, ymin = -Inf, ymax = Inf), alpha = 0.1, fill="#A7303099", inherit.aes = FALSE)


