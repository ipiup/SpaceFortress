# Bootstrap Magnitude Based Predictions (BMBP)
# Copyright 2018 Mladen Jovanovic
#
# To reference plese use:
#
#      Jovanovic, M. (2018, May, 23). Bootstrap Magnitude Based Predictions. 
#          Complementary Training. Retrieved from http://www.complementarytraining.com/bmbp
#
#

# Load bootstrap magnitude based prediction functions
source('BMBP.R')

set.seed(667)

# Generate data
N = 25
athletes <- sprintf("Athlete_%03d", seq(1, N))
verticalJumpReal <- rnorm(n = N, mean = 40, sd = 8)

TypicalError = 1
verticalJumpTestPre1 <- verticalJumpReal + rnorm(n = N, mean = 0, sd = TypicalError)  
verticalJumpTestPre2 <- verticalJumpReal + rnorm(n = N, mean = 0, sd = TypicalError) 

df <- data.frame(Athlete = athletes,
                 Pre1 = verticalJumpTestPre1,
                 Pre2 = verticalJumpTestPre2)

## RELIABILITY
bootVJ <- bootReliability(df = df, var1 = "Pre1", var2 = "Pre2",
                          plot = TRUE, samples = 1000, measurementError = 0, iter = TRUE)
bootVJ$estimates
############################################


## Generate change after intervention
verticalJumpTestPost <- verticalJumpReal + sample(c(5, -5), N, replace = TRUE)
df$Post <- verticalJumpTestPost
df$diff <- df$Post - df$Pre1


# Plot Individual change
estTE <- bootVJ$estimates$Value[6]
estSWC <- bootVJ$estimates$Value[9]
t.conf <- qt(1-((1-0.95)/2), df = length(na.omit(df$diff))-1) 

df$lower <- df$diff - estTE*t.conf
df$upper <- df$diff + estTE*t.conf
df$y <- runif(n = nrow(df), 0, 1)

gg <- ggplot(df, aes(y = y, x = diff)) +
    theme_bw() + 
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank()) +
    annotate("rect", xmin = -estSWC, xmax = estSWC, ymin = -Inf, ymax = Inf, alpha = 0.3) +
    geom_errorbarh(aes(xmax = upper, xmin = lower), color = "black", height = 0.05) + 
    ylab("") + 
    xlab("Change in Vertical Jump Height") +
    theme(axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())+
    geom_point(shape=21, size=3, fill="white")
gg

# Check the changes
change1 <- bootChange(df = df, pre = "Pre1", post = "Post",
                      samples = 1000, SWCcoef = 0.2, TE = 0, plot = FALSE)
change1

change2 <- bootChange(df = df, pre = "Pre1", post = "Post",
                      samples = 1000, SWCcoef = 0.2, TE = estTE, plot = FALSE)
change2

# Check what MBIR says
library(mbir)

smd_test(x = df$Post, y = df$Pre1, paired = TRUE, conf.int = 0.95, swc = 0.2, plot = TRUE)
