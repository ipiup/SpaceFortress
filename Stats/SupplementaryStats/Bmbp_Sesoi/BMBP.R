# Bootstrap Magnitude Based Predictions (BMBP)
# Copyright 2018 Mladen Jovanovic
#
# To reference please use:
#
#      Jovanovic, M. (2018, May, 23). Bootstrap Magnitude Based Predictions. 
#          Complementary Training. Retrieved from http://www.complementarytraining.com/bmbp
#
#
require(dplyr)
require(ggplot2)
require(ggthemes)
require(gridExtra)
require(boot)
require(irr)
require(ggridges)
require(reshape2)

# Get bootstraped reliability estimates using 2 repeated measures 
bootReliability <- function(df,
                          var1,
                          var2,
                          xlabel = var1,
                          ylabel = var2,
                          SWCcoef = 0.2,
                          logTrans = FALSE, # Log transformation - next version
                          measurementError = 0, # Random error to add in boot samples
                          confidence = 0.95,
                          samples = 2000,
                          plot = TRUE,
                          iter = TRUE,
                          na.rm = TRUE) {
    # Plot    
    df$diff <- df[[var2]] - df[[var1]]
    df$avg <- (df[[var1]] + df[[var2]]) / 2
    sd.diff <- sd(df$diff, na.rm = na.rm)
    
    t.conf <- qt(1-((1-confidence)/2), df = length(na.omit(df$diff))-1) 
    
    # For SWC use pooled between Variable 1 and Variable 2
    N <- nrow(df)
    SWC <- sqrt(((var(df[[var1]], na.rm = na.rm)*(N-1)) + (var(df[[var2]], na.rm = na.rm)*(N-1))) /(2*(N-1))) * SWCcoef
    
    # Create graphs
        graphDF <- data.frame(x = df[[var1]], y = df[[var2]], diff = df$diff, avg = df$avg)
        plot1 <- ggplot(graphDF, aes(x = x, y = y)) +
            theme_bw() +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank()) +
            geom_point(alpha = 0.3) +  
            geom_smooth(method = "lm", se = FALSE) +
            geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "red") +
            xlab(xlabel) + 
            ylab(ylabel) + 
            theme(legend.position="none")
        
        plot2 <- ggplot(graphDF, aes(x = avg, y = diff)) + 
            annotate("rect", xmin = -Inf, xmax = Inf, ymin = -SWC, ymax = SWC, fill = "grey", alpha = 0.3) +
            geom_point(alpha = 0.3) +
            geom_smooth(method = "lm", se = FALSE) +
            theme_bw() +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank()) + 
            geom_hline(yintercept = mean(df$diff, na.rm = na.rm)) +
            geom_hline(yintercept = mean(df$diff, na.rm = na.rm) + t.conf*sd.diff, color = "grey") +
            geom_hline(yintercept = mean(df$diff, na.rm = na.rm) - t.conf*sd.diff, color = "grey") +
            ylab(paste(ylabel, " - ", xlabel, sep = "")) + 
            xlab(paste("(", xlabel, " + ", ylabel, ") / 2", sep = "")) +
            theme(legend.position="right")
        
        BAgraph <- arrangeGrob(plot1, plot2, ncol=2)

    # Bootstrap
    bs <- function(data, indices) {
        if(iter) cat(".")
        
        # Data for reliability
        df <- data[indices,] # allows boot to select sample 
        
        # Add random measurement error
        mError1 <- rnorm(n = nrow(df), sd = measurementError, mean = 0)
        mError2 <- rnorm(n = nrow(df), sd = measurementError, mean = 0)
        df[[var1]] <- df[[var1]] + mError1
        df[[var2]] <- df[[var2]] + mError2
        
        # Get the difference 
        df$diff <- df[[var2]] - df[[var1]]
        
        # Get the thresholds for LOA
        t.conf <- qt(1-((1-confidence)/2), df = length(na.omit(df$diff))-1) 

        ## Estimations
        SWC <- sqrt(((var(df[[var1]], na.rm = na.rm)*(N-1)) + (var(df[[var2]], na.rm = na.rm)*(N-1))) /(2*(N-1))) * SWCcoef
        sd.diff <- sd(df$diff, na.rm = na.rm)
        meanDiff <- mean(df$diff, na.rm = na.rm)
        medianDiff <- median(df$diff, na.rm = na.rm) + rnorm(n = 1, mean = 0, sd = 10^-6) # Small 'noise' for boot
        MADdiff <- mad(df$diff, na.rm = na.rm) + rnorm(n = 1, mean = 0, sd = 10^-6) # Small 'noise' for boot
        TE <- sd.diff / sqrt(2)
        LOA <- t.conf*sd.diff
        CORR <- cor(df[[var1]], df[[var2]], use = "complete.obs") # Correlation
        ICC <-  icc(data.frame(df[[var1]], df[[var2]]), 
                          "twoway", "agreement")$value # ICC
        # Get the hit rate 
        hit.rate <- ifelse(df$diff <= SWC & df$diff >= -SWC, 1, 0)
        hit.rate <- sum(hit.rate) / length(hit.rate)
        
        # Vector for saving results
        results <- numeric(13) 
        
        results[1] <- meanDiff
        results[2] <- sd.diff
        results[3] <- medianDiff
        results[4] <- MADdiff
        results[5] <- LOA
        results[6] <- TE
        results[7] <- CORR
        results[8] <- ICC
        results[9] <- SWC
        results[10] <- SWC / TE
        results[11] <- SWC / LOA
        results[12] <- SWC / MADdiff
        results[13] <- hit.rate

        return(results)
    }
    
    results <- boot(data = df, statistic = bs,
                    R = samples)
    
    meanDiff <- boot.ci(results, type="perc", index = 1, conf = confidence)
    SDdiff <- boot.ci(results, type="perc", index = 2, conf = confidence)
    medianDiff <- boot.ci(results, type="perc", index = 3, conf = confidence)
    MADdiff <- boot.ci(results, type="perc", index = 4, conf = confidence)
    LOA <- boot.ci(results, type="perc", index = 5, conf = confidence)
    TE <- boot.ci(results, type="perc", index = 6, conf = confidence)
    CORR <- boot.ci(results, type="perc", index = 7, conf = confidence)
    ICC <- boot.ci(results, type="perc", index = 8, conf = confidence)
    SWC <- boot.ci(results, type="perc", index = 9, conf = confidence)
    SWCtoTE <- boot.ci(results, type="perc", index = 10, conf = confidence)
    SWCtoLOA <- boot.ci(results, type="perc", index = 11, conf = confidence)
    SWCtoMAD <- boot.ci(results, type="perc", index = 12, conf = confidence)
    HitRate <- boot.ci(results, type="perc", index = 13, conf = confidence)
    
    estimates <- data.frame(meanDiff = c(meanDiff$t0, meanDiff$percent[4], meanDiff$percent[5]),
                            SDdiff = c(SDdiff$t0, SDdiff$percent[4], SDdiff$percent[5]),
                            medianDiff = c(medianDiff$t0, medianDiff$percent[4], medianDiff$percent[5]),
                            MADdiff = c(MADdiff$t0, MADdiff$percent[4], MADdiff$percent[5]),
                            LOA = c(LOA$t0, LOA$percent[4], LOA$percent[5]),
                            TE = c(TE$t0, TE$percent[4], TE$percent[5]),
                            CORR = c(CORR$t0, CORR$percent[4], CORR$percent[5]),
                            ICC = c(ICC$t0, ICC$percent[4], ICC$percent[5]),
                            SWC = c(SWC$t0, SWC$percent[4], SWC$percent[5]),
                            SWCtoTE = c(SWCtoTE$t0, SWCtoTE$percent[4], SWCtoTE$percent[5]),
                            SWCtoLOA = c(SWCtoLOA$t0, SWCtoLOA$percent[4], SWCtoLOA$percent[5]),
                            SWCtoMAD = c(SWCtoMAD$t0, SWCtoMAD$percent[4], SWCtoMAD$percent[5]),
                            HitRate = c(HitRate$t0, HitRate$percent[4], HitRate$percent[5]))
    
    estimates <- t(estimates) # Transpose
    row.names(estimates) <- NULL
    metrics <- c("meanDiff",
                 "SDdiff",
                 "medianDiff",
                 "MADdiff",
                 "LOA",
                 "TE",
                 "CORR",
                 "ICC",
                 "SWC",
                 "SWCtoTE",
                 "SWCtoLOA",
                 "SWCtoMAD",
                 "HitRate")
    estimates <- data.frame(Estimate = factor(metrics, levels = metrics),
                            estimates)
    colnames(estimates) <- c("Estimate", "Value", "Lower", "Upper")
    
    # Create graphs
        signalVSnoiseGraph <- ggplot(filter(estimates, Estimate %in% c("SWCtoTE", "SWCtoLOA", "SWCtoMAD", "HitRate")),
                                     aes(x = Value, y = Estimate)) +
                              theme_bw() +
                              theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                      panel.background = element_blank()) +
                              geom_errorbarh(aes(xmax = Upper, xmin = Lower), color = "black", height = 0.3) + 
                              geom_point(shape=21, size=1.5, fill="white") +
                              ylab("") + xlab("")
    if (plot == TRUE) {        
        plot(BAgraph)
        plot(signalVSnoiseGraph)
    }

    return(list(N = nrow(df),
                bootSamples = samples,
                measurementError = measurementError,
                confidence = confidence,
                SWCcoef = SWCcoef,
                logTrans = logTrans,
                estimates = estimates,
                BAgraph = BAgraph,
                signalVSnoiseGraph = signalVSnoiseGraph))
}

# Get bootstraped estimates of effects
bootChange <- function(df,
                      pre,
                      post,
                      xlabel = pre,
                      ylabel = post,
                      logTrans = FALSE, # Log transformation - next version
                      SWCcoef = 0.2,
                      confidence = 0.95,
                      samples = 2000,
                      TE = 0, # Typical Error
                      iter = TRUE,
                      na.rm = TRUE,
                      plot = TRUE) {
    
    
    # Bootstrap
    bs <- function(data, indices) {
        if(iter) cat(".")
        
        # Get the data
        df <- data[indices,] # allows boot to select sample 
        
        # Add Typical Error around data pre and post scores
        TypicalError1 <- rnorm(n = nrow(df), sd = TE, mean = 0)
        TypicalError2 <- rnorm(n = nrow(df), sd = TE, mean = 0)
        
        df[[pre]] <- df[[pre]] + TypicalError1
        df[[post]] <- df[[post]] + TypicalError2
        
        # Get the difference 
        df$diff <- df[[post]] - df[[pre]]
        
        ## Calculate the estimates
        # Estimate SWC from Pre
        SWC <- sd(df[[pre]], na.rm = na.rm) * SWCcoef
        N <- nrow(df)
        meanDiff <- mean(df$diff, na.rm = na.rm)
        # Get the MBIs for mean change
        sd.diff <- sd(df$diff, na.rm = na.rm)
        
        medianDiff <- median(df$diff, na.rm = na.rm) + rnorm(n = 1, mean = 0, sd = 10^-6) # Small 'noise' for boot
        MADdiff <- mad(df$diff, na.rm = na.rm) + rnorm(n = 1, mean = 0, sd = 10^-6) # Small 'noise' for boot
        
        # Estimate Cohen's D using pooled SD
        pooledSD <- sqrt(((var(df[[pre]], na.rm = na.rm)*(N-1)) + (var(df[[post]], na.rm = na.rm)*(N-1))) /(2*(N-1)))
        CohenD <- (mean(df[[post]], na.rm = na.rm) - mean(df[[pre]], na.rm = na.rm)) / pooledSD 
        
        # 'Novel metric' - robust efect
        robustEffect <- medianDiff / MADdiff 
        
        # Magnitude based predictions
        MBP.Harmful.Count <- sum(ifelse(df$diff < -SWC, 1, 0)) / N
        MBP.Beneficial.Count <- sum(ifelse(df$diff > SWC, 1, 0)) / N
        MBP.Trivial.Count <- sum(ifelse(df$diff >= -SWC & df$diff <= SWC, 1, 0)) / N
        
        # Get the mean harmful and beneficial effect
        MBP.Harmful.Mean <- mean(df$diff[df$diff < -SWC])
        MBP.Beneficial.Mean <- mean(df$diff[df$diff > SWC])
        
        # Get the median harmful and beneficial effect
        MBP.Harmful.Median <- median(df$diff[df$diff < -SWC])
        MBP.Beneficial.Median <- median(df$diff[df$diff > SWC])
        
        # Vector for saving results
        results <- numeric(10)     
        
        results[1] <- meanDiff
        results[2] <- sd.diff
        results[3] <- medianDiff
        results[4] <- MADdiff
        results[5] <- SWC
        results[6] <- CohenD
        results[7] <- robustEffect
        results[8] <- MBP.Harmful.Count
        results[9] <- MBP.Trivial.Count
        results[10] <- MBP.Beneficial.Count
        results[11] <- MBP.Harmful.Mean
        results[12] <- MBP.Beneficial.Mean 
        results[13] <- MBP.Harmful.Median
        results[14] <- MBP.Beneficial.Median
        
        return(results)
    }
    
    results <- boot(data = df, statistic = bs,
                    R = samples)
    
    meanDiff <- boot.ci(results, type="perc", index = 1, conf = confidence)
    
    # Get MBI for mean
    SWCpre <- sd(df[[pre]], na.rm = na.rm) * SWCcoef
    meanDiff.Harmful <- sum(ifelse(results$t[,1] < -SWCpre, 1, 0)) / samples
    meanDiff.Trivial <- sum(ifelse(results$t[,1] >= -SWCpre & results$t[,1] <= SWCpre, 1, 0)) / samples
    meanDiff.Beneficial <- sum(ifelse(results$t[,1] > SWCpre, 1, 0)) / samples
    
    SDdiff <- boot.ci(results, type="perc", index = 2, conf = confidence)
    
    # Get MBI for median
    medianDiff <- boot.ci(results, type="perc", index = 3, conf = confidence)
    medianDiff.Harmful <- sum(ifelse(results$t[,3] < -SWCpre, 1, 0)) / samples
    medianDiff.Trivial <- sum(ifelse(results$t[,3] >= -SWCpre & results$t[,3] <= SWCpre, 1, 0)) / samples
    medianDiff.Beneficial <- sum(ifelse(results$t[,3] > SWCpre, 1, 0)) / samples
    
    MADdiff <- boot.ci(results, type="perc", index = 4, conf = confidence)
    SWC <- boot.ci(results, type="perc", index = 5, conf = confidence)
    
    # Get MBI for Cohen's D
    CohenD <- boot.ci(results, type="perc", index = 6, conf = confidence)
    CohenD.Harmful <- sum(ifelse(results$t[,6] < -SWCcoef, 1, 0)) / samples
    CohenD.Trivial <- sum(ifelse(results$t[,6] >= -SWCcoef & results$t[,6] <= SWCcoef, 1, 0)) / samples
    CohenD.Beneficial <- sum(ifelse(results$t[,6] > SWCcoef, 1, 0)) / samples
    
    robustEffect <- boot.ci(results, type="perc", index = 7, conf = confidence)
    
    # MBP
    MBP.Harmful.Count <- boot.ci(results, type="perc", index = 8, conf = confidence)
    MBP.Trivial.Count <- boot.ci(results, type="perc", index = 9, conf = confidence)
    MBP.Beneficial.Count <- boot.ci(results, type="perc", index = 10, conf = confidence)
    MBP.Harmful.Mean <- boot.ci(results, type="perc", index = 11, conf = confidence)
    MBP.Beneficial.Mean <- boot.ci(results, type="perc", index = 12, conf = confidence) 
    MBP.Harmful.Median <- boot.ci(results, type="perc", index = 13, conf = confidence)
    MBP.Beneficial.Median <- boot.ci(results, type="perc", index = 14, conf = confidence)  
    
    changeEstimate <- data.frame(meanDiff = c(meanDiff$t0, meanDiff$percent[4], meanDiff$percent[5]),
                                 meanDiff.Harmful = c(meanDiff.Harmful, NA, NA),
                                 meanDiff.Trivial = c(meanDiff.Trivial, NA, NA),
                                 meanDiff.Beneficial = c(meanDiff.Beneficial, NA, NA),
                                 
                                 SDdiff = c(SDdiff$t0, SDdiff$percent[4], SDdiff$percent[5]),
                                 
                                 medianDiff = c(medianDiff$t0, medianDiff$percent[4], medianDiff$percent[5]),
                                 medianDiff.Harmful = c(medianDiff.Harmful, NA, NA),
                                 medianDiff.Trivial = c(medianDiff.Trivial, NA, NA),
                                 medianDiff.Beneficial = c(medianDiff.Beneficial, NA, NA),
                                 
                                 MADdiff = c(MADdiff$t0, MADdiff$percent[4], MADdiff$percent[5]),
                                 SWC = c(SWC$t0, SWC$percent[4], SWC$percent[5]),
                                 
                                 CohenD = c(CohenD$t0, CohenD$percent[4], CohenD$percent[5]),
                                 CohenD.Harmful = c(CohenD.Harmful, NA, NA),
                                 CohenD.Trivial = c(CohenD.Trivial, NA, NA),
                                 CohenD.Beneficial = c(CohenD.Beneficial, NA, NA),
                                 
                                 robustEffect = c(robustEffect$t0, robustEffect$percent[4], robustEffect$percent[5]),
                                 
                                 MBP.Harmful.Count = c(MBP.Harmful.Count$t0, MBP.Harmful.Count$percent[4], MBP.Harmful.Count$percent[5]),
                                 MBP.Trivial.Count = c(MBP.Trivial.Count$t0, MBP.Trivial.Count$percent[4], MBP.Trivial.Count$percent[5]),
                                 MBP.Beneficial.Count = c(MBP.Beneficial.Count$t0, MBP.Beneficial.Count$percent[4], MBP.Beneficial.Count$percent[5]),
                                 MBP.Harmful.Mean = c(MBP.Harmful.Mean$t0, MBP.Harmful.Mean$percent[4], MBP.Harmful.Mean$percent[5]),
                                 MBP.Beneficial.Mean = c(MBP.Beneficial.Mean$t0, MBP.Beneficial.Mean$percent[4], MBP.Beneficial.Mean$percent[5]), 
                                 MBP.Harmful.Median = c(MBP.Harmful.Median$t0, MBP.Harmful.Median$percent[4], MBP.Harmful.Median$percent[5]),
                                 MBP.Beneficial.Median = c(MBP.Beneficial.Median$t0, MBP.Beneficial.Median$percent[4], MBP.Beneficial.Median$percent[5]))
    changeEstimate <- t(changeEstimate)
    changeEstimate <- data.frame(Estimate = factor(row.names(changeEstimate),
                                                   levels = row.names(changeEstimate)),
                                 changeEstimate)
    colnames(changeEstimate) <- c("Estimate", "Value", "Lower", "Upper")
    row.names(changeEstimate) <- NULL
    
    # Create graphs
        plotDF <- changeEstimate %>%
            filter(Estimate %in% c("MBP.Harmful.Count",
                                   "MBP.Trivial.Count",
                                   "MBP.Beneficial.Count"))
        plotDF$Magnitude <- factor(c("Harmful", "Trivial", "Beneficial"),
                                   levels = c("Harmful", "Trivial", "Beneficial"))
        plotDF$Effect <- changeEstimate[13:15, 2]
        
        gg <- ggplot(plotDF, aes(y = Magnitude, x = Value)) +
            theme_bw() + 
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank()) +
            geom_errorbarh(aes(xmax = Upper, xmin = Lower), color = "black", height = 0.05) + 
            ylab("") + 
            xlab("Probability") +
            geom_point(shape=21, size=3, fill="white")+
            geom_point(aes(x = Effect), color = "red", shape = "|", size = 5)
        
        effectsGraph <- gg
        if (plot == TRUE) plot(gg)
        
        plotDF <- changeEstimate %>%
            filter(Estimate %in% c("meanDiff",
                                   "medianDiff",
                                   "MBP.Harmful.Mean",
                                   "MBP.Beneficial.Mean",
                                   "MBP.Harmful.Median",
                                   "MBP.Beneficial.Median"))
        
        plotDF$Change <- factor(c("Change Mean",
                                  "Change Median",
                                  "Harmful Mean",
                                  "Beneficial Mean",
                                  "Harmful Median",
                                  "Beneficial Median"),
                                levels = c(
                                    "Harmful Median",
                                    "Harmful Mean",
                                    "Beneficial Median",
                                    "Beneficial Mean",
                                    "Change Median",
                                    "Change Mean"))
        
        gg <- ggplot(plotDF, aes(y = Change, x = Value)) +
            theme_bw() + 
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                  panel.background = element_blank()) +
            geom_vline(xintercept = 0, color = "grey") +
            geom_vline(xintercept = -SWCpre, color = "grey", linetype = "dashed") +
            geom_vline(xintercept = SWCpre, color = "grey", linetype = "dashed") +
            geom_errorbarh(aes(xmax = Upper, xmin = Lower), color = "black", height = 0.1) + 
            ylab("") + 
            xlab("Change") +
            geom_point(shape=21, size=3, fill="white")
        changeGraph <- gg
        if (plot == TRUE) plot(gg)

    return(list(N = nrow(df),
                bootSamples = samples,
                TE = TE,
                confidence = confidence,
                SWCcoef = SWCcoef,
                logTrans = logTrans,
                changeEstimate = changeEstimate,
                effectsGraph = effectsGraph,
                changeGraph = changeGraph))
}

