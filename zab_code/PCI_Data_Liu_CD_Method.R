
pci <- read.csv("PCI vs MED Dataset - Cardiac Death Only.csv", header = TRUE)
head(pci)

################################################################################
# Liu et al. 
################################################################################
set.seed(1234)
source("0_FullOriginalGmetaCode.R")

mdata <- cbind(pci$PCI_Death,
               pci$PCI_Total, 
               pci$MED_Death,
               pci$MED_Total)

cd.results <- gmeta(mdata, 
                    gmi.type = "2x2", 
                    method = "exact1", 
                    gmo.xgrid = seq(-6, 6, by = 0.001), 
                    report.error = TRUE)
summary(cd.results)
# Summary of Combined CD:
#                   mean     median    stddev   ci.lower    ci.upper
# Combined CD -0.3060972 -0.3061018 0.1333215 -0.5690538 -0.04607136
# *Using the median for the estimate
cd.results$pvalue  # 0.02103026
exp(c(cd.results$combined.median, cd.results$combined.ci)) 
# 0.7363117 0.5660608 0.9549738

################################################################################
# Plotting
################################################################################
library(tidyverse)

cmbd.cvs.exact1 <- 1 - 2 * abs(cd.results$combined.cd - 0.5)
anti.liu.et.al <- as.data.frame(cbind("logOR" = cd.results$x.grids, 
                                      "pVal2Sided" = cmbd.cvs.exact1))
anti.liu.et.al$Method <- rep("Liu et al.", length(anti.liu.et.al$logOR))

sz <- 1
sz.line <- 1.3
sz.text <- 18

ggplot() + 
  geom_line(data = anti.liu.et.al, 
            mapping = aes(x = logOR, 
                          y = pVal2Sided),
            size = sz.line) +
  geom_hline(aes(yintercept = 0.05)) +
  geom_vline(aes(xintercept = 0), linetype = "longdash") +
  theme_bw() +
  scale_x_continuous(name = "Log Odds Ratio",
                     minor_breaks = seq(-1.5, 0.5, 0.25),
                     breaks = seq(-1.5, 0.5, 0.5),
                     labels = seq(-1.5, 0.5, 0.5),
                     limits = c(-1.5, 0.5)) +
  scale_y_continuous(name = "Two-Sided p-value",
                     minor_breaks = seq(0, 1, by = 0.1),
                     breaks = seq(0, 1, by = 0.2),
                     labels = seq(0, 1, by = 0.2),
                     limits = c(0, 1),
                     sec.axis = sec_axis(trans = ~ (1 - .) * 100, 
                                         name = "Confidence Level",
                                         breaks = seq(100, 0, by = -20),
                                         labels = seq(100, 0, by = -20))) +
  coord_cartesian(clip = "off")

