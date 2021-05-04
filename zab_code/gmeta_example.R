
source("FullOriginalGmetaCode.R")
# create small example meta-analysis data set
mdata <- cbind(TRT_event = c(1, 4, 2, 1),
               TRT_n = c(12, 42, 13, 24),
               CTRL_event = c(3, 5, 2, 2),
               CTRL_n = c(42, 24, 13, 24))
# run data set through CD combining method
cd.results <- gmeta(mdata, 
                    gmi.type = "2x2", 
                    method = "exact1", 
                    gmo.xgrid = seq(-6, 6, by = 0.001), 
                    report.error = TRUE)
# relevant results (you can use either the mean or median as the point estimate)
# (the point estimate and CI bounds are log odds ratios)
summary(cd.results)

data.frame(cd.results$combined.mean,
           cd.results$combined.median,
           cd.results$combined.sd,
           cd.results$combined.ci[1],
           cd.results$combined.ci[2],
           cd.results$pvalue)

# their version of a p-value function
plot(cd.results, plot.option = "cv")  # cv = confidence curve

# their version of a p-value function with individual studies' CVs plotted below
plot(cd.results, plot.option = "cv", studies = 1:4)
