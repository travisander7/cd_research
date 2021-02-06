
# Note that this code is pretty old, and is not pretty:)
# I originally had this in a loop

library(vegan)  # generates tables with row & column totals matching our table
library(tidyverse)

# example data:
treatment <- c(0, 0, 0, 0,  1, 1, 1, 1)
outcome <- c(1, 0, 1, 0,   0, 1, 0, 0,
             1, 3, 1, 2,   2, 3, 2, 3)

mydat <- list(matrix(outcome, nrow = 2, byrow = TRUE,
                     dimnames = list(Out = c(1, 0), TRT = treatment)))
mydat

# calculate the observed sufficient statistic for beta (the test statistic t in
# my dissertation)
sum.xiyi <- sum(mydat[[1]][1, ] * as.numeric(colnames(mydat[[1]])))

# calculate the observed sufficient statistic for within-study correlation for 
# the treatment group  
delta1.suf.stat <- mydat[[1]][, colnames(mydat[[1]]) == 1]
sum.delta1.suf.stat <- sum(delta1.suf.stat[1, ] * 
                             (colSums(delta1.suf.stat) - delta1.suf.stat[1, ]))

# calculate the observed sufficient statistic for within-study correlation for 
# the control group
delta0.suf.stat <- mydat[[1]][, colnames(mydat[[1]]) == 0]
sum.delta0.suf.stat <- sum(delta0.suf.stat[1, ] * 
                             (colSums(delta0.suf.stat) - delta0.suf.stat[1, ]))

###############################################################################
########################## Get Permutation p-Value ############################
###############################################################################

# generate a bunch of tables via random permutation
mydat.perm <- permatfull(mydat[[1]], times = 1000000, fixedmar = "both")
# note that you can change the times argument to whatever you want. I generally
# use at least two values to make sure the next line of code returns the same
# number (to make sure I didn't miss any data sets by not running permatfull
# enough). This can take a while to run.

# grab the unique tables
all.tables <- unique(mydat.perm$perm)

# Eliminate tables that do not meet the other constraints:

# Condition on the observed sufficient statistic for within-study correlation
# for the treatment group  
# i.e. only keep tables that have the same value as sum.delta1.suf.stat
vec.delta1.suf.stat <- lapply(all.tables, function(y) y[, colnames(y) == 1])
vec.sum.delta1.suf.stat <- lapply(vec.delta1.suf.stat, 
                                  function(y) sum(y[1, ] * 
                                                    (colSums(y) - y[1, ])))

# Save tables that have same treatment correlation as original table
first.gamma.tables <- vector("list", length(all.tables))
for (i in 1:length(all.tables)) {
  if (vec.sum.delta1.suf.stat[[i]] == sum.delta1.suf.stat) {
    first.gamma.tables[[i]] <- all.tables[[i]]
  }
}

# pick out the tables meeting this constraint
first.gamma.tables <- first.gamma.tables[!sapply(first.gamma.tables, is.null)]
first.gamma.tables

# further condition:

# Condition on the observed sufficient statistic for within-study correlation
# for the control group  
# i.e. only keep tables that have the same value as sum.delta0.suf.stat
vec.delta0.suf.stat <- lapply(first.gamma.tables, 
                              function(y) y[, colnames(y) == 0])
vec.sum.delta0.suf.stat <- lapply(vec.delta0.suf.stat, 
                                  function(y) sum(y[1, ] * 
                                                    (colSums(y) - y[1, ])))

# Save tables that have same control correlation as original table
gamma.tables <- vector("list", length(first.gamma.tables))
for (i in 1:length(first.gamma.tables)) {
  if (vec.sum.delta0.suf.stat[[i]] == sum.delta0.suf.stat) {
    gamma.tables[[i]] <- first.gamma.tables[[i]]
  }
}

# pick out the tables meeting this constraint
gamma.tables <- gamma.tables[!sapply(gamma.tables, is.null)]
gamma.tables  # final set of tables


###############################################################################
########################### Calculate the p-Value #############################
###############################################################################

############################# probabilities for all tables ####################
choose.n.y <- function(table) {
  numer.indiv <- rep(0, dim(table)[2])
  for (i in 1:dim(table)[2]) {
    # choose(ni yi)
    numer.indiv[i] <- choose(sum(table[, i]), table[1, i])
  }
  return(numer.indiv)
}

allt.choose <- lapply(gamma.tables, choose.n.y)  # "choose" for each cluster
allt.prod <- lapply(allt.choose, prod)  # Numerators for each cluster
allt.all_sum <- sum(unlist(allt.prod))  # Denominator - add all table products

allt.all_prob <- unlist(allt.prod) / allt.all_sum

# Check validity of probabilities
sum(allt.all_prob) # should sum to 1

############################## test statistics ##############################
beta.test.stats <- rep(NA, length(gamma.tables))

# calculate test stats of all tables
for (i in 1:length(gamma.tables)) {
  beta.test.stats[[i]] <- sum(gamma.tables[[i]][1, ] * 
                                as.numeric(colnames(gamma.tables[[i]])))
}

# get table of test statistics and probabilities
stats.and.probs <- as.data.frame(cbind("TestStat" = beta.test.stats, 
                                       "Probs" = allt.all_prob))
dist1 <- stats.and.probs[order(stats.and.probs$TestStat), ]
dist1

# compute p-value
sum(stats.and.probs$Probs[stats.and.probs$TestStat >= sum.xiyi])

# collapsed table
dist1 %>% group_by(TestStat) %>% 
  summarise(Probs = sum(Probs), UnormProbs = sum(Probs) * allt.all_sum)

