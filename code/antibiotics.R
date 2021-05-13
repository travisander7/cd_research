library(tidyverse)
source('code/get_cd.R')
source("zab_code/FullOriginalGmetaCode.R")
# AntibioticsRheumaticFeverData.csv: data
# AntibioticsRheumaticFever1.txt: data (long format)
# AntibioticsRheumaticFever_distn_32.txt: distribution

#################### Data ####################
data <- read_csv('data/AntibioticsRheumaticFeverData.csv')
t_obs <- sum(data$Antibiotics_Event)

dist <- read_table('data/AntibioticsRheumaticFever_distn_32.txt', col_names = c('t', 'p', 'p.norm'))

#################### Graph ####################
tibble(
  theta = seq(-2, 1, length = 1000),
  perm = 1 - 2*abs(0.5-map_dbl(theta, get_cd, dist, t_obs)),
  liu = 1 - 2*abs(0.5-gmeta(as.matrix(data), gmi.type = '2x2', method = 'exact1', gmo.xgrid = theta, report.error = TRUE)$combined.cd)
) %>%
  pivot_longer(-theta, names_to = 'method', values_to = 'alpha') %>%
  ggplot(aes(theta, alpha, col = method)) +
  geom_line() +
  geom_hline(yintercept = 0.05, linetype = 'dashed')
