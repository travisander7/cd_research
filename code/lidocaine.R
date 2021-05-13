library(tidyverse)
source('code/get_cd.R')
source("zab_code/FullOriginalGmetaCode.R")
# Lidocaine Dataset.csv: data
# Lidocaine1.txt: data (long format)
# Lidocaine_distn_1.txt: distribution

#################### Data ####################
data <- read_csv('data/Lidocaine Dataset.csv') %>%
  select(-Study)
t_obs <- sum(data$Lidocaine_Dead)

dist <- read_table('data/Lidocaine_distn_1.txt', col_names = c('t', 'p', 'p.norm'))

#################### Graph ####################
tibble(
  theta = seq(-3, 2, length = 1000),
  perm = 1 - 2*abs(0.5-map_dbl(theta, get_cd, dist, t_obs)),
  liu = 1 - 2*abs(0.5-gmeta(as.matrix(data), gmi.type = '2x2', method = 'exact1', gmo.xgrid = theta, report.error = TRUE)$combined.cd)
) %>%
  pivot_longer(-theta, names_to = 'method', values_to = 'alpha') %>%
  ggplot(aes(theta, alpha, col = method)) +
  geom_line() +
  geom_hline(yintercept = 0.05, linetype = 'dashed')
