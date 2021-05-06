library(tidyverse)
source('code/get_est.R')
source("zab_code/FullOriginalGmetaCode.R")
# AntibioticsRheumaticFeverData.csv: data
# AntibioticsRheumaticFever1.txt: data (long format)
# AntibioticsRheumaticFever_distn_32.txt: distribution

#################### Data ####################
data <- read_csv('data/AntibioticsRheumaticFeverData.csv')
t_obs <- sum(data$Antibiotics_Event)

dist <- read_table('data/AntibioticsRheumaticFever_distn_32.txt', col_names = c('t', 'p', 'p.norm'))

#################### Exact Permutation method ####################
cd_df <- exp(seq(-5, 0, length = 100)) %>%
  set_names %>%
  map_df(function(alpha) get_est(dist, t_obs, alpha), .id = 'alpha') %>%
  mutate_at('alpha', as.numeric)

#################### Liu method ####################
liu <- gmeta(
  as.matrix(data), 
  gmi.type = "2x2", 
  method = "exact1", 
  gmo.xgrid = seq(-2, 1, length = 1000), 
  report.error = TRUE
)

liu_df <- tibble(
  log_OR = liu$x.grids,
  p_value = 1 - 2*abs(0.5-liu$combined.cd)
)

#################### Graph ####################
ggplot(cd_df) +
  geom_line(aes(midp_lower, alpha)) +
  geom_line(aes(midp_upper, alpha)) +
  geom_line(aes(lower, alpha), linetype = 'dashed') +
  geom_line(aes(upper, alpha), linetype = 'dashed') +
  geom_line(aes(log_OR, p_value), liu_df, col = 'blue')
