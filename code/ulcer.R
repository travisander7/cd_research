library(tidyverse)
source('code/get_est.R')
source("zab_code/FullOriginalGmetaCode.R")
# Ulcer Data.csv: data
# Ulcer1.txt: data (long format)
# Ulcer_distn_82.txt: distribution

#################### Data ####################
data <- read_csv('data/Ulcer Data.csv') %>%
  transmute(
    TrtEvent, 
    TrtTotal = TrtEvent + TrtNonevent, 
    CtrlEvent, 
    CtrlTotal = CtrlEvent + CtrlNonevent
  )
t_obs <- sum(data$TrtEvent)

dist <- read_table('data/Ulcer_distn_82.txt', col_names = c('t', 'p', 'p.norm'))

#################### Exact Permutation method ####################
cd_df <- seq(1e-5, 1-1e-5, length = 1000) %>%
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

ggplot(dist) +
  geom_col(aes(t, p.norm)) +
  geom_vline(xintercept = t_obs)
