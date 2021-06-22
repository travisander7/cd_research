library(tidyverse)
source('zab_code/pRandom_flexible.R')
source('code/get_cd.R')
set.seed(4)

#################### Sample dataset ####################
theta <- 1
sim <- pRandom(theta, k = 5, p_ic_init = 0.1, tau2 = 0.2, min.n = 20, max.n = 30)

#################### Permutation method ####################
perm <- with(sim, rema::rema(TRT_event, TRT_n, CTRL_event, CTRL_n, alpha = 0.01))
perm_ci <- log(perm$CI)
perm_df <- tibble(
  thetas = seq(perm_ci[1], perm_ci[2], length = 1000),
  cd = get_cd(thetas, perm$dist$norm.probs, perm$dist$test.stat, perm$tstat)
)

#################### Liu method ####################
liu <- gmeta::gmeta(
  with(sim, cbind(TRT_event, TRT_n, CTRL_event, CTRL_n)), 
  gmi.type = '2x2', 
  method = 'exact1', 
  ci.level = 0.99
)
liu_ci <- liu$combined.ci
liu_df <- tibble(
  thetas = seq(liu_ci[1], liu_ci[2], length = 1000),
  cd = gmeta::gmeta(
    with(sim, cbind(TRT_event, TRT_n, CTRL_event, CTRL_n)),
    gmi.type = '2x2',
    method = 'exact1',
    gmo.xgrid = thetas
  )$combined.cd
)

#################### Area and height calculation ####################
bind_rows(perm = perm_df, liu = liu_df, .id = 'method') %>%
  mutate(curve = 1-2*abs(0.5-cd)) %>%
  group_by(method) %>%
  mutate(is_height = row_number() == which.min(abs(thetas - theta))) %>%
  summarize(area = sum(curve)*(max(thetas) - min(thetas))/1000, height = sum(curve*is_height))
