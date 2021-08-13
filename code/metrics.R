library(tidyverse)
source('zab_code/pRandom_flexible.R')
source('code/get_cd.R')
set.seed(1)

theta <- 1
sim <- pRandom(theta, 10, 0.05, 0.5, 20, 30)

m <- with(sim, rema::rema(TRT_event, TRT_n, CTRL_event, CTRL_n))
m$dist
m$tstat

height <- 1 - abs(1 - 2*get_cd(theta,  m$dist$norm.probs, m$dist$test.stat, m$tstat))

tibble(
  thetas = seq(-4, 4, length = 1000),
  dist = get_cd(thetas, m$dist$norm.probs, m$dist$test.stat, m$tstat),
  curve = 1-abs(1-2*dist)
) %>%
  ggplot(aes(thetas, curve)) +
  geom_area(alpha = 0.3, col = 'black', fill = 'blue') +
  geom_vline(xintercept = theta) +
  geom_hline(yintercept = height, linetype = 'dashed')
ggsave('data/metric.png')
