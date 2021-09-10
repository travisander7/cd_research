library(tidyverse)
source('zab_code/pRandom_flexible.R')
source('code/get_cd.R')
set.seed(1)

theta <- 1
sim <- pRandom(theta, 10, 0.05, 0.5, 20, 30)

m <- with(sim, rema::rema(TRT_event, TRT_n, CTRL_event, CTRL_n))
p_value <- 1 - abs(1 - 2*get_cd(0,  m$dist$norm.probs, m$dist$test.stat, m$tstat))
height <- 1 - abs(1 - 2*get_cd(theta,  m$dist$norm.probs, m$dist$test.stat, m$tstat))

cd_df <- tibble(
  thetas = seq(-4, 4, length = 1000),
  dist = get_cd(thetas, m$dist$norm.probs, m$dist$test.stat, m$tstat),
  curve = 1-abs(1-2*dist),
  density = c(NA, diff(dist, 2)/diff(thetas, 2), NA)
)
counternull <- cd_df %>%
  filter(thetas < m$TE) %>%
  summarize(thetas[which.min(abs(curve - p_value))]) %>%
  as.numeric

cd_df %>%
  pivot_longer(-thetas) %>%
  ggplot(aes(thetas, value)) +
  geom_line() +
  facet_wrap(~name, ncol = 1)
ggsave('data/curve_density.png')

cd_df %>%
  ggplot() +
  geom_line(aes(thetas, curve)) +
  geom_point(aes(x = counternull, y = p_value), shape = 'circle open', size = 3) +
  geom_hline(yintercept = 0.05, linetype = 'dashed') +
  geom_vline(xintercept = 0)
ggsave('data/cd.png')

cd_df %>%
  ggplot(aes(thetas, curve)) +
  geom_area(alpha = 0.3, col = 'black', fill = 'blue') +
  geom_vline(xintercept = theta) +
  geom_hline(yintercept = height, linetype = 'dashed')
ggsave('data/metric.png')

