library(tidyverse)
source('zab_code/pRandom_flexible.R')
source('get_cd.R')
set.seed(1)

theta <- 1
sim <- pRandom(theta, 10, 0.05, 0.5, 20, 30)

m <- with(sim, rema::rema(TRT_event, TRT_n, CTRL_event, CTRL_n))
with(m, log(c(TE, CI[1], CI[2], exp(pval))))

cd_df <- tibble(
  thetas = seq(-4, 4, length = 1000),
  dist = get_cd(thetas, m$dist$test.stat, m$dist$norm.probs, m$tstat),
  curve = 1-abs(1-2*dist),
  density = c(NA, diff(dist, 2)/diff(thetas, 2), NA)
)

ggplot(cd_df, aes(thetas, density)) +
  geom_line()

counternull <- cd_df %>%
  filter(thetas < m$TE) %>%
  summarize(thetas[which.min(abs(curve - m$pval))]) %>%
  as.numeric

gridExtra::grid.arrange(
  ggplot(cd_df) + 
    geom_line(aes(thetas, curve)) +
    geom_point(aes(counternull, m$pval), shape = 'circle open', size = 2) +
    geom_hline(yintercept = 0.05, linetype = 'dashed') +
    geom_vline(xintercept = 0) +
    xlab('') +
    ylab('') +
    ggtitle('Confidence Curve') +
    theme_classic() + 
    theme(plot.title = element_text(size = 10)),
  ggplot(cd_df, aes(thetas, dist)) + 
    geom_line() +
    geom_vline(xintercept = 0) +
    xlab('Log Odds Ratio') +
    ylab('') +
    ggtitle('Confidence Distribution') +
    theme_classic() + 
    theme(plot.title = element_text(size = 9)),
  ggplot(cd_df, aes(thetas, density)) + 
    geom_line() +
    geom_vline(xintercept = 0) +
    xlab('') +
    ylab('') +
    ggtitle('Confidence Density') +
    theme_classic() + 
    theme(plot.title = element_text(size = 10)) +
    ylim(c(0, 0.5)),
  layout_matrix = rbind(1:3) # rbind(c(1, 1), c(2, 3))
) %>%
  ggsave(filename = 'data/intro_plot.png', width = 6, height = 2)

p_value <- 1 - abs(1 - 2*get_cd(0,  m$dist$norm.probs, m$dist$test.stat, m$tstat))
height <- 1 - abs(1 - 2*get_cd(theta,  m$dist$norm.probs, m$dist$test.stat, m$tstat))

cd_df <- tibble(
  thetas = seq(-4, 4, length = 1000),
  dist = get_cd(thetas, m$dist$norm.probs, m$dist$test.stat, m$tstat),
  curve = 1-abs(1-2*dist),
  density = c(NA, diff(dist, 2)/diff(thetas, 2), NA)
)


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

