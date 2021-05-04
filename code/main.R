library(tidyverse)
source('code/get_est.R')
source('code/CMLE.R')

# Data
# x <- c(0, 0, 0, 0, 1, 1, 1, 1)
# z <- c(1, 0, 1, 0, 0, 1, 0, 0)
# n <- c(2, 3, 2, 2, 2, 4, 2, 3)
# t <- sum(x*z)

# Get estimates
# dist <- get_dist(x, z, n)

t <- 2
dist <- tibble(
  t = c(1, 2, 3, 4),
  p = c(5, 20, 60, 15),
  p.norm = p/sum(p)
)

cd <- exp(seq(-5, 0, length = 100)) %>%
  set_names %>%
  map_df(function(alpha) get_est(dist, t, alpha), .id = 'alpha') %>%
  mutate_at('alpha', as.numeric)
slice(cd, which.min(abs(0.05 - alpha)))

# P-value function
ggplot(cd) +
  geom_line(aes(lower, alpha)) +
  geom_line(aes(upper, alpha))

# Midp p-value function
ggplot(cd) +
  geom_line(aes(midp_lower, alpha)) +
  geom_line(aes(midp_upper, alpha)) +
  xlab(expression(beta)) +
  ylab('Level of significance') +
  geom_hline(yintercept = 0.05, linetype = 'dashed')

u <- dist$t
p <- dist$p.norm
t <- t - min(u)
u <- u - min(u)
uppers <- map_dbl(alphas, function(alpha) CMLE(p, u, t, t.or.alpha = alpha/2, ci = TRUE, dir = 'upper', midp = TRUE))

ggplot() +
  geom_line(aes(uppers, alphas))
