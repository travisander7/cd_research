library(tidyverse)

df <- read_csv('data/sim_results.csv') %>%
  drop_na %>%
  filter(
    !(theta == -1 & tau2 == 0.2 & rep == 233),
    !(theta == -1 & tau2 == 0 & rep == 2277),
    !(theta == -1 & tau2 == 0.8 & rep == 122),
    !(theta == -1 & tau2 == 0.8 & rep == 2231),
    !(theta == 0.5 & tau2 == 0.8 & rep == 68)
  ) %>%
  group_by(theta, tau2) %>%
  mutate(row = ceiling(row_number()/3)) %>%
  filter(row <= 1000) %>%
  ungroup %>%
  mutate(height3 = map_dbl(height, ~min(.x/0.2, 1)))

########## Area ##########
# 1) Change "area" to "Area Under the CV"
# 2) Move the legend to be at the bottom
# 3) Change "method" to "CD Method"
# 4) Change "II-CC-FF" to "Cunen and Hjort"
# 5) Change "Liu" to "Liu et al."
# 6) Change "Permutation-Based"
# 7) Make all values of theta show up on the x-axis (so include -0.5 and 0.5)
df %>%
  group_by(method, theta, tau2) %>%
  summarize_at('area', mean) %>%
  mutate(`CD method` = recode(
    method, 
    perm = 'Permutation-Based', 
    liu = 'Liu et al.', 
    ii_cc_ff = 'Cunen and Hjort'
  )) %>%
  ggplot(aes(theta, area, col = `CD method`)) +
  geom_line() +
  geom_point() +
  facet_grid(cols = vars(tau2)) +
  theme_classic() +
  xlab(expression(theta)) +
  ylab('Area Under the CV') +
  scale_x_continuous(
    breaks = c(-1, -0.5, 0, 0.5, 1),
    sec.axis = sec_axis(~ ., name = expression(tau^2), breaks = NULL, labels = NULL)
  ) +
  scale_y_continuous(breaks = 0:4) +
  ylim(0, 4) +
  theme(
    legend.position = 'bottom', 
    legend.title = element_blank()
  )
# ggsave(filename = 'data/area.png', width = 6, height = 3)

########## Height ##########
df %>%
  group_by(method, theta, tau2) %>%
  summarize_at(c('area', 'height', 'height2', 'height3'), mean) %>%
  pivot_longer(contains('height'), 'metric') %>%
  ggplot(aes(theta, value, col = method)) +
  geom_line() + 
  facet_grid(vars(metric), vars(tau2), scales = 'free_y')

df %>%
  group_by(method, theta, tau2) %>%
  summarize_at('height', mean) %>%
  ggplot(aes(theta, height, col = method)) +
  geom_line() + 
  geom_point() +
  geom_hline(yintercept = 0.5 + qnorm(c(0.025, 0.975))/sqrt(12*1000), linetype = 'dashed') +
  facet_grid(cols = vars(tau2))

x <- replicate(10000, mean(sapply(runif(1000), function(x) min(x/0.2, 1))))
ci <- quantile(x, c(0.025, 0.975))
df %>%
  group_by(method, theta, tau2) %>%
  summarize_at('height3', mean) %>%
  ggplot(aes(theta, height3, col = method)) +
  geom_line() + 
  geom_point() +
  geom_hline(yintercept = ci, linetype = 'dashed') +
  facet_grid(cols = vars(tau2))

########## Height graph ##########
get_coverage <- function(alpha, .method, .theta, .tau2){
  df %>%
    filter(method == .method, theta == .theta, tau2 == .tau2) %>%
    summarize(coverage = mean(alpha < height)) %>%
    pull(coverage)
}

a <- expand_grid(alpha = seq(0, 1, length = 100), distinct(df, method, theta, tau2)) %>%
  rename_at(c('method', 'theta', 'tau2'), ~str_c('.', .x))

tictoc::tic()
b <- a %>%
  mutate(coverage = pmap_dbl(a, get_coverage))
tictoc::toc()  

b %>%
  group_by(.method, .theta, .tau2) %>%
  mutate(
    median = qbeta(0.5, (1-alpha)*n(), n()+1-(1-alpha)*n()),
    lower = qbeta(0.025, (1-alpha)*n(), n()+1-(1-alpha)*n()),
    upper = qbeta(0.975, (1-alpha)*n(), n()+1-(1-alpha)*n())
  ) %>%
#  mutate_at(c('coverage', 'median', 'lower', 'upper'), ~.x + alpha - 1) %>%
  ggplot(aes(alpha)) +
  geom_line(aes(y = coverage, col = .method)) +
  geom_line(aes(y = median)) +
  geom_line(aes(y = lower), linetype = 'dashed') +
  geom_line(aes(y = upper), linetype = 'dashed') +
  facet_grid(vars(.theta), vars(.tau2))
