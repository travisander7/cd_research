library(tidyverse)

df <- read_csv('sim_results.csv') %>%
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
df %>%
  group_by(method, theta, tau2) %>%
  summarize_at('area', mean) %>%
  ggplot(aes(theta, area, col = method)) +
  geom_line() +
  facet_wrap(~tau2)

########## Height ##########
df %>%
  group_by(method, theta, tau2) %>%
  summarize_at(c('area', 'height', 'height2', 'height3'), mean) %>%
  pivot_longer(contains('height'), 'metric') %>%
  ggplot(aes(theta, value, col = method)) +
  geom_line() + 
  facet_grid(vars(metric), vars(tau2), scales = 'free_y')

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
