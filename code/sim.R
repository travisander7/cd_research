library(tidyverse)
source('zab_code/pRandom_flexible.R')
source('code/get_cd.R')
# 4 middle, 1 high end, 55 low end
seed <- 4
set.seed(seed)

F2f<-function (x, Fx) {
  fx = diff(Fx, lag = 2)/diff(x, lag = 2)
  fx = c(NA, fx, NA) # match the length of x-fx-cdf
  # n.xgrids
  n = length(fx)
  # liner extrapolation
  fx[1] = max(0, (fx[2]-fx[3])/(x[2]-x[3]) * (x[1]-x[2]) + fx[2])
  fx[n] = max(0, (fx[n-1]-fx[n-2])/(x[n-1]-x[n-2]) * (x[n]-x[n-1]) + fx[n-1])
  # return
  return(fx)
  #[update in v2.0: previously #return(c(NA, fx, NA)) # NA so that the edge of the plot of density is right.]
}

data <- pRandom(
  theta = 1, 
  k = 5, 
  p_ic_init = 0.1, 
  tau2 = 0.2, 
  min.n = 20, 
  max.n = 30
) %>%
  as_tibble %>%
  select(TRT_event, TRT_n, CTRL_event, CTRL_n)

rema_obj <- with(data, rema::rema(TRT_event, TRT_n, CTRL_event, CTRL_n, alpha = 0.05))
dist <- rema_obj$dist
t_obs <- sum(data$TRT_event)

tibble(
  thetas = seq(-2, 4, length = 1000),
  cd = get_cd(thetas, dist$norm.probs, dist$test.stat, t_obs),
  liu = gmeta::gmeta(
    as.matrix(data), 
    gmi.type = '2x2', 
    method = 'exact1', 
    gmo.xgrid = thetas, 
    report.error = TRUE
  )$combined.cd
) %>%
  pivot_longer(-thetas, names_to = 'method', values_to = 'distribution') %>%
  group_by(method) %>%
  mutate(
    curve = 1 - 2*abs(0.5-distribution),
    density = F2f(thetas, distribution) # c(NA, diff(distribution, 2)/diff(thetas, 2), NA) (same as F2f)
  ) %>%
  pivot_longer(-c(thetas, method), names_to = 'type') %>%
  ggplot(aes(thetas, value, col = method)) +
  geom_line() +
  facet_wrap(~type, ncol = 1)
ggsave(str_c('data/first', seed, '.png'))

ci_df <- seq(1e-3, 1-1e-3, length = 100) %>%
  set_names %>%
  map_df(
    function(alpha){
      ci <- with(data, rema::rema(TRT_event, TRT_n, CTRL_event, CTRL_n, alpha = alpha)$CI) %>%
        t
      colnames(ci) <- c('lower', 'upper')
      as_tibble(ci)
    }, .id = 'alpha'
  ) %>%
  mutate_at('alpha', as.numeric)

tibble(
  theta = seq(-2, 5, length = 1000),
  cd = get_cd(theta, dist$norm.probs, dist$test.stat, t_obs)
) %>%
  ggplot() +
  geom_line(aes(lower, alpha), ci_df) +
  geom_line(aes(upper, alpha), ci_df) +
  geom_line(aes(exp(theta), 1-2*abs(0.5-cd)), col = 'blue', linetype = 'dashed') +
  xlim(0, 50)
ggsave(str_c('data/second', seed, '.png'))
