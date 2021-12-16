library(tidyverse)

a <- read_csv('data/ends.csv') %>%
  right_join(select(df, method, theta, tau2, rep, extreme, only_one))
a %>%
  group_by(method) %>%
  summarize(mean(first +last > 0.02))
