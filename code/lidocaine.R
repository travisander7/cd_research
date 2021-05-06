library(tidyverse)
source('code/get_dist.R')
# Lidocaine Dataset.csv: data

data <- read_csv('data/Lidocaine Dataset.csv') %>%
  select(-Study) %>%
  pivot_longer(everything(), names_to = c('Treatment', '.value'), names_sep = '_') %>%
  mutate(Treatment = as.numeric(Treatment == 'Lidocaine')) %>%
  arrange(Treatment)

t_obs <- sum(data$Treatment*data$Dead)
dist <- get_dist(data$Treatment, data$Dead, data$Total)
