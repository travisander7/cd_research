library(tidyverse)
df <- read_csv('data/sim.csv')

df %>%
	filter(!only_one, !extreme) %>%
	group_by(p_ic_init, tau2, method) %>%
	summarize(
		area = mean(area),
		height = mean(height)
	) %>%
	as.data.frame
