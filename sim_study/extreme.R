library(tidyverse)
df <- read_csv('data/sim.csv')
df %>%  
	group_by(p_ic_init, tau2) %>% 
	summarize(
		only_one = sum(only_one), 
		good = (n() - sum(extreme))/3, 
		extreme = 5000 - only_one - good
	)
