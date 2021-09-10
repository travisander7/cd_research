library(tidyverse)

df <- read_csv('data/sim_results.csv')

# Analysis of number of only one or extreme datasets
df %>%  
	group_by(theta, p_ic_init, tau2) %>% 
	summarize(
		only_one = sum(only_one), 
		good = (n() - sum(extreme))/3, 
		extreme = 1000 - only_one - good
	) %>%
  pivot_longer(c(only_one, good, extreme)) %>%
  ggplot(aes(theta, value, col = name)) +
  geom_line() +
	facet_grid(vars(tau2), vars(p_ic_init))

# Analysis of area and height by theta, p_ic_init, and tau2

plot <- df %>%
	filter(!only_one, !extreme) %>%
	group_by(theta, p_ic_init, tau2, method) %>%
	summarize(
		area = mean(area),
		height = mean(height),
		cover = mean(lower < theta & upper > theta)
	) %>%
	ggplot(aes(theta, col = method)) +
	facet_grid(vars(tau2), vars(p_ic_init))

plot + geom_line(aes(y = area))
ggsave('data/area.png')
plot + geom_line(aes(y = height))
plot + geom_line(aes(y = cover))
# y aesthetic is coverage (95%)

# Analyze whether height ~ Unif(0, 1)
df %>%
	filter(!only_one, !extreme) %>%
	ggplot(aes(height, col = method)) +
	geom_density(adjust = 2)