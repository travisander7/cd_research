# Wikipedia example data
data <- rbind(c(1,9),c(11,3))

# Using R's function
p_value <- fisher.test(data, alternative = 'less')$p.value

# Using hypergeometric distribution
phyper(1, 12, 12, 10)

# Completely manual computation
choose(sum(data[1,]), data[1,1])*choose(sum(data[2,]), data[2,1])/choose(sum(data), sum(data[,1])) + # 1 in upper left
choose(sum(data[1,]), data[1,1]-1)*choose(sum(data[2,]), data[2,1]+1)/choose(sum(data), sum(data[,1])) # 0 in upper left

# Mid-p (one-sided)
0.5*phyper(1, 12, 12, 10) + phyper(0, 12, 12, 10)
