# meta::metabin()
#   Run a simple meta analysis and look out output
#   Do some by hand (Inverse variance fixed effect and DerSimonian Laird random effects)
#   Find dataset or make up dataset
library(tidyverse)

df <- read_csv('binary_data.csv')
m <- meta::metabin(Ee, Ne, Ec, Nc, data = df, studlab = Author, method = 'Inverse', sm = 'OR')

# Adjust for zeros
df[5, 2:5] <- df[5, 2:5] + 0.5

# Calculations for each study
by_study <- df %>%
  mutate(
    OR = (Ee*(Nc-Ec))/(Ec*(Ne-Ee)),
    var = 1/Ee + 1/(Nc-Ec) + 1/Ec + 1/(Ne-Ee),
    lower = exp(qnorm(0.025, log(OR), sqrt(var))),
    upper = exp(qnorm(0.975, log(OR), sqrt(var))),
    weight = 1/var,
    w_perc = 100*weight/sum(weight)
  )

# Pooled calculations
pooled <- by_study %>%
  summarize(
    OR = exp(sum(log(OR)*w_perc)/100),
    var = 1/sum(weight)
  ) %>%
  mutate(
    lower = exp(qnorm(0.025, log(OR), sqrt(var))),
    upper = exp(qnorm(0.975, log(OR), sqrt(var))),
    z = log(OR)/sqrt(var),
    p_value = 2*pnorm(z)
  )

print(m)
print(by_study)
print(pooled)
