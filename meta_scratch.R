# meta::metabin()
#   Run a simple meta analysis and look out output
#   Do some by hand (Inverse variance fixed effect and DurStemonian Laird random effects)
#   Find dataset or make up dataset
library(tidyverse)

df <- read_csv('binary_data.csv')

m <- meta::metabin(Ee, Ne, Ec, Nc, data = df, studlab = Author, method = 'Inverse', sm = 'OR')



#                         OR           95%-CI %W(fixed) %W(random)
# Alcorta-Fleischmann 0.4982 [0.0445; 5.5741]       1.5        1.5
# Craemer             1.0715 [0.5497; 2.0886]      19.3       19.3
# Eriksson            1.1968 [0.3646; 3.9282]       6.1        6.1
# Jones               0.5238 [0.1298; 2.1137]       4.4        4.4
# Knauer              0.3267 [0.0133; 8.0515]       0.8        0.8
# Kracauer            0.9070 [0.3489; 2.3580]       9.4        9.4
# La Sala             0.9390 [0.4205; 2.0965]      13.3       13.3
# Maheux              0.0980 [0.0125; 0.7687]       2.0        2.0
# Schmidthauer        0.7156 [0.2547; 2.0102]       8.1        8.1
# van der Zee         0.8335 [0.4304; 1.6143]      19.7       19.7
# Wang                0.5468 [0.2588; 1.1553]      15.4       15.4

#                          OR           95%-CI     z p-value
# Fixed effect model   0.7833 [0.5842; 1.0503] -1.63  0.1026

# Adjust for zeros
df[5, 2:5] <- df[5, 2:5] + 0.5

df <- df %>%
  mutate(
    OR = (Ee*(Nc-Ec))/(Ec*(Ne-Ee)),
    var = 1/Ee + 1/(Nc-Ec) + 1/Ec + 1/(Ne-Ee),
    lower = exp(log(OR) - qnorm(0.975)*sqrt(var)),
    upper = exp(log(OR) + qnorm(0.975)*sqrt(var)),
    a = 1/(OR*var)
  )

print(df)
