# meta::metabin()
#   Run a simple meta analysis and look out output
#   Do some by hand (Inverse variance fixed effect and DerSimonian Laird random effects)
#   Find dataset or make up dataset

df <- readr::read_csv('binary_data.csv')
m <- meta::metabin(Ee, Ne, Ec, Nc, data = df, studlab = Author, method = 'Inverse', sm = 'OR')

# Adjust for zeros
df[5, 2:5] <- df[5, 2:5] + 0.5

# Calculate study statistics
y <- with(df, (Ee*(Nc-Ec))/(Ec*(Ne-Ee)))
v <- with(df, 1/Ee + 1/(Nc-Ec) + 1/Ec + 1/(Ne-Ee))
lower <- exp(qnorm(0.025, log(y), sqrt(v)))
upper <- exp(qnorm(0.975, log(y), sqrt(v)))
w <- 1/v

# Pool study statistics
OR <- exp(sum(w*log(y))/sum(w))
var <- 1/sum(w)
ci <- exp(qnorm(c(0.025, 0.975), log(OR), sqrt(var)))
z <- log(OR)/sqrt(var)
p_value <- 2*pnorm(z)

print(m)
print(OR)
print(p_value)
