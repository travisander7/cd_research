df <- readr::read_csv('binary_data.csv')

# Shuffle data
set.seed(1)
df <- dplyr::mutate_if(df, is.numeric, sample)

m <- meta::metabin(Ee, Ne, Ec, Nc, data = df, studlab = Author, method = 'Inverse', sm = 'OR')

# Adjust for zeros
df[which(df$Ee == 0), 2:5] <- df[which(df$Ee == 0), 2:5] + 0.5

# Calculate study statistics
y <- with(df, (Ee*(Nc-Ec))/(Ec*(Ne-Ee)))
v <- with(df, 1/Ee + 1/(Nc-Ec) + 1/Ec + 1/(Ne-Ee))
lower <- exp(qnorm(0.025, log(y), sqrt(v)))
upper <- exp(qnorm(0.975, log(y), sqrt(v)))
w <- 1/v

# Calculate tau^2
Q <- sum(w*log(y)^2) - sum(w*log(y))^2/sum(w)
df <- length(y) - 1
C <- sum(w) - sum(w^2)/sum(w)
tau2 <- (Q - df)/C

# Pool study statistics
w <- 1/(v + tau2)
OR <- exp(sum(w*log(y))/sum(w))
var <- 1/sum(w)
ci <- exp(qnorm(c(0.025, 0.975), log(OR), sqrt(var)))
z <- log(OR)/sqrt(var)
p_value <- 2*pnorm(z)

print(m)
print(tau2)
print(OR)
print(p_value)
