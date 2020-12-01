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

# Calculate tau^2
Q <- sum(w*y^2) - sum(w*y)^2/sum(w)
df <- length(y) - 1
C <- sum(w) - sum(w^2)/sum(w)
tau2.1 <- (Q - df)/C

# Pool study statistics
w <- 1/(v + tau2)
OR <- sum(w*y)/sum(w)
var <- 1/sum(w)
