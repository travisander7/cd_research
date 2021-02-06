df <- readr::read_csv(
 'Author,Ee,Ne,Ec,Nc
  Alcorta-Fleischmann,2,279,1,70
  Craemer,18,1273,17,1287
  Eriksson,6,1858,5,1852
  Jones,3,297,6,314
  Knauer,0,300,1,295
  Kracauer,8,1331,9,1359
  La Sala,12,1618,12,1520
  Maheux,1,510,10,509
  Schmidthauer,7,232,8,192
  van der Zee,17,287,21,299
  Wang,11,984,19,938'
)
m <- meta::metabin(Ee, Ne, Ec, Nc, data = df, studlab = Author, method = 'Inverse', sm = 'OR')

#################### Fixed effects ####################

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

#################### Random effects ####################

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