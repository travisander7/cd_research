# Exploring consequences of simulation design:
# K: 5, 10, 30
# n: 40, 100, 250, 1000
# theta: 0, 0.5, 1, 1.5, 2
# tau^2: 0, 0.1, . . . , 1
# p_c: 0.1, 0.4
# sigma^2: 0.1, 0.4

# Data-generating models of dichotomous outcomes:
# n: unif(20, 30), unif(230, 240)
# theta: 0, 1
# tau: 0.001, 0.5, 1, 2

# A comparison of confidence distribution approaches for rare event meta-analysis
# K: 10
# theta: 0, 0.5 1, 2
# tau^2: 0, 0.05, 0.10, 0.20, 0.40, 0.80
# mu: -4.5, -4, -3.5, -3
# n: unif(50, 500)

# A permutation-based approach for meta-analyses of rare events
# mu: -4, -3.5, -3
# tau^2: 0, 0.05, 0.10, 0.20, 0.40, 0.80
# n: unif(10, 50)
# theta: -1, -0.5, 0, 0.5, 1
# K: 10

# Combining information across diverse sources
# K: 5, 10, 20, 50
# n: unif(50, 150) control: n*unif(0.5, 1.5)
# theta: -1.5

# Exact meta-analysis approach for discrete data
# K: 48
# theta: 1 to 10
# control arm probabilities: unif(0, 0.01 or 0.05)

# Prediction intervals for random-effects meta-analysis
# mu: 0
# tau^2: 0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5
# K: 3, 5, 10, 15, 20, 25

# tau^2: 0.01, 0.1, 0.2, 0.4, 0.6
# mu: -0.5, 0, 0.5
# K: 3, 6, 12, 24, 48, 96
# n: unif(20, 200)
# control arm probabilities: unif(0.05, 0.65)