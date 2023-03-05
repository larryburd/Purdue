xbarSD <- function(s, n) {
  return(s/sqrt(n))
}

# Returns zscore for given mean, standard deviation,
# random variable x, and sample size
xbarZScore <- function(x, mu, s, n) {
  xbarSigma <- xbarSD(s, n)
  numerator <- x - mu
  zScore <- numerator / xbarSigma
  return(zScore)
}

# Returns z score for given sample proportion p.hat, population proportion p, and sample size n
phatZScore <- function(p.hat, p, n) {
  numerator <- p.hat - p
  denominator <- sqrt((p * (1 - p)) / n)
  return(numerator / denominator)
}

# Find F stat for 2-way ANOVA data
fstat <- function(MSA, MSE) {
  return(MSA/MSE)
}

# H.0 for testing for interaction is "there is no interaction"
# If there is interaction, then the rest of the test cannot be performed
# Find whether to reject H.0 based on fstat and alpha
# Use MSB if checking whether blocking was useful
rejectNull <- function(MSA, MSE, a, df1, df2) {
  f <- fstat(MSA, MSE)
  print(paste("F-Stat:", round(f, 2)))
  cv <- qf(a, df1, df2, lower.tail = F)
  print(paste("Critical Value:", cv))
  # Reject null if greater than critical value
  if (f > cv) {
    print("Reject H.0")
  } else {
    print("Fail to reject H.0")
  }
}
rejectNull(1006.8402, 136.4024, .05, 2, 18)
rejectNull(237.8551, 136.4024, .05, 4, 18)


xbarZScore(75, 45, 105, 125)
1 - pnorm(3.194)
phatZScore(.3, .34, 130)
pnorm(-0.96)

