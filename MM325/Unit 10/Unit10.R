# BINOMIAL DISTRIBUTIONS -----------------------------------------
# Binomial Distribution for x
p <- .4
n <- 12
x <- 10
493646 * .1584
# Expected Value of X, variance, and standard deviation
expVal <- n * p
variance <- expVal * (1 - p)
stdDev <- sqrt(variance)
dfEvVSd <- data.frame(ExpectedValue = expVal, 
                      Variance = variance, 
                      StandardDeviation = stdDev)
dfEvVSd

# Finds the probability that X = x is true for a single value
dbinom(x, size = n, prob = p)

# Finds the probability that X = x is true for a vector of x's
dists <- dbinom(x, size = n, prob = p)
res <- data.frame(x = x, P = dists)
res
plot(res, type = 'h', col = 'blue', lwd = 10)

# Finds the probability that X < x, where x is a vector
sum(dbinom(x, size = n, prob = p))

# Finds the probability that X <= x
pbinom(x, size = n, prob = p)

# Finds the probability that X > x 
pbinom(x, size = n, prob = p, lower.tail = FALSE)

# CHI-SQUARE ----------------------------------------------------
a <- .025
df <- 1

# Right sided chi-square test critical value
round(qchisq(a, df, lower.tail = FALSE), digits =3)


# Find variance of a vector
X <- c(3.503, 3.627, 4.033, 4.214, 4.193, 4.013)

# Calculate common stats
n <- length(X)
df <- (n - 1)
smplVar<- var(X)
smplStDev <- sqrt(smplVar)
popVar <- (smplVar * df) / n
popVar <- 5400^2

# Print the stats of the vector
dfStats <- data.frame(DegreesOfFreedom = df, SampleVariance = smplVar,
                      SampleStandardDev = smplStDev, 
                      PopulationVariance = popVar)
dfStats
# Find Chi-Square Statistic
chi <- (df * smplVar) / popVar
chi

var_pop <- function(x) {
  mean((x - mean(x))^2)
}

# Conduct a chi-square test for goodness of fit given a vector of values
# and an equal probability of for each value
x <- c(17,19,23,21,10)
p <- 1/5
a <- 0.025
chiGoF(a, x, p, printVars = TRUE)
vP <- rep(p, length(x))
res <- chisq.test(x, p = vP)
?chisq.test
res$statistic
res$expected[1]

# Function to determine if H0 should be rejected based on a given alpha level a,
# vector of values x, and a given probability p.  
chiGoF <- function(a, x, p, printVars = FALSE) {
  # Find chi test stat
  df <- length(x) - 1
  critVal <- round(qchisq(a, df, lower.tail = FALSE), digits =3)
  
    # Extend p into a vector if it is a single value
  if (length(p) == 1){
    vP <- rep(p, length(x))
  } else {
    vP <- p
  }
  
  # Conduct a chi-square test for goodness of fit
  res <- chisq.test(x, p = vP)
  xSq <- res$statistic
  
  # Print values if requested
  if (printVars) {
    print(c("Critical Value: ", critVal))
    print(res)
    print(c("Expected Values: ", res$expected))
  }
  
  # Return whether to reject or not
  if (xSq > critVal) {
    return("Conclusion: Reject H0")
  } else {
    return("Conclusion: Fail to reject H0")
  }
}


vac <- c(rep("diseased", 48), rep("Not Diseased", 46))
notVac <- c(rep("diseased", 57), rep("Not Diseased", 51))
vacStatus <- c("Vaccinated", "Not Vaccinated")
# Create a matrix of the data
data <- matrix(c(48, 46, 57, 51), nrow = 2, ncol = 2, byrow = TRUE, 
               dimnames = list(c("Vaccinated", "Not Vaccinated"), 
                               c("Diseased", "Not Diseased")))

data
res <- chisq.test(data)
res$expected
res

# Create a matrix to hold the data
data <- matrix(c(48, 46, 57, 51), nrow = 2, byrow = TRUE)
colnames(data) <- c("Diseased", "Not Diseased")
rownames(data) <- c("Vaccinated", "Not Vaccinated")
# Perform the chi-squared test for association
result <- chisq.test(data)
# Print the result
cat("Chi-squared test for association:\n")
print(result)

# Create a matrix to hold the data
data <- matrix(c(18, 64, 20, 23, 30, 49, 14, 18, 45, 61, 20, 22), 
               nrow = 3, byrow = TRUE)
colnames(data) <- c("Favorable", "Unfavorable", "Neutral")
rownames(data) <- c("18-30", "30-45", "Over 45")
# Perform the chi-squared test for association
result <- chisq.test(data)
# Print the result
result$expected
cat("Chi-squared test for association:\n")
print(result)

values <- c(61,69,57,78)
data <- matrix(values, nrow = 2, byrow = TRUE)
rownames(data) <- c("Diseased", "Not Diseased")
colnames(data) <- c("Vaccinated", "Not Vaccinated")

values <- c(60,50,52,20,16,18,17,16,16)
data <- matrix(values, nrow = 3, byrow = TRUE)
rownames(data) <- c("Favorable", "Unfavorable", "Neutral")
colnames(data) <- c("18-30", "30-45", "Over 45")

data
res <- chisq.test(data)
res$expected
res
