# Returns the coeffient of determination for two given vectors
coefOfDeterm <- function(x, y) {
  r <- cor(x, y)
  return(r * r)
}

# Find the predicted value of x given an intercept of b.0 and a slope of b.1
lmPrediction <- function(b.0, b.1, x) {
  return(b.0 + b.1 * x)
}

# Check whether to reject the null hypothesis based on given alpha level
# degrees of freedom and t value.  Returns true if null should be rejected
check_t <- function(a, n, t) {
  # find value of p for a given alpha value and degrees of freedom
  p <- ((1 - a) / 2) + a
  df <- n - 2
  
  # Finds the critical t value using p and degrees of freedom
  crit_t <- qt(p, df)
  print(paste("Critical Value of t:", crit_t))
  return((t > crit_t) | (t < (crit_t * -1)))
}

# Finds the sum of squared errors from given lm model and 
# a vector containing the dependent variables
findSSE <- function(model, d) {
  sse <- sum((fitted(model) - d) ^ 2)
  return(sse)
}

# Finds the sum of squared errors from given lm model and 
# a vector containing the dependent variables
findSSR <- function(model, d) {
  ssr <- sum((fitted(model) - mean(d)) ^ 2)
  return(ssr)
}

# Finds the sum square of totals for a given model and 
# vectors containing the dependent and independent variables
findSST <- function(model, d) {
  sse <- findSSE(model, d)
  ssr <- findSSR(model, d)
  
  df <- data.frame(SSE = sse, 
                   SSR = ssr,
                   SST = sse + ssr)
  
  print(df)
}

# Returns the variance of error/residuals from a given linear regression model
varOfResiduals <- function(model) {
  return(summary(model)$sigma ^ 2)
}

# Returns the variance of the slope for a give linear regression model
varOfSlope <- function(model) {
  return(vcov(model)[2,2])
}

# Find the interval bounds for a given confidence level a, 
# with a linear regression model m that has n number of observations
intervalCalc <-function(a, n, model) {
  vSlope <- varOfSlope(model)
  slope <- model$coefficients[2]
  
  # find value of p for a given alpha value and degrees of freedom
  p <- ((1 - a) / 2) + a
  df <- n - 2
  
  # Finds the critical t value using p and degrees of freedom
  crit_t <- qt(p, df)
  
  # Find lower and upper bound of the interval
  lBound <- round(slope, 4) - round(crit_t, 3) * round(sqrt(vSlope), 3)
  uBound <- round(slope, 4) + round(crit_t, 3) * round(sqrt(vSlope), 3)
  
  # Print the data
  df <- data.frame(UpperBound = uBound, LowerBound = lBound)
  print(df)
}

# Independent and dependent variable v
i <- c(31,31,38,48,49)
d <- c(2,4,5,5,7)

# dependent variable goes on the left of lm
res <- lm(d~i)

# Do the things with the functions
findSST(res, d)
summary(res)
check_t(.9, 19, 1.88)
varOfResiduals(res)
varOfSlope(res)
intervalCalc(.95, 5, res)

