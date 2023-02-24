# Needed to stop rounding from happening
options(digits = 15)

# Return adjusted CoD from given residual sum of squares (sse), 
# total sum of squares (sst), num of data (n), and num of indepent vars (k)
# Remember to add one to df
adjCoD <- function(sse, sst, n, k) {
  CoD <- CoDFromSS (sse, sst)
  numerator <- n - 1
  denominator <- n - k - 1
  return(1 - (numerator / denominator) * CoD)
}

# Return the coefficient of determinations from a given sum of squares
# and a total sum of squares
CoDFromSS <- function(ss, sst) {
  return(ss/sst)
}

# Returns the coefficient of determination for two given vectors
CoDFromData <- function(x, y) {
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
i <- c(0,
       0.5,
       1,
       1.5,
       2,
       2.5,
       3,
       4,
       5,
       5.5)
d <- c(60,
       66,
       69,
       75,
       78,
       81,
       87,
       93,
       96,
       99)
cor(i, d)
cor(d, i)
# dependent variable goes on the left of lm
res <- lm(d~i)

# Do the things with the functions
findSST(res, d)
summary(res)
check_t(.9, 19, 1.88)
varOfResiduals(res)
varOfSlope(res)
intervalCalc(.95, 5, res)
lmPrediction(491.64210526, 25.34360902, 21)

11278.70+2888.44*8+810.75*7

emp1 <- 10811.66 + 2636.56 * 8 + 757399 * 5
emp2 <- 10811.66 + 2636.56 * 10 + 757399 * 5
emp2 - emp1
emp1

coefOfDetermFromSS(362726.0368, 983912.6838)

adjCoD(621186.6470, 983912.6838, 79, 2)

h <- c()
gpa <- c(2,2,4,4,2)
score <- c(25,17,28,27,26)
res <- lm(score ~ h + gpa)
summary(res)

data <- read.csv("Purdue/Purdue/MM325/Unit 8/hw.csv")
names(data)
# Simple regression
res <- lm (data$Second.Test.Grade ~ data$First.Test.Grade)

# Multiple Regression
res <- lm(data$Number.of.Tickets ~ data$Age + data$GPA)
summary(res)

38915.10696 + 1390.552286
11722.40 + 3182.56 * 6 + 1202.44 * 3
a <- 139082.395680 + 3199.644590 * 22
b <- 139082.395680 + 3199.644590 * 22 + 21288.797965
a - b
a
b

123744.813905+3650.924912*26+17603.709201
b
