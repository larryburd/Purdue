#############################################################################
# Title:  Unit8Discussion.R
# Author: Laurence Burden
# Date:   25 Feb 2023
#
# Purpose:  Create a linear regression model from multiple variables
# 
# Data source: 
# https://www.datafiles.samhsa.gov/dataset/
# national-survey-drug-use-and-health-2019-nsduh-2019-ds0001
# 
#
# Continuous: impydays = "How many days in past yr you were unable to work"
#             methamyfq = "Total # days used methamphetamine past 12 months"
#             heryrtot = "Total # days used heroin past 12 months"
# Categories: diffthink = "Serious difficulty concentrating, remembering, etc"
#             1 = Yes  2 = No
#############################################################################
library(tidyverse)

# Load data
filepath <- paste("Purdue/Purdue/MM325/Unit 7/",
                  "NSDUH_2019.RData",
                  sep = "")
load(filepath)


# Get only those that have missed work days in the past year and answered
# independent variable questions (985 = Bad data & > 993 = Refused/Blank)
data <- PUF2019_100920 |>
  filter(impydays < 366 
         & methamyfq != 985 & methamyfq < 994
         & heryrtot != 985 & heryrtot < 994
         & diffthink < 3) # Only 1 and 2 are useful answers

# Recode data
data$methamyfq[data$methamyfq > 365] <- 0 # Recode "did not use" to 0
data$heryrtot[data$heryrtot > 365] <- 0   # Recode "did not use" to 0
data$diffthink[data$diffthink == 2] <- 0  # Recode 2->0, now 0 = no & 1 = yes

# Find the linear regression model of the data
regResult <- lm(data$impydays ~ data$methamyfq + 
                  data$heryrtot + data$diffthink)

# Calculate the coefficient of determination
CoD <- summary(regResult)$r.squared

# Print results
summary(regResult)
print(paste("Coefficient of Determination:", CoD))
