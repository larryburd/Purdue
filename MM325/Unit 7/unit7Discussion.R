#############################################################################
# Title:  Unit7Discussion.R
# Author: Laurence Burden
# Date:   18 Feb 2023
#
# Purpose:  Create a linear regression model 
# 
# Data source: 
# https://www.datafiles.samhsa.gov/dataset/national-survey-drug-use-and-health-2019-nsduh-2019-ds0001
# 
#
# Questions:  impydays = "How many days in past yr you were unable to work"
#             methamyfq = "Total # days used methamphetamine past 12 months"
# Categories: 
#############################################################################
library(tidyverse)

# Load data
filepath <- paste("Purdue/Purdue/MM325/Unit 7/",
                  "NSDUH_2019.RData",
                  sep = "")
load(filepath)

# Filter data to only those that have used methamphetamine (1 - 365) 
# and answered how many days were unable to work in the past year (0 - 365)
data <- PUF2019_100920 |>
  filter(impydays < 366 & methamyfq < 365)

# Title for scatterplot
scatterTitle <- paste("Comparison of Methampetamin Use with Days",
                      "Unable to Work Within the Past Year",
                      sep = "")

# Plot the data to a scatter plot with lm line
# The days taken meth represents the independent var 
# and days unable to work is the dependent var
data |>
  ggplot(aes(x = methamyfq, y = impydays)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs( title = scatterTitle,
        x = "Number of Days Taking Methamphetamine (Min=1)",
        y = "Number of Days Unable to Work (Min=0)",
        caption = "Data Source: https://www.samhsa.gov/")

# Find the linear regression model of the data
regResult <- lm(data$impydays ~ data$methamyfq)
summary(regResult)

# Calculate the coefficient of determination
cod <- summary(regResult)$r.squared
print(paste("Coefficent of Determination:", cod))
