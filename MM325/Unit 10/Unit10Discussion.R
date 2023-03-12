#####################################################################
# title: Unit10Discussion.R
# Author: Laurence Burden
# Date: 11Mar2023
# Purpose: Perform Chi-Square Test on WVS data
#
# Questions: Q148 (Worries of civil war): 
#               1: Very Much, 2: Great Deal, 3: Not much, 4: Not at all
#            Q275 (Highest educational Level)
#               0: early childhood, 1: primary, 2: lower secondary,
#               3: Upper secondary, 4: post-secondary, 5: short-cycle tertiary
#               6: Bachelors, 7: Masters, 8: Doctorate
#
# Citation: Haerpfer, C., Inglehart, R., Moreno, A., Welzel, C., Kizilova, 
#             K., Diez-Medrano J., M. Lagos, P. Norris, E. Ponarin & B. Puranen 
#             (eds.). 2022. World Values Survey: Round Seven - Country-Pooled 
#             Datafile Version 5.0. Madrid, Spain & Vienna, Austria: JD Systems 
#             Institute & WVSA Secretariat. doi:10.14281/18241.20
#####################################################################
library(tidyverse)

# Function to just returns the solution from
# the base sum function, but this lets addmargin 
# rename the total columns correctly
Total <- function(x) {
  return(sum(x))
}

# Load World Value Survey Data
filepath <- paste('/Users/laure/OneDrive/Documents/Purdue/Purdue/MM325',
                  '/Unit 10/WVS_Cross-National_Wave_7_rData_v5_0.rdata',
                  sep = "")
load(filepath)

# Filter to those responded to the relevant questions
WVS <- `WVS_Cross-National_Wave_7_v5_0` |>
  filter(Q148 > 0 & Q275 > -1)

# Data table using our two relevant questions to be worked on 
data <- table(WVS$Q275, WVS$Q148)
colnames(data) <- c('Very Much', 'Great Deal', 'Not Much', 'Not at All')
rownames(data) <- c('Early Childhood', 'Primary', 'Lower Secondary', 
                    'Upper Secondary', 'Post-Secondary', 'Short Cycle Tertiary',
                    'Bachelors', 'Masters', 'Doctorate')

# Create contingency table with total row and column
contTable <- addmargins(data, c(1,2), FUN = Total)

# Perform chi-square test
result <- chisq.test(data)

# Find critical value for chi-square test
a <- .05 # alpha level
df <- 24 # degrees of freedom

# Right sided chi-square test critical value
critVal <- round(qchisq(a, df, lower.tail = FALSE), digits =3)

# Print results
contTable
print(paste("Critical value for alpha =", a, "and degrees of freedom =", 
        df, ":", critVal))
results
