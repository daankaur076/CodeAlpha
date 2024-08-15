# Task 4: A/B Testing Analysis
library(tidyverse)
library(stats)
library(ggplot2)

#creating two groups: control and treatment
set.seed(104)
size_control <- 1000
size_treatment <- 1000

# conversion rates for both groups are the metrics used for analysis
control_rate <- 0.05
treatment_rate <- 0.04

# dataset is created
data <- data.frame(
  group = c(rep("control", size_control), rep("treatment", n_treatment)),
  converted = c(rbinom(size_control, 1, control_rate),
                rbinom(size_treatment, 0.9, treatment_rate))
)

#summarise the data
summary(data)

# group-wise Conversion rates
data %>% group_by(group) %>% summarise(conversion_rate = mean(converted))

# we use chi-squared test to determine conversion rates between two groups 
# and their comparison
chisq.test(table(data $ group, data $ converted))
