# chi-squared test for given probabilities with given proportions
# BrP

# open packages

if(!require(dplyr)) install.packages("dplyr") 
library(dplyr)
if(!require(rstatix)) install.packages("rstatix") 
library(rstatix)
if(!require(rstatix)) install.packages("tibble")  
library(tibble)            

# working directory

setwd("~/Documents/Thesis Dissertation/Data Analysis/SPR (BrPL1) Interpretation Task")

# bringuing the treat data

dataoff3 <- read.csv("dataoFF3.csv")

# S+C condition

sc <- dataoff3 %>%
  filter(Answer %in% c("Volume", "Cardinal"), condition == "S+C") %>%
  count(Answer) %>%
  column_to_rownames("Answer") %>%
  as.matrix()

# Define the expected proportions (CARDINAL, VOLUME)
expected_proportions <- c(0.6, 0.4) # oscillation

# Check if observed categories match expected proportions
#if (!all(names(expected_proportions) %in% rownames(sc))) {
#  stop("The observed and expected categories do not match.")
#}

# Perform the chi-squared test
chi_test <- chisq.test(x = sc, p = expected_proportions, rescale.p = TRUE)

# View the results
print(chi_test)

# X-squared = 0.0035842, df = 1, p-value = 0.9523

################################################################################

# S+M

# Create the contingency table for S+M condition
sm <- dataoff3 %>%
  filter(Answer %in% c("Volume", "Cardinal"), condition == "S+M") %>%
  count(Answer) %>%
  column_to_rownames("Answer") %>%
  as.matrix()

# Define the expected proportions (CARDINAL, VOLUME)
expected_proportions <- c(0.2, 0.8)

# Check if observed categories match expected proportions
if (!all(names(expected_proportions) %in% rownames(sm))) {
  stop("The observed and expected categories do not match.")
}

# Perform the chi-squared test
chi_test <- chisq.test(x = sm, p = expected_proportions, rescale.p = TRUE)

# View the results
print(chi_test)

# X-squared = 37.527, df = 1, p-value = 9.016e-10

################################################################################

# P+C

pc <- dataoff3 %>%
  filter(Answer %in% c("Volume", "Cardinal"), condition == "P+C") %>%
  count(Answer) %>%
  column_to_rownames("Answer") %>%
  as.matrix()

# Define the expected proportions (1 for Volume, 0 for Cardinal)
expected_proportions <- c(0.8, 0.2)

# Check if observed categories match expected proportions
if (!all(names(expected_proportions) %in% rownames(pc))) {
  stop("The observed and expected categories do not match.")
}

# Perform the chi-squared test
chi_test <- chisq.test(x = pc, p = expected_proportions, rescale.p = TRUE)

# View the results
print(chi_test)

# X-squared = 0.74227, df = 1, p-value = 0.3889

################################################################################

# P+M

pm <- dataoff3 %>%
  filter(Answer %in% c("Volume", "Cardinal"), condition == "P+M") %>%
  count(Answer) %>%
  column_to_rownames("Answer") %>%
  as.matrix()

# Define the expected proportions (1 for Volume, 0 for Cardinal)
expected_proportions <- c(0.8, 0.2)

# Check if observed categories match expected proportions
if (!all(names(expected_proportions) %in% rownames(pc))) {
  stop("The observed and expected categories do not match.")
}

# Perform the chi-squared test
chi_test <- chisq.test(x = pm, p = expected_proportions, rescale.p = TRUE)

# View the results
print(chi_test)

# X-squared = 0.19672, df = 1, p-value = 0.6574

################################################################################

# S+M additional analysis to investigate possible oscillation

# Create the contingency table for S+M condition
sm <- dataoff3 %>%
  filter(Answer %in% c("Volume", "Cardinal"), condition == "S+M") %>%
  count(Answer) %>%
  column_to_rownames("Answer") %>%
  as.matrix()

# Define the expected proportions (CARDINAL, VOLUME)
expected_proportions <- c(0.4, 0.6)

# Check if observed categories match expected proportions
if (!all(names(expected_proportions) %in% rownames(sm))) {
  stop("The observed and expected categories do not match.")
}

# Perform the chi-squared test
chi_test <- chisq.test(x = sm, p = expected_proportions, rescale.p = TRUE)

# View the results
print(chi_test)

# S+M X-squared = 0.18456, df = 1, p-value = 0.6675

################################################################################
