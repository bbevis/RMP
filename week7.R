
#################################
# Week 7 - Hypothesis testing
# Imai & Williams 7.2
#################################

install.packages('car')
install.packages('AER')
install.packages("haven")
install.packages("pwr")
library(tidyverse)
library(car)
library(AER)
library(haven)
library(pwr)
# In this classic experiment (often attributed to Sir Ronald A. Fisher),
# someone is tasked with distinguishing between cups of tea prepared in two different ways
# (e.g., with milk poured first vs. tea poured first)

# Creating table for tea-tasting experiment: one with
# all 8 cups correctly classified,
# and another with 6 out of the 8 cups correctly classified

x = matrix(c(4,0,0,4), byrow = TRUE, ncol = 2, nrow = 2)

# 4 cups of "Milk first" were correctly identified as "M".
# 4 cups of "Tea first" were correctly identified as "T".

y = matrix(c(3,1,1,3), byrow = TRUE, ncol = 2, nrow = 2)

# M = Milk poured first
# T = Tea poured first

rownames(x) = colnames(x) = rownames(y) = colnames(y) = c("M", "T")

# The first test on x asks if perfect classification is statistically significant.

fisher.test(x, alternative = "greater")

# A p-value of 0.01429 is below the typical significance level of 0.05,
# suggesting that the tea-taster’s ability to correctly classify all cups is statistically significant.


# This test uses the default two-sided alternative,
# evaluating whether there is any association between the classification
# categories in y (where 6 out of 8 cups are correctly classified).
fisher.test(y)

# A p-value of 0.4857 suggests there is no statistically significant association
# between the classification and the actual arrangement of cups.

# CI indicates high level of uncertainty
##########################################
# Statistical hypothesis testing
#
# 1. Specify a null hypothesis
# 2. Choose a test statistic and level of alpha
# 3. Derive the sampling distribution of the test statistic
# 4. Compute p-value (one or two sided)
# 5. Reject the null if p-value is less than or equal to the alpha
#
##########################################

n = 1018
x_bar = 550 / n # mean
se = sqrt(0.5 * 0.5 / n) # The standard error of the sample mean
# under the assumption that the true population proportion is 0.5.

# pnorm() function allows you to simulate the normal distribution
upper = pnorm(x_bar, mean = 0.5, sd = se, lower.tail = FALSE)

# This calculates the upper tail probability. It computes the probability that the sample proportion
# x_bar is greater than 0.5 (the hypothesized population proportion)
# lower.tail = FALSE: Specifies that we are interested in the probability in the upper tail

lower = pnorm(0.5 - (x_bar - 0.5), mean = 0.5, sd = se)
# This calculates the lower tail probability. It computes the probability that the sample proportion
# x_bar is less than 0.5.

# 1 sided p-value
upper

# 2 sided p-value
upper + lower

# since upper and lower areas are the same, we can also do:
2 * upper

# When using the normal distribution, researchers often use the z-score
z_score = (x_bar - 0.5) / se
pnorm(z_score, lower.tail = FALSE) # 1 sided p-value
2 * pnorm(z_score, lower.tail = FALSE) # 2 sided p-value

# Confidence intervals
# Directly related to hypothesis tests

# 99% confidence interval containing 0.5
c(x_bar - qnorm(0.995) * se, x_bar + qnorm(0.995) * se)

# The qnorm() function returns the critical value (z-score) associated with
# a given cumulative probability for a standard normal distribution.

# Lower Bound: It subtracts the margin of error (z-score * se) from x_bar

# we can reject the null if 1 - alpha confidence levels does not contain mean
# we fail to reject the null when alpha = 0.01

##############################################################
# Using the Star dataset

# data("STAR", package = "qss")
# write.csv(STAR, "STAR.csv", row.names = FALSE)
star = read.csv("STAR.csv")

# Assuming that the null hypothesis is: the mean reading test score = 710, we can calculate a
# 2 sided one sample t-test

t.test(star$g4reading, mu = 710)
# we reject at the 0.05 level, the null hypothesis that the population mean(has a test score of 710.

# 2 sample tests allows us to test whether a hypothesis is not zero

# Testing the null hypothesis of zero average treatment effect
star = star %>%
  mutate(classtype = factor(classtype, labels = c("Small", "Regular", "Regular with aide")))


reading_small = filter(star, classtype == "Small")$g4reading
reading_regular = filter(star, classtype == "Regular")$g4reading

t.test(reading_small, reading_regular)
# since P > 0.05, we fail to reject the null

# Failing to find a significant effect because the data is not informative enough
# i.e. too small a sample size


################################################
# Power
################################################

# We can use the power analysis to quantify the degree of informativeness of data
# where Power = 1 - (type 2 error). Remember that type 2 error is a false positive.

# Min sample size

# Power analysis for a t-test
# Parameters:
# d = effect size (Cohen's d)
# sig.level = significance level (usually 0.05)
# power = desired power (e.g., 0.80)
# type = type of t-test, e.g., "two.sample" for a two-sample t-test
# alternative = "two.sided" or "one.sided" test

pwr.t.test(d = 0.5, sig.level = 0.05, power = 0.80, type = "two.sample", alternative = "two.sided")

# Calculating Power

# power is often interpreted with a few general guidelines:
#
# < 0.5: Very low power, meaning the test is unlikely to detect an effect, even if one is present. Such tests are at high risk for Type II errors (false negatives).
# 0.5 – 0.7: Low power, providing some ability to detect effects but with a substantial risk of Type II errors.
# 0.7 – 0.8: Moderate power, considered marginally acceptable in some research contexts, but may still miss small effects.
# ≥ 0.8: Good power, widely accepted as the standard threshold. Tests with this level of power are usually adequate for detecting meaningful effects.
# > 0.9: High power, desirable for studies where detecting smaller effects is critical, or for confirmatory studies.

# Sample data for two groups
group1 <- c(12, 14, 15, 13, 14)  # Group 1 data
group2 <- c(10, 12, 11, 9, 11)   # Group 2 data

# Run the t-test
test_result <- t.test(group1, group2, var.equal = TRUE)

# Calculate effect size (Cohen's d)
mean_diff <- abs(mean(group1) - mean(group2))
pooled_sd <- sqrt(((length(group1) - 1) * var(group1) + (length(group2) - 1) * var(group2)) / (length(group1) + length(group2) - 2))
cohen_d <- mean_diff / pooled_sd

# Calculate power
power_result <- pwr.t.test(d = cohen_d, n = min(length(group1), length(group2)), sig.level = 0.05, type = "two.sample", alternative = "two.sided")
round(power_result$power, 4)

# Cohen's d is a measure of effect size that quantifies the difference between two group means in terms of standard deviation.


#######################################
# Linear Hypothesis testing
#######################################

# 1. Testing Equality of Coefficients in a Model
# Imagine you’re running a regression to analyze the impact of various educational interventions
# on students' test scores, and you have dummy variables for two specific types of interventions,
# interventionA and interventionB. After running your regression, you might wonder whether these
# interventions have the same effect on test scores. With linearHypothesis(),
# you can test the null hypothesis that the coefficients of interventionA and interventionB are equal.
# linearHypothesis() allows you to directly test this specific constraint without altering your model.
# This is more efficient and targeted, helping you draw conclusions without expanding your model unnecessarily.

## NOTE THIS CODE DOES NOT RUN. THIS IS ONLY AN EXAMPLE
# Fit a model with two interventions
model <- lm(test_scores ~ interventionA + interventionB + control_variables, data = dataset)

# Test if the coefficients of interventionA and interventionB are equal
library(car)
linearHypothesis(model, "interventionA = interventionB")


# 2. Testing Joint Significance of Multiple Variables
# Suppose you’re exploring the impact of various demographic factors on income.
# You may have several categorical variables for different demographics, such as race, gender, and education_level.
# With linearHypothesis(), you could test if all coefficients associated with education_level are jointly significant.

# Fit a model with multiple demographic variables
model <- lm(income ~ race + gender + education_level + control_variables, data = dataset)

# Test the joint significance of all education levels
linearHypothesis(model, c("education_levelHighSchool = 0",
                          "education_levelCollege = 0",
                          "education_levelGraduate = 0"))

#############################################################
# Another example

# load data
d4=read_dta("https://www.dropbox.com/sh/rqmo1hvij1veff0/AABQXwWvdZdvGlT6Y8LIfoMha/dataset4.dta?dl=1")

# Run an OLS regression of the log quantity on the log price, controlling for ice,
# indicating that the Great Lakes were frozen preventing transport by ship,
# and a set of seasonal dummy variables (to capture seasonality in demand;
# note that dataset has 12 seasonal dummies; i.e. they tread every month as a season).

d4 %>%
  with(summary(lm(quantity ~ price + ice)))

# a little trick when you have lots of variables
IVs = c('price', 'ice' , 'seas1'  , 'seas2'  , 'seas3' ,  'seas4' ,  'seas5' ,  'seas6'  , 'seas7' , 'seas8' , 'seas9', 'seas10', 'seas11')

f = as.formula(paste("quantity", paste(IVs, collapse = " + "), sep = " ~ "))
mod = lm(f, data = d4)
summary(mod)

# non-linear model
IVs = c('log(price)', 'ice' , 'seas1'  , 'seas2'  , 'seas3' ,  'seas4' ,  'seas5' ,  'seas6'  , 'seas7' , 'seas8' , 'seas9', 'seas10', 'seas11')
f = as.formula(paste("log(quantity)", paste(IVs, collapse = " + "), sep = " ~ "))
mod = lm(f, data = d4)
summary(mod)

# What is the estimated price elasticity?
# The elasticity is approximately -.65. As price increases by 1%, we expect quantity
# to decrease by .65%

# Hypothesis 1: Cartel -> increases price
IVs = c('cartel', 'ice' , 'seas1'  , 'seas2'  , 'seas3' ,  'seas4' ,  'seas5' ,  'seas6'  , 'seas7' , 'seas8' , 'seas9', 'seas10', 'seas11')
f = as.formula(paste("quantity", paste(IVs, collapse = " + "), sep = " ~ "))
mod_quant = lm(f, data = d4)
summary(mod_quant)

linearHypothesis(mod_quant,"cartel=0")
# Remember that a small P is evidence against (to reject) the null

# Hypothesis 2: Cartel -> restricts supply

IVs = c('cartel', 'ice' , 'seas1'  , 'seas2'  , 'seas3' ,  'seas4' ,  'seas5' ,  'seas6'  , 'seas7' , 'seas8' , 'seas9', 'seas10', 'seas11')
f = as.formula(paste("price", paste(IVs, collapse = " + "), sep = " ~ "))
mod_price = lm(f, data = d4)
summary(mod_price)

linearHypothesis(mod_price,"cartel=0")
