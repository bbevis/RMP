
#################################
# Week 8 - Advanced Estimation
#################################

# install.packages("sandwich")
# install.packages("lmtest")
# install.packages("plm")
# install.packages("AER")

# Load necessary libraries
library(tidyverse)         # For data manipulation
library(lmtest)        # For statistical tests
library(sandwich)      # For robust standard errors
library(plm)           # For panel data methods
library(AER)           # For instrumental variables and 2SLS

# Sample data preparation (simulating time-series panel data)
# This example creates a time-series dataset with panel structure for illustration

set.seed(42) # ensures replicability

data <- data.frame(
  id = rep(1:50, each = 20),                       # 50 individuals, observed over 20 periods
  time = rep(1:20, times = 50),
  y = rnorm(1000),                                 # Outcome variable
  x = rnorm(1000),                                 # Explanatory variable
  z = rnorm(1000),                                 # Instrumental variable
  season = rep(1:4, length.out = 1000)             # Seasonal factor (quarterly)
)

### Fixed Effects (FE) Models ###
# FE models control for time-invariant characteristics of each unit
# Here, we use 'plm' to estimate a fixed effect model for panel data
# Model: y ~ x, with fixed effects for 'id' (individual effects)

# Convert data to a panel data frame
pdata <- pdata.frame(data, index = c("id", "time"))

# Fixed Effects Estimation
fe_model <- plm(y ~ x, data = pdata, model = "within")
summary(fe_model)  # Display summary with coefficient estimates and statistics

# Explanation:
# The "within" model in plm calculates deviations from each individual mean,
# which removes fixed characteristics unique to each entity.

### Time Series Analysis ###
# Exploring trend over time and controlling for seasonal trends

# Example: Estimating a time trend with seasonal controls
# Add time variable to control for trend and seasonal dummies to control for seasonality

data %>%
  with(summary(lm(y ~ time + factor(season))))

# Explanation:
# The 'time' variable captures any overall trend in the data.
# 'factor(season)' adds dummy variables for each season to account for seasonal patterns.

### Two-Way Fixed Effects (TWFE) ###
# Adding both individual (id) and season fixed effects to control for effects at both levels
# fixed effects for both id and season

twfe_model <- plm(y ~ x + z + factor(season), data = pdata, model = "within")
summary(twfe_model)

# Explanation:
# By including time fixed effects (factor(season)), we control for shocks or trends
# affecting all individuals in each period, alongside the individual fixed effects.

### Endogeneity ###
# Endogeneity occurs when a variable in a model
# is influenced by factors that also impact the outcome being studied,
# which can lead to biased or misleading results.

### Instrumental Variables (IV) and Two-Stage Least Squares (2SLS) ###
# Instrumental Variables (IV) are used when the main explanatory variable (x) is endogenous
# Instrument z is used as a tool to deal with the endogeneity of x

# Step 1: First stage of 2SLS, regress x on instrument z
first_stage <- lm(x ~ z, data = data)
data$x_hat <- fitted(first_stage)  # Save fitted values of x

# Step 2: Second stage, regress y on fitted values of x (x_hat)
second_stage <- lm(y ~ x_hat, data = data)
summary(second_stage)

# Explanation:
# In the first stage, we use z to predict x, which helps isolate exogenous variation in x.
# In the second stage, we use this exogenous part of x (x_hat) to estimate its effect on y.

# Alternatively, using the 'ivreg' function from AER package for 2SLS estimation
iv_model <- ivreg(y ~ x | z, data = data)
summary(iv_model)

# Explanation:
# 'ivreg' directly estimates a 2SLS model, where we specify y ~ x and indicate
# z as an instrument for x. It performs both stages internally.
