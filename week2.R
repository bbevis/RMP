
###############################################
# Week 2 - Causality
# Imai & Williams 2.3 - 2.7, 3.1 - 3.2
###############################################

# install.packages('tidyverse')
library(tidyverse)
# if you still can't use %>%....
library(dplyr)

############################
# Observational studies
############################

# In 1992, NJ, rasied minimum wage from $4.25 to $5.05
# Did the increase in minimum wage increase employment (reduce unemployment)?

# This increase was in effect in NJ but not in PA

############################
# Loading and saving data
############################

minwage = read.csv('minwage.csv')
# write.csv(minwage, 'minwage.csv', row.names = FALSE)

head(minwage)
summary(minwage)

unique(minwage$location)
unique(minwage$chain)

# What are the average wages in NJ and PA before and after 1992?

minwage = minwage %>%
  mutate(state = if_else(location == 'PA', 'PA', 'NJ'))

minwage %>%
  mutate(wage_min_before = if_else(wageBefore >= 5.05, 1, 0),
         wage_min_after = if_else(wageAfter >= 5.05, 1, 0)) %>%
  group_by(state) %>%
  summarise(prop_before = mean(wage_min_before),
            prop_after = mean(wage_min_after))

# Confounds

minwage %>%
  mutate(wage_min_before = if_else(wageBefore >= 5.05, 1, 0),
         wage_min_after = if_else(wageAfter >= 5.05, 1, 0)) %>%
  group_by(location) %>%
  summarise(prop_before = mean(wage_min_before),
            prop_after = mean(wage_min_after))

# Who did not raise the min wage? Check restaurants in central NJ
minwage %>%
  filter(location == 'centralNJ') %>%
  mutate(wage_min_before = if_else(wageBefore >= 5.05, 1, 0),
         wage_min_after = if_else(wageAfter >= 5.05, 1, 0)) %>%
  group_by(chain) %>%
  summarise(prop_before = mean(wage_min_before),
            prop_after = mean(wage_min_after))


# lets check sample sizes of each restaurant chain

minwage %>%
  filter(state == 'NJ') %>%
  group_by(chain, location) %>%
  count() %>%
  pivot_wider(names_from = 'location', values_from = 'n') %>%
  mutate(prop_NJ = northNJ / sum(centralNJ, northNJ, shoreNJ, southNJ))

# Diff-in-diff

# why can't we just compare before and after?
# or just after?

# DiD

# (after treated - before treated) - (after control - before control)
minwage %>%
  group_by(state) %>%
  summarise(diff = mean(fullAfter) - mean(fullBefore)) %>%
  pivot_wider(names_from = state, values_from = diff) %>%
  mutate(diff_in_diff = NJ - PA)

# Descriptive statistics
# you can use the summary() function

minwage %>%
  group_by(state) %>%
  summarise(M = mean(wageAfter),
          Med = median(wageAfter),
          IQ_range = IQR(wageAfter),
          sd_val = sd(wageAfter),
          n = n(),
          n_unique = n_distinct(wageAfter),
          se = sd_val / sqrt(n))

# Lastly.... a few more things on data manipulation

# IF you have NAs in your dataset

minwage %>%
  group_by(state) %>%
  summarise(M = mean(wageAfter, na.rm = TRUE),
            Med = median(wageAfter),
            IQ_range = IQR(wageAfter),
            sd_val = sd(wageAfter, na.rm = TRUE),
            n = n(),
            n_unique = n_distinct(wageAfter),
            se = sd_val / sqrt(n))
