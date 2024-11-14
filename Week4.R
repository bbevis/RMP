###############################################
#
# Week 4 - Regressions
# Imai & Williams 4.1 - 4.3
#
###############################################

library(tidyverse)
library(lubridate)


###############################################
# Part 1 - correlations
###############################################

# Election results by state
pres08 = read.csv('pres08.csv')

# Polls leading up to the election by sate
polls08 = read.csv('polls08.csv')

# predict the outcome of the 2008 US presidential election

############
# Data prep
############

# estimate the margin between Obama and McCain
pres08 = pres08 %>%
  mutate(margin_actual = Obama - McCain) %>%
  rename(Obama_actual = Obama,
         McCain_actual = McCain)

class(polls08$middate) # check date format

polls08 = polls08 %>%
  mutate(margin_pred = Obama - McCain,
         election_date = ymd("2008-11-04"), # create election year
         middate = ymd(middate), # convert to date format
         days_to_election = as.numeric(election_date - middate)) %>%
  rename(Obama_pred = Obama,
         McCain_pred = McCain)

# join datasets
polls_data = polls08 %>%
  left_join(pres08, by = 'state')

##################
# Exploratory analysis
##################

# Do actual vs predicted margins correlate?
polls_data %>%
  select(margin_pred, margin_actual) %>% # easiest to just keep the columns you want to correlate
  cor()

polls_data %>%
  select(state, days_to_election, margin_pred, margin_actual) %>%
  group_by(state) %>%
  summarise(margin_pred_m = mean(margin_pred),
            margin_actual_m = mean(margin_actual)) %>%
  ggplot(aes(x = margin_pred_m, y = margin_actual_m)) +
  geom_point() +
  theme_minimal()

###############################################
# Part 2 - Regressions
###############################################

# Do women promote different policies than men?
women = read_csv("women.csv")

# In India since the mid 1990s, 33% of village councils heads were randomly reserved for women.
# Sample taken from West Bengal
# Implemented at Gram Panchayat (GP) level, which contains many villages

# Estimate the impact of the reservation policy on repaired irrigation system and drinking water facilities

# Simple univariate regression
women %>%
  with(summary(lm(water ~ reserved)))

women %>%
  ggplot(aes(x = reserved, y = water)) +
  geom_smooth(stat = "smooth", method = "lm") +
  theme_minimal() +
  theme(axis.text=element_text(size=20), # Make sure texts and titles are legible
        panel.grid.minor = element_blank(), # remove specific grid lines
        axis.title=element_text(size=20, face="bold"))

women %>%
  with(summary(lm(irrigation ~ reserved)))

women %>%
  group_by(reserved) %>%
  summarise(irrigation = mean(irrigation),
            water = mean(water))

