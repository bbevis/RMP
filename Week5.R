###############################################
#
# Week 5 - More regressions
# Imai & Williams 4.4 - 4.5
#
###############################################

library(tidyverse)
# Regressions with multiple predictors

# data("social", package = 'qss')
# write.csv(social, 'social.csv', row.names = FALSE)
social = read.csv('social.csv')

# DV = voter turnout 2006
# Main IV = social pressure
# Investigate whether social pressure increases voter turnout

colnames(social)
# during the primary election in Michigan, they randomly assigned 4 messages
# for voters to receive. Note that in the control group, no messages were sent.
# Which message increased voter turnout the most?

unique(social$messages)
# A full explanation can be found in chapter 2.4.2.

# Expectations:
# 1. Civic duty messages increase voter turnout more than other messages
# 2. Pretreatment effects do not vary by message type.
#   E.g Age, hhsize and voter turnout in 2004 are not innfluenced by treatment

social %>%
  with(summary(lm(primary2006 ~ messages)))

# Notice that a coefficient is missing (only 3 are displayed). Why?

social = social %>%
  mutate(messages = fct_relevel(messages, c('Control', 'Civic Duty', 'Neighbors', 'Hawthorne')))

social %>%
  with(summary(lm(primary2006 ~ messages)))

# Validate with descriptive statistics
social %>%
  group_by(messages) %>%
  summarise(m = mean(primary2006),
            se = sd(primary2006) / sqrt(n())) %>%
  ggplot(aes(y = m, ymin = m - se, ymax = m + se, x = reorder(messages, desc(m)))) +
  geom_errorbar(width = .3, color = "#69b3a2") +
  geom_point(size = 4, color="#69b3a2") +
  theme_minimal() +
  coord_flip() +
  theme(legend.position = 'none',
        axis.text=element_text(size=20), # Make sure texts and titles are legible
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(), # remove specific grid lines
        axis.title=element_text(size=20, face="bold")) +
  labs(y="Proportion Voter Turnout",x="")

# controlling for other factors
social %>%
  with(summary(lm(primary2006 ~ messages)))

social %>%
  with(summary(lm(primary2006 ~ messages + yearofbirth + primary2004 + hhsize)))

social %>%
  with(summary(lm(primary2006 ~ messages*primary2004 + yearofbirth + hhsize)))
# This generates two main effects and one interaction effect
# The effectiveness of the messages now depend on 2004 primary turnout.

# When Neighbors == 0, 2004 voter turnout increases 2006 voter turnout by .14
# When primary2004 == 0, Neighbors increases voter turnout by .07
# When both Neighbors and primary2004 == 1, voter turn out increases by an additional .003:

# In other words, Neighbor messages becomes more effective if someone voted in 2004 by .003
# without 2004 turnout, 2006 turnout increases by .07

social %>%
  mutate(age = 2024 - yearofbirth) %>%
  with(summary(lm(primary2006 ~ I(age^2) + messages)))

# As age increases, voter turnout increases exponentially by .00003 percentage points

social %>%
  mutate(age = 2024 - yearofbirth) %>%
  with(summary(lm(hhsize ~ age)))

# log-log models allow you to interpret the coefficient as an elasticity
social %>%
  mutate(age = 2024 - yearofbirth) %>%
  with(summary(lm(log(hhsize) ~ log(age))))


#####################################
# Regression Discontinuity Design
#####################################

URL <- "https://raw.githubusercontent.com/DS4PS/pe4ps-textbook/master/data/RegDisc2.csv"
data <- read.csv( URL, stringsAsFactors=F )

# We are going to replicate a study conducted by Carpenter and Dobkin (2009)
# on the effect of alcohol consumption on mortality rates.
# Statistics related to the effect of alcohol consumption are worrisome,
# from high mortality rates due to car accidents to health related problems, especially among young adults.

# Data are made available by Josh Angrist and Steve Pischke here: http://masteringmetrics.com/resources/.

data = data %>%
  mutate(Treatment = ifelse(agecell > 21, 1, 0 ),
         age_c = agecell - 21)

reg1 = lm( all ~ Treatment + age_c, data = data ) # 	Overall mortality rate
reg2 = lm( mva ~ Treatment + age_c, data = data ) # Mortality rate for car accidents
reg3 = lm( alcohol ~ Treatment + age_c, data = data )
reg4 = lm( drugs ~ Treatment + age_c, data = data )

summary(reg1)
summary(reg2)


# We first predict the final exam grades based on our regression discontinuity model.
predfit = predict(reg2, data)

# We set colors for the graph
palette(c(adjustcolor("steelblue"),
            adjustcolor("darkred")))

# To differently color the treatment and control group
# we need to create a new Treatment variable that where the Control group = 2.
data = data %>%
  mutate(Treatment2 = ifelse(Treatment==0,2,1))

# Plot our observed value

data %>%
  ggplot(aes(x = age_c, y = mva, color=Treatment2)) +
  geom_point() +
  # geom_smooth(method = "lm", se = FALSE) +
  geom_abline() +
  geom_vline(xintercept=0, color="red") +
  theme_minimal() +
  theme(legend.position = 'None')
