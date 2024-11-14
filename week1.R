
install.packages('devtools')
install.packages('tidyverse')

library(devtools)
library(tidyverse)


############################
# Loading and saving data
############################

resume = read.csv('resume.csv')
# write.csv(resume, 'resume.csv', row.names = FALSE)

############################
# Summarising data
############################

dim(resume) # rows and columns
head(resume) # display first few rows

tail(resume) # display last few rows
summary(resume) # simple descriptive statistics

############################
# how many African-American sounding surnames receive a call back?

resume %>%
  group_by(race, call) %>%
  count()

# HOWEVER...
# Group_by() should usually be followed by summarise()
resume %>%
  group_by(race, call) %>%
  summarise(n = n())

resume %>%
  group_by(race, call) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = call,
              values_from = n)

resume %>%
  group_by(race, call) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = call,
              values_from = n) %>%
  rename(no_callback = '0',
         callback = '1')

############################
# What if there is a sample bias - i.e. there were just more African-American
# sounding names?

# we can look at proportions of callbacks
# To create new variables, use the mutate() command

resume %>%
  group_by(race, call) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = call,
              values_from = n) %>%
  rename(no_callback = '0',
         callback = '1') %>%
  mutate(prop_callbacks = callback / sum(callback, no_callback))

# average callback rate
resume %>%
  summarise(m = mean(call))

resume %>%
  summarise(m = mean(call))

############################
# Logical values and operators
############################

# Data types

# Numeric - integers and decimals
x = 10
y = 5.5

class(x)
class(y)

# Integers - whole numbers
x = 42L
class(x)

# Characters - strings/text
x = 'QDA'
class(x)

# Logical - Boolean True/False
sunny = TRUE
class(sunny)

# Factors - Categorical data with levels, used for representing categories or groups.
gender <- factor(c("Male", "Female"))
class(gender)

# Data storage containers

# List - A collection of elements of any data type.
my_list <- list(1, "apple", TRUE)
my_list

# Vectors - lists that hold the same data type
num_vec <- c(1, 2, 3, 4)
num_vec

# Matrix - 2D array (think columns and rows without and column or row names)
mat <- matrix(1:9, nrow = 3, ncol = 3)
mat

# Array - A multi-dimensional generalization of matrices that can hold data in more than two
arr <- array(1:8, dim = c(2, 2, 2))
arr

# Dataframe - A table or 2D structure where columns can contain different types of data.
df <- data.frame(name = c("Alice", "Bob"), age = c(25, 30))
df

# We will mostly be using dataframes!!

# Lets focus on logical operators
sunny = TRUE
class(sunny)

as.integer(sunny)
as.integer(!sunny)

# Operators & and |
a = c(sunny, !sunny, !sunny)
b = c(sunny, !sunny, sunny)

a
b

a & b
a|b

############################
# Relational operators
############################

'Hello' == 'hello'
"Hello" != 'hello'

a == b
a != b

############################
# Subsetting
############################
# What is the callback rate for people with African American sounding names?
resume %>%
  filter(race == 'black') %>%
  summarise(m = mean(call))


# What about women with African American sounding names?
resume %>%
  filter(race == 'black' & sex == 'female') %>%
  summarise(m = mean(call))

resume %>%
  filter(race == 'black' | sex == 'female') %>%
  summarise(m = mean(call))

# Why are the call rates different?

# what if we just wanted the numerical value?
resume %>%
  filter(race == 'black' & sex == 'female') %>%
  summarise(m = mean(call)) %>%
  pull()

############################
# Simple conditional statements
############################

# use if else to create new variables - useul for creating new
resume %>%
  mutate(BlackFemale = if_else(race == "black" &
                                 sex == 'female', 1, 0))
# With different operators
age <- c(15, 25, 40, 65, 80)

# Create a new variable 'age_group' using a conditional when-like statement
age_group <- ifelse(age < 18, "Child",
                    ifelse(age < 60, "Adult", "Senior"))


# if_else() handles two conditions

# the case_when() function is a great way to handle multiple conditional statements

# Sample data
age <- c(15, 25, 40, 65, 80)

# Create a new variable 'age_group' using case_when()
age_group <- case_when(
  age < 18 ~ "Child",
  age < 60 ~ "Adult",
  age >= 60 ~ "Senior"
)

age_group

############################
# Factor variables
############################

# useful for categorical variables
resume %>%
  mutate(type = if_else(race == "black" &
                                 sex == 'female', "BlackFemale", ""))

# IMPORTANT - factors have levels that allow you to order categories
# usually done alphabetically, but can be manually specified.
# These determine the order they are plotted and analysed

x = resume %>%
  mutate(type = if_else(race == "black" &
                          sex == 'female',
                        "BlackFemale", "Others")) %>%
  mutate(type=fct_relevel(type, c("BlackFemale", "Others")))

levels(x$type)

x = resume %>%
  mutate(type = if_else(race == "black" &
                          sex == 'female',
                        "BlackFemale", "Others")) %>%
  mutate(type=fct_relevel(type, c("Others", "BlackFemale")))

levels(x$type)
