#import basic libraries for EDA
library(tidyverse)
library(dplyr)
library(Hmisc)
library(ggplot2)

#read data
data <- read_csv('TrainingWiDS2021.csv')

# let's have a quick look
View(data)
head(data)

# describe(data)
# summary(data)

str(data)

# let's see a few basic exploratory things
# 1.) what is the general volume of diabetes cases

data %>% 
  group_by(diabetes_mellitus) %>% 
  summarise(count = n())

# it's about 27%

# 2.) what's the distribution of diabetes cases across the genders recorded

data %>% 
  group_by(diabetes_mellitus, gender) %>% 
  summarise(count = n())

# more male cases, more male records

# we have a couple of missing values which likely won't show up on the plot due to their small volume so let's exclude them

data %>% 
  filter(!is.na(gender)) %>% 
  group_by(diabetes_mellitus, gender) %>% 
  summarise(count = n()) %>% 
  ggplot() +
  geom_col(aes(x=factor(diabetes_mellitus),count, fill=gender))+
  facet_wrap(~gender)+
  theme_light()

# first glimpse:it seems that females seem to have a slightly lower number of cases but
# there's more male data on record apparently

data %>% 
  group_by(gender) %>% 
  summarise(n())

## 66 observations' genders are missing. That's not a huge amount, let's drop the rows

data_clean <- data %>% 
  filter(!is.na(gender))

# 3.) check the age distribution

data_clean%>% 
  group_by(age,diabetes_mellitus) %>% 
  summarise(count=n())

data_clean %>% 
  ggplot(aes(age)) +
  geom_histogram(binwidth = 1)+
  theme_light()

# some ages are tagged as 0 which must be a mistake. I'm gonna drop these rows. 

#let's check how many instances of NA/0 ages we have
data_clean %>% 
  filter(is.na(age)|age == 0) %>% 
  group_by(age) %>% 
  summarise(n())

# 30 people are 0 years old. probably a mistake and was supposed to be NA. 
# NAs take about 3 % of the whole population. Assuming that our model would
# get rid of these values anyway, I'm just gonna drop them. 

data_clean <- data_clean %>% 
  filter(!is.na(age) & age != 0)

# check histogram again

data_clean %>% 
  ggplot(aes(age)) +
  geom_histogram(binwidth = 1)+
  theme_light()

# ok, seems a bit more reasonable. Let's move on.




