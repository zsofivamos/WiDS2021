library(DataExplorer)
library(tidyverse)
library(dplyr)
library(Hmisc)
library(ggplot2)

# read in cleaner dataset
data <- read.csv("data_clean.csv")

# check basic stuff 
str(data)

# there seem to be X, X1 and encounter id fields assigned as some sort of index. Let's remove them if they aren't needed
nrow(data) == n_distinct(data$encounter_id)
# yup, good to go. let's drop the indeces

data <- data %>% 
  select(-c("X","X1"))

# View(data)


# check what's missing 
plot_missing(data)

# we seem to have a large number of missing variables, let's filter for those that are have more than 50% missing
# so all in the bad & remove categories

features_to_drop <- profile_missing(data)%>% 
  filter(pct_missing>=0.50)

features_to_drop <- features_to_drop$feature

# let's remove these variables, we won't be able to use them

df1 <- data %>% 
  select(-features_to_drop)

plot_missing(df1)

#now let's have a look at what variables we have in the blue zone

blue_zone <- profile_missing(df1) %>% 
  filter(pct_missing > 0.05)

blue_zone <- blue_zone$feature

df1 %>% 
  select(blue_zone) %>% 
  describe()

describe(df1)

# one variable seems to be categorical, let's check what we have there
sum(is.na(df1$hospital_admit_source))

str(df1$hospital_admit_source) ## it's a factor, so let's not screw that up while replacing
# let's see the range
table(df1$hospital_admit_source)

# impute the factor variable NAs and add another factor "Unknown"
df1$hospital_admit_source <- factor(ifelse(is.na(df1$hospital_admit_source), "Unknown", paste(df1$hospital_admit_source)),
levels = c(levels(df1$hospital_admit_source),"Unknown"))

# this variable should no longer be in the missing list

blue_zone <- profile_missing(df1) %>% 
  filter(pct_missing > 0.05)

blue_zone <- blue_zone$feature

# how many columns is that
df1 %>% 
  select(blue_zone) %>% 
  ncol()

# 37. Let's check the distributions of these variables

df1 %>% 
  select(blue_zone) %>% 
  plot_histogram() 

# as a rule of thumb learned from Stackoverflow, I'm going to pick out the numerical variables and impute the missing 
# values with the mean. 
