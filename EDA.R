#import basic libraries for EDA
library(DataExplorer)
library(tidyverse)
library(dbplyr)
library(Hmisc)

#read data
data <- read_csv('TrainingWiDS2021.csv')

# let's have a quick look
View(data)
head(data)
# describe(data)
# summary(data)
# introduce(data)

# create summary report to have a better idea -we have a lot of variables
# summary_report <- create_report(data)

# check what's missing
plot_missing(data)

# we seem to have a large number of missing variables, let's filter for those that are have more than 50% missing
# so all in the bad & remove categories

features_to_drop <- profile_missing(data)%>% 
  filter(pct_missing>=0.50)

features_to_drop <- features_to_drop$feature

# let's remove these variables, we won't be able to use them
# I'm also dropping the column X1 as we already have encounter id

df1 <- data %>% 
  select(-c(features_to_drop,"X1"))

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

# where we have no information on hospital admit source, I'm just going to impute a new value 'Unknown'

df1$hospital_admit_source <- ifelse(is.na(df1$hospital_admit_source), "Unknown", df1$hospital_admit_source)

# this variable can now be removed from the missing list

blue_zone <- profile_missing(df1) %>% 
  filter(pct_missing > 0.05)

blue_zone <- blue_zone$feature

# how many columns is that
df1 %>% 
  select(blue_zone) %>% 
  ncol()

# 39. Let's check the distributions of these variables

df1 %>% 
  select(blue_zone) %>% 
  plot_histogram()

# observation: all variables either end in _min/_max or _apache. This could be something worth looking into

summary(df1$bun_apache)
summary(df1$bun_apache)

# impute the blue zone with the mean values


impute(df1$bun_apache, mean)
