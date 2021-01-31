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

blue_zone

## MISSING APACHE SCORES -------------------------------------------

# check the apache related ones first
missing_apache_scores <- grep("apache",blue_zone, value = TRUE)

df1 %>% 
  select(missing_apache_scores) %>% 
  plot_histogram()

# 4 out of 7 apache scores seem to have a distribution skewing to the right, 
# the rest are closer to normal distribution
# skewed: bun_apache, creatinine_apache, glucose_apache, wbc_apache
# normal-ish: hematocrit_apache, sodium_apache, temp_apache

df1 %>% 
  select(missing_apache_scores) %>% 
  summary()

# library for most frequent value calculations
library(modeest)

# check most frequent value ebsides NA 
mfv(df1$bun_apache, na_rm = T)

# bun_apache: most people seem to be closer to the median value but there are some extreme cases - will impute with the median

mfv(df1$creatinine_apache, na_rm = T)

# creatinine apache: mfv is closer to median, will use that

mfv(df1$glucose_apache, na_rm = T)
summary(df1$glucose_apache)
# same for glucose_apache

#let's check one of the normal distributions

summary(df1$temp_apache)
summary(df1$sodium_apache)
summary(df1$hematocrit_apache)

# I'll impute the mean values for these ones

# just to check how many columns contain 'apache' lower case 
# grep("apache",colnames(df1), value = T)

# skewed: bun_apache, creatinine_apache, glucose_apache, wbc_apache
# let's impute accordingly
df1$bun_apache <- impute(df1$bun_apache, median)

# check to see if we changed the distribution significantly
df1$bun_apache %>% plot_histogram()

df1$creatinine_apache <- impute(df1$creatinine_apache, median)
df1$glucose_apache <- impute(df1$glucose_apache, median)
df1$wbc_apache <- impute(df1$wbc_apache, median)

# imputing the mean for the rest
# normal-ish: hematocrit_apache, sodium_apache, temp_apache
df1$hematocrit_apache <- impute(df1$hematocrit_apache, mean)
df1$sodium_apache <- impute(df1$sodium_apache, mean)
df1$temp_apache <- impute(df1$temp_apache, mean)

# missing should be zero
df1 %>% 
  select(missing_apache_scores) %>% 
  plot_missing()
  
## MISSING MIN-MAX VALUES --------------------------------------------

missing_min_max <- grep("_min|_max",blue_zone, value = TRUE)

df1 %>% 
  select(missing_min_max) %>% 
  plot_missing()

df1 %>% 
  select(missing_min_max) %>% 
  plot_histogram()

# df1$d1_glucose_max <- impute(df1$d1_glucose_max, median)
# df1$d1_glucose_min <- impute(df1$d1_glucose_max, median)
# df1$d1_bun_max <- impute(df1$d1_bun_max, median)
# df1$d1_bun_min <- impute(df1$d1_bun_min, median)
# df1$d1_creatinine_max <- impute(df1$d1_creatinine_max, median)
# df1$d1_creatinine_min <- impute(df1$d1_creatinine_min, median)
# df1$d1_platelets_max <- impute(df1$d1_platelets_max, median)
# df1$d1_platelets_min <- impute(df1$d1_platelets_min, median)
# df1$d1_wbc_max <- impute(df1$d1_wbc_max, median)
# df1$d1_wbc_min <- impute(df1$d1_wbc_min, median)




