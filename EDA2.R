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

# as a rule of thumb learned from Stackoverflow, I'm going to pick out the numerical variables and impute the missing 
# values with the mean. 

## Imputing Factors

# let's check which variables are factors and how many values are missing. Then we can go ahead and create
# "Unknown" categories

df1 %>% 
  select_if(is.factor) %>% 
  plot_missing()

# notice that our target variable is not a factor. we'll have to change that.
df1$diabetes_mellitus <- factor(df1$diabetes_mellitus, labels = c("No","Yes"))

# We're good for most of these, only need to add "unknown" to icu_admit_source and ethnicity

df1$icu_admit_source <- factor(ifelse(is.na(df1$icu_admit_source), "Unknown", paste(df1$icu_admit_source)),
                                    levels = c(levels(df1$icu_admit_source),"Unknown"))

df1$ethnicity <- factor(ifelse(is.na(df1$ethnicity), "Unknown", paste(df1$ethnicity)),
                               levels = c(levels(df1$ethnicity),"Unknown"))


## Imputing numbers

df1 %>% 
  select_if(is.numeric) %>% 
  plot_histogram()


## check correlations
plot_correlation(na.omit(df1), type = "c")

# first glimpse, there seem to be a few values positively correlating with diabetes: glucose_apache, d1_glucose_max, age, bmi, weight, 
# arf_apache, bun_apache, creatinine_apache, d1_bun_max, d1_bun_min, d1_creatinine_max, d1_creatinine_min, d1_glucose_min, 
# d1_potassium_max

imputed_df <- df1 %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), mean(., na.rm = TRUE)))

# cool! We have our imputed & cleaned data!!

write.csv(imputed_df,"imputed_df.csv")

## Let's transform the holdout set's factors too for consistency

df2 <- read.csv("UnlabeledWiDS2021.csv")

df2$icu_admit_source <- factor(ifelse(is.na(df2$icu_admit_source), "Unknown", paste(df2$icu_admit_source)),
                               levels = c(levels(df2$icu_admit_source),"Unknown"))

df2$ethnicity <- factor(ifelse(is.na(df2$ethnicity), "Unknown", paste(df2$ethnicity)),
                        levels = c(levels(df2$ethnicity),"Unknown"))

df2$hospital_admit_source <- factor(ifelse(is.na(df2$hospital_admit_source), "Unknown", paste(df2$hospital_admit_source)),
                                    levels = c(levels(df2$hospital_admit_source),"Unknown"))

df2 %>% 
  select_if(is.factor) %>% 
  plot_missing()

# cool, let's save to csv
write_csv(df2, "unlabeled.csv")

