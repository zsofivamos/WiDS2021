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

## Imputing Factors -------------------------------------------------------------------------------------------------------------------

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

# let's check which variables are factors and how many values are missing. Then we can go ahead and create
# "Unknown" categories when needed, or simply add them to the existing ones.

df1 %>% 
  select_if(is.factor) %>% 
  plot_missing()

# notice that our target variable is not on the list, so it's currently not encoded as a factor. we'll have to change that.
df1$diabetes_mellitus <- factor(df1$diabetes_mellitus, labels = c("No","Yes"))

# We're good for most of these, only need to check icu_admit_source and ethnicity
table(df1$icu_admit_source)

# this variable doesn't have an unknown category, let's add it in
df1$icu_admit_source <- factor(ifelse(is.na(df1$icu_admit_source), "Unknown", paste(df1$icu_admit_source)),
                                    levels = c(levels(df1$icu_admit_source),"Unknown"))

# check existing values of ethnicity
table(df1$ethnicity)

# here we do have an Other/Unknown value, let's just put all the NAs in that box 
df1$ethnicity <- factor(ifelse(is.na(df1$ethnicity), "Other/Unknown", paste(df1$ethnicity)),levels = c(levels(df1$ethnicity)))


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

write.csv(imputed_df,"imputed_df.csv",row.names=FALSE)

## Let's transform the holdout set too for consistency -----------------------------------------------------------------------------

df2 <- read.csv("UnlabeledWiDS2021.csv")

## drop variables
df2 <- df2 %>% 
  select(-features_to_drop)

## compare and clean up factors
df2 %>% select_if(is.factor) %>% colnames()

# icu_admit_source ---------------------

# compare
levels(df1$icu_admit_source)
levels(df2$icu_admit_source)

# we have an empty factor here, let's deal with it
library(forcats)
df2$icu_admit_source<- df2$icu_admit_source %>% fct_collapse("Unknown" = c("","Unknown"))
# reorder to match df1
df2$icu_admit_source <-fct_relevel(df2$icu_admit_source, c("Accident & Emergency","Floor","Operating Room / Recovery","Other Hospital","Other ICU","Unknown"))


# hospital_admit_source ---------------------------

levels(df1$hospital_admit_source)
levels(df2$hospital_admit_source)
str(df2$hospital_admit_source)
# assign Unknown value and use levels from df1 to align
df2$hospital_admit_source <- factor(ifelse(is.na(df2$hospital_admit_source)|df2$hospital_admit_source == "", "Unknown", paste(df2$hospital_admit_source)),
                                    levels = c(levels(df1$hospital_admit_source)))





# ethnicity ------------------------------------------

# ethnicity seems to have blanks in the test set as a factor level. we'll need to fix this
levels(df1$ethnicity)
levels(df2$ethnicity)

table(df2$ethnicity)
View(df2 %>% select(ethnicity))

# some blank rows were not picked up by the NA counter
# let's merge those into the unknown

df2$ethnicity <- df2$ethnicity %>% fct_collapse("Other/Unknown" = c("","Other/Unknown"))

# reorder to match df1
df2$ethnicity <-fct_relevel(df2$ethnicity, c("African American","Asian","Caucasian","Hispanic","Native American","Other/Unknown"))

# compare levels
levels(df1$ethnicity)
levels(df2$ethnicity)

# gender ----------------------------------------------

levels(df1$gender)
levels(df2$gender)

# ok, the holdout set has blanks, let's call those U here too
levels(df2$gender)[levels(df2$gender) == ""] <- "U"
# reorder
df2$gender <-fct_relevel(df2$gender, c("F","M","U"))


# icu_stay_type ----------------------------------------------

levels(df1$icu_stay_type)
levels(df2$icu_stay_type)
# wow, a match.

# icu_type -----------------------------------------------

levels(df1$icu_type)
levels(df2$icu_type)
# wooow, another match, incredible
                         
## Missing data in the test set
df2 %>% plot_missing()

# seems like we gotta impute the values here.

imputed_test_set <- df2 %>% 
  mutate_if(is.numeric, ~replace(., is.na(.), mean(., na.rm = TRUE)))

# cool, let's save to csv
write.csv(imputed_test_set, "unlabeled.csv", row.names=FALSE)

