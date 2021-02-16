library(tidyverse)
library(dplyr)

data <- read.csv("imputed_df.csv")
head(data)

# see if there are any interesting correlations going on
library(GGally)
ggcorr(data)

# same thing we've seen before: d1_glucose_max, glucose_apache, 

correlations <- cor(data %>% 
                      select_if(is.numeric) %>% 
                      # deleting readmission_status as it caused NA values in the matrix
                      # all of its values were 0
                      select(-c(readmission_status,X)))

# check what correlates with diabetes the most - looking at the top 15 values 
# selected 16 for the positive as diabetes will come first for sure
positive_correlations <- as.data.frame(correlations[order(correlations[,"diabetes_mellitus"],decreasing=TRUE),"diabetes_mellitus"][1:16])
negative_correlations <- as.data.frame(correlations[order(correlations[,"diabetes_mellitus"],decreasing=FALSE),"diabetes_mellitus"][1:15])

# create dfs from the correlation matrices
names(positive_correlations) <- "Coefficient"
positive_correlations <- rownames_to_column(positive_correlations, "Variable")

names(negative_correlations) <- "Coefficient"
negative_correlations <- rownames_to_column(negative_correlations, "Variable")

# bind them together
top_correlations <- rbind(positive_correlations, negative_correlations)

# sort by Coefficient & remove the label
top_correlations <- top_correlations[order(top_correlations[,"Coefficient"], decreasing = TRUE),][-1,]

## Baseline model

# I'm going to take the top 4 variables and add gender to them
# this way we can get a baseline model. 
# I'm going to add gender to the model as well. 

base_variables <- top_correlations[1][1:4,]

library(estimatr)
library(knitr)

lm_model <- lm_robust(diabetes_mellitus~ d1_glucose_max + glucose_apache + bmi + weight, data = data) %>% 
  tidy() %>% 
  kable(digits = 3)

# this is a pretty shitty model. not much information going on there. 


