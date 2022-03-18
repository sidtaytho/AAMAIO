install.packages("tidyLPA")

library(tidyLPA)
library(dplyr)
library(tidyverse)
library(mclust)

#dataset
ea <- Employee_absenteeism #bring in da data
ea #lets see it

set.seed(123)
ss <- sample(1:8336, 50)   # Take 50 random rows
df <- ea[ss, ]    # Subset the 50 rows
df

summary()
m3 <- df %>% #this symbol passes the results of one function to the next
  select(Age, LengthService, AbsentHours) %>%
  single_imputation() %>%
  scale() %>%
  estimate_profiles(1:3)
m3
# AIC and #BIC are the most common used fit index
## smallest AIC/BIC value best representation
### use them both
#### entropy = certainty
plot_profiles(m3)

summary()
m3 <- df %>% #this symbol passes the results of one function to the next
  select(Age, LengthService, AbsentHours) %>%
  single_imputation() %>%
  scale() %>%
  estimate_profiles(3)
m3
plot_profiles(m3)

get_data(m3) #prints the data out if you wanted to use it for subsequent research
get_fit(m3) #extra fit indexes
get_estimates(m3) #means and standard error of data

