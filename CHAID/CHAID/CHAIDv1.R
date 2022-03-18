library(rpart)
library(readr)
library(caTools)
library(dplyr)
install.packages('party')
install.packages('partykit')
install.packages('rpart.plot')
library(party)
library(partykit)
library(rpart.plot)

install.packages("CHAID", repos="http://R-Forge.R-project.org")
library(CHAID)

?chaid

attach(mentalhealth)
chaiddata <- mentalhealth
str(data)
summary(data)

ctrl<- chaid_control(alpha2 = 0.05, alpha3 = -1, alpha4 = 0.05,
                     minsplit = 20, minbucket = 7, minprob = 0.01,
                     stump = FALSE, maxheight = -1)

mhchaid <- chaid(work_interfere ~ Gender + Country + family_history + treatment, data = chaiddata, na.action = na.omit, 
      control = ctrl)
