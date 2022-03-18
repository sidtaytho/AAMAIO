install.packages("CHAID", repos="http://R-Forge.R-project.org") #new package!
library(CHAID) #let's bring her in

chaiddata <- mentalhealth #renaming my data

str(chaiddata) #structure of the data (class, length, content)

#let's create some factors!
wi <- factor(work_interfere) #mental illness interference at work
gen <- factor(Gender) #gender
ctry <- factor(Country) #country 
fam <- factor(family_history) #family history of mental illness
trtmt <- factor(treatment) #sought treatment for mental illness
rw <- factor(remote_work) #working remotely at least 50% of time

#do these factors even exist? let's see..
is.factor(wi)
is.factor(gen)
is.factor(ctry)
is.factor(fam)
is.factor(trtmt)
is.factor(rw)

#setting those parameters!
ctrl<- chaid_control(alpha2 = 0.05, alpha4 = 0.05, 
                    minsplit = 2, minbucket = 2) 
#alpha2: merging threshold
#alpha4: splitting threshold
#minsplit: minimum # of observations for a split
#minbucket: minimum # of observations in a terminal node

#run the model!
mhchaid <- chaid(wi ~ gen + ctry + fam + trtmt + rw, data = chaiddata, 
                 na.action = na.omit, control = ctrl)
#mhchaid: this is the name of my model "Mental Health CHAID"
#wi: dependent variable
#gen + ctry + fam + trtmt + rw: independent variables
#chaiddata: my dataset
#na.action = na.omit: gets rid of NA data
#control = ctrl: specifying the parameters from the code above

print(mhchaid) #prints a table 
plot(mhchaid) #prints a plot

plot(mhchaid, type = "simple")
