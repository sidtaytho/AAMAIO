install.packages("ltm")

library(psych)
library(ltm)


##2 parameter logistic model for dichotomous item

data("Abortion")
st <- Abortion
st

#Estimating the Parameters of the Two Parameter Logistic Model
## z1 - 1 latent variable
## IRT.param - keeps the values (difficiulty, discrimination) in traditional format (instead of slope intercept)
IRT <- ltm(st ~ z1, IRT.param = TRUE) 
summary(IRT)
coef(IRT)
#dif -  b: how hard the question is
#dis - a: how good the question is at figuring out people's ability


#Item Characteristic Curves - the probability of getting an item correct at specific ability
plot(IRT, type = "ICC")

#Item Information Curve - shows how well each item measures the trait
plot(IRT, type = "IIC")     

#Test Information Function - aggregates the IIC of all items
plot(IRT, type="IIC", items = 0)


#combination of response patterns & expected number of combinations
## z - ability estimate for each pattern
factor.scores(IRT)

#pattern of data
## Lz - the person's ability estimate
person.fit(IRT)

#probabilty that the items do not fit the model (can biased by large sample size)
item.fit(IRT)

##most people focus on those A and B estimates
