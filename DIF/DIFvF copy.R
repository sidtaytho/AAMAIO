install.packages("difR")
install.packages("eRm")

library(psych)
library(ltm)
library(difR)
library(eRm)

data("Abortion")
st <- Abortion
st
##need a group column for the DIF - lets do gender
set.seed(123)
gen <- sample(1:2, 379, replace = TRUE)
gen #1 = female; 2 = male
st$gen <- gen #add it to my st dataset
st


#1PL/Rasch model
rm <- rasch(st[1:4]) ##conditional maximum likelihood estimation of item parameters
summary(rm)

#Plot Rasch Model
plot(rm,type=c("ICC")) ##ICC
plot(rm,type=c("IIC")) ##IIC
plot(rm,type=c("IIC"), items=0) ##IIC

rm <- rasch(st) 

#testing for unidimensionality
test <- unidimTest(rasch(st[1:4]))
test

plot(test, type = "b", pch = 1:2)
legend("topright", c("Real Data", "Average Simulated Data"), lty = 1, 
       pch = 1:2, col = 1:2, bty = "n")


#DIF - difLord
dif <- difLord(st, group = gen, focal.name = 1, model = "1PL", purify = TRUE)
dif

#plotting the results
plot(dif)

#look at some other methods of detecting DIF
dichoDif(st, group = gen, focal.name = 1, model = "1PL", purify = TRUE, method = c("Lord","Raju","MH"))

