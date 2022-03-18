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

#testing for unidimensionality
test <- unidimTest(rasch(st[1:4]))
test

plot(test, type = "b", pch = 1:2)
legend("topright", c("Real Data", "Average Simulated Data"), lty = 1, 
       pch = 1:2, col = 1:2, bty = "n")


#1PL/Rasch model
rm <- RM(st[1:4]) ##conditional maximum likelihood estimation of item parameters

rm ##short summary
summary(rm) ##long summary
betas <- -coef(rm)
round(sort(betas), 2)


#Plot Rasch Model
plotjointICC(rm) ##ICC

plotPImap(rm) ##person-item map
plotPImap(rm, sorted = TRUE)


#Model Identification
round(sum(betas), 10) ##verifies identification constraint

tmp <- RM(st[1:4], sum0 = FALSE) 
round(coef(tmp), 2) 
##no difficulty parameter is returned for the first item 
summary(rm)
-sum(rm$etapar)


#Model Estimation
rm2 <- rasch(st[1:4]) ##fitting the model using marginal maximmum likelihood
rm2 ##the model is identified using normal distribution, so item parameters can be estimated because the mean of the person parameters is fixed to 0 (which wasn't the case in CML)

cor(coef(rm2)[, 1], betas)


#DIF - Mantel-Haenszel method
dif <- difMH(st, group = "gen", focal.name = 1)
dif

#plot
plot(dif)


#Lord's Chi-Square-Test
lord <- difLord(st, group = "gen", focal.name = 1, model = "1PL")
lord

#plot
plot(lord) #mine are good
plot(lord, plot = "itemCurve", item = 1) #if you wanted to look at one item


#Person Parameters
##maximum likelihood (ML)
pp <- person.parameter(rm)
pp_ml <- coef(pp)
##maximum a posteriori (MAP) & expected a posteriori (EAP)
pp_map <- factor.scores(rm2, method = "EB", resp.patterns = st[1:4])
pp_eap <- factor.scores(rm2, method = "EAP", resp.patterns = st[1:4])
tmp1 <- data.frame(ML = pp_ml, MAP = pp_map$score.dat$z1, EAP = pp_eap$score.dat$z1)
round(cor(tmp1), 4)

plot(tmp1[, 1:2])

#quick little 2pl interlude
IRT <- ltm(st ~ z1, IRT.param = TRUE) 

pp_2pl <- factor.scores(IRT, method = "EB", resp.patterns = st)
cor(pp_map$score.dat$z1, pp_2pl$score.dat$z1)

#Item and Test Information
IRT
plot(IRT, items = 1:4, type = "IIC") #item information curves
plot(IRT, items = 0, type = "IIC") #test information function
