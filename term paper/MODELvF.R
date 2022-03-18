library(lavaan)
library(semTools)
library(psych)
library(semPlot)

##generate model
demo.model <- '
org commitment ~ .8*job satisfaction  #strength of regression with external criterion
intent to leave ~ -.5*org commitment

job satisfaction =~ .8*x1 + .8*x2 + .8*x3 + .8*x4 + .8*x5  #definition of factor f with loadings on 5 items
org commitment =~ .7*y1 + .7*y2 + .7*y3 + .7*y4 + .7*y5
intent to leave =~ .7*z1 + .7*z2 + .7*z3 + .7*z4 + .7*z5

x1 ~~ (1-.8^2)*x1 #residual variances. Note that by using 1-squared loading, we achieve a total variability of 1.0 in each indicator (standardized)
x2 ~~ (1-.8^2)*x2
x3 ~~ (1-.8^2)*x3
x4 ~~ (1-.8^2)*x4
x5 ~~ (1-.8^2)*x5

y1 ~~ (1-.8^2)*x1 #residual variances. Note that by using 1-squared loading, we achieve a total variability of 1.0 in each indicator (standardized)
y2 ~~ (1-.8^2)*x2
y3 ~~ (1-.8^2)*x3
y4 ~~ (1-.8^2)*x4
y5 ~~ (1-.8^2)*x5

z1 ~~ (1-.8^2)*x1 #residual variances. Note that by using 1-squared loading, we achieve a total variability of 1.0 in each indicator (standardized)
z2 ~~ (1-.8^2)*x2
z3 ~~ (1-.8^2)*x3
z4 ~~ (1-.8^2)*x4
z5 ~~ (1-.8^2)*x5
'

# generate data; note, standardized lv is default
set.seed(1234)
simData <- simulateData(demo.model, sample.nobs=200)
describe(simData, skew = FALSE)

#look at the data
View(simData)[,1:4]


model <- '
org commitment ~ job satisfaction # "~ is regressed on"
intent to leave ~ org commitment

job satisfaction =~ x1+ x2 + x3 + x4 + x5 # "=~ is measured by"
org commitment =~ y1+ y2 + y3 + y4 + y5
intent to leave =~ z1 + z2 + z3 + z4 + z5
'

##descriptive statistics for observed variables
describe(simData, skew = FALSE)

#factor scores
cfa <- cfa(model, simData)
lavPredict(cfa, method = "regression")


#FSR - SAM 
fit.sam <- lavaan:::sam(model, data = simData,
                        sam.method = "local")
coef(fit.sam)
summary(fit.sam, standardized = TRUE)
semPaths(fit.sam, "std", style = "LISREL", edge.label.cex = 1.5)
parameterEstimates(fit.sam, add.attributes = TRUE, ci = FALSE)
fitmeasures(fit.sam, c("cfi", "rmsea", "srmr"))

#correlations
inspect(fit.sam, "cor.all")

#SEM
sem.fit <- sem(model, data=simData)
coef(sem.fit)
summary(sem.fit, standardized=TRUE)
semPaths(sem.fit, "std", style = "LISREL", edge.label.cex = 1, residuals = FALSE)
fitmeasures(sem.fit, c("cfi", "rmsea", "srmr"))


compareFit(sem.fit, fit.sam)



