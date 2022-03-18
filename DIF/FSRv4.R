install.packages("cSEM")
library(cSEM)

alp
alp <- na.omit(alp)

alpmodel <-
  '# latent variable definitions
  purpose =~ P1 + P2 + P3 + P4 + P5
  energy =~ E1 + E2 + E3 + E4 + E5
  warmth =~ W1 + W2 + W3 + W4 + W5
  enablement =~ EN1 +EN2 + EN3 + EN4 + EN5
  consideration =~ CON1 + CON2 + CON3 + CON4 + CON5
  ind contribution =~ INDCON1 + INDCON2 + INDCON3 + INDCON4 + INDCON5
  structure =~ S1 + S2 + S3 + S4 + S5
  change orient =~ CO1 + CO2 + CO3 + CO4 + CO5'


alpfit <- lavaan:::fsr(alpmodel, data = alp, cmd = "sem", fsr.method = "Croon")
summary(alpfit, standardized = T, fit = T)
fitMeasures(alpfit, c("cfi", "rmsea", "srmr"))
semPaths(alpfit,intercepts=FALSE, whatLabels = "est")


model <-
'# latent variable definitions
  purpose =~ P1 + P2 + P3 + P4 + P5
  energy =~ E1 + E2 + E3 + E4 + E5
  warmth =~ W1 + W2 + W3 + W4 + W5
  enablement =~ EN1 +EN2 + EN3 + EN4 + EN5
  consideration =~ CON1 + CON2 + CON3 + CON4 + CON5
  ind contribution =~ INDCON1 + INDCON2 + INDCON3 + INDCON4 + INDCON5
  structure =~ S1 + S2 + S3 + S4 + S5
  change orient =~ CO1 + CO2 + CO3 + CO4 + CO5
  
#structural model
  purpose ~ energy
  warmth ~ enablement
  consideration ~ ind contribution
  structure ~ change orient
'


fit <- lavaan:::fsr(model, data = alp, fsr.method = "Croon", se= "standard", output = "lavaan")
summary(fit, standardized = T, fit = T)
semPaths(alpfit)



lavaanPlot(model = fit, node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stars = c("regress"))


sem <- sem(model, data = alp)
summary(sem, standardized = T, fit = T)

lavaanPlot(model = sem, node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "grey"), coefs = TRUE, covs = TRUE, stars = c("regress"))


