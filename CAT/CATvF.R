install.packages("catR")
library(catR)

# Item bank generation with 100 items, 2PL model
bank <- genDichoMatrix(items = 100, model = "2PL", seed = 1)
bank

# Creating a starting list: 1 item with theta value of 0
start <- list(theta = 0)

# Creation of 'test' list: weighted likelihood estimation of provisional ability, 
# and MFI criterion for next item selection
test <- list(method = "WL", itemSelect = "MFI")

# Creation of a stopping rule: classification criterion, ability level for classifying = 2; sig level = .05
stop <- list(rule = "classification", thr = 2, alpha =0.05)

# CAT test
res <- randomCAT(2, bank, start = start, test = test, stop = stop)
res

# Plotting results
plot(res)
plot(res, ci = TRUE)
plot(res, ci = TRUE, trueTh = FALSE)
plot(res, ci = TRUE, classThr = 2)

