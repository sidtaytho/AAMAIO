install.packages("cluster")
install.packages("factoextra")

library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

#DATASET #1
install.packages("cluster.datasets")
library(cluster.datasets)
data(us.south.demographics.1965)
demo <- us.south.demographics.1965 #rename
demo <- na.omit(demo) #remove missing values
demo <- scale(demo) #standardize the data, so its comparable
demo <- scale(demo[c('median.age', 'african.americans', 'income', 'urban.population')]) #lets just scale the numerical data
demo <- scale(demo) #try again - but w/ only numerical data
demo


#DATASET #2
EA
EA <- na.omit(EA)
EA <- scale(EA)
EA <- scale(EA[c('Age','AbsentHours')])
EA <- scale(EA)
EA

k2 <- kmeans(EA, centers = 2, nstart = 20)
str(k2)
k2

fviz_cluster(k2, data = EA)

# compute gap statistic
gap_stat <- clusGap(EA, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
# Print the result
print(gap_stat, method = "firstmax")

#vizualize
fviz_gap_stat(gap_stat)

# Compute k-means clustering with k = 6
final <- kmeans(EA, 4, nstart = 25)
print(final)

fviz_cluster(final, data = EA)


EA %>%
  mutate(Cluster = final$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")
