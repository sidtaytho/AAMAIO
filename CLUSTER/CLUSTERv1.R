install.packages("cluster")
install.packages("factoextra")

library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

#DATASET
install.packages("cluster.datasets")
library(cluster.datasets)
data(us.south.demographics.1965)
demo <- us.south.demographics.1965 #rename
demo <- na.omit(demo) #remove missing values
demo <- scale(demo) #standardize the data, so its comparable
demo <- scale(demo[c('population.density','median.age', #lets just scale the numerical data
            'urban.population','rural.population','income')])
demo <- scale(demo) #try again - but w/ only numerical data
demo

k5 <- kmeans(demo, centers = 5, nstart = 25)
str(k5)
k5

fviz_cluster(k5, data = demo)

# compute gap statistic
gap_stat <- clusGap(demo, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
# Print the result
print(gap_stat, method = "firstmax")

#vizualize
fviz_gap_stat(gap_stat)

# Compute k-means clustering with k = 6
final <- kmeans(demo, 6, nstart = 25)
print(final)

fviz_cluster(final, data = demo)



us.south.demographics.1965 %>%
  mutate(Cluster = final$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")
