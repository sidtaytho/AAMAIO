install.packages("cluster")
install.packages("factoextra")

library(ggplot2)    # graphics
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization

ea <- Employee_absenteeism #bring in da data
ea #lets see it

EA <- ea %>% group_by(DepartmentName) #group by department name
EA

nea <- EA %>% summarise(
  LengthService = mean(LengthService),
  AbsentHours = mean(AbsentHours),
  Age = mean(Age)) #lets create an object that shows the averages of each dept

df <- nea %>% remove_rownames %>% column_to_rownames(var="DepartmentName")
                                                
df <- na.omit(df) #get rid of missing data 
df <- scale(df) #standardize the continuous data
df #now we look

# compute gap statistic
gap_stat <- clusGap(df, FUN = kmeans, nstart = 20,
                    K.max = 10, B = 50) #this will look at wss and figure out how many clusters we need
# Print the result
print(gap_stat, method = "firstmax") #print it out
#vizualize
fviz_gap_stat(gap_stat) #plot time

# kmeans cluster analysis!
k3 <- kmeans(df, centers = 3, nstart = 20) #3 centers (3 clusters)
str(k3) #structre of it
k3 #print the cluster info 

kplot <- fviz_cluster(k3, data = df) #vizualization time! big dot is the center
kplot

#cluster centers
k3$centers

    
#Hierarchial clustering
    #Euclidean distance
    d <- dist(df, method = "euclidean") # distance matrix
    print(d, digits = 1)
    
    #hierarchial clustering
    fit <- hclust(d, method="ward")
    plot(fit) # display dendogram
    groups <- cutree(fit, k=3) # cut tree into 3 clusters
    # draw dendogram with red borders around the 3 clusters
    rect.hclust(fit, k=3, border="red")
    
    #silhoutte plot
    plot(silhouette(cutree(fit,3), d))
    
    #scree plot
    wss <- (nrow(df))*sum(apply(df,2,var))
    for (i in 2:20) wss[i] <- sum(kmeans(df, centers=i)$withinss)
    plot(1:20, wss, type="b", xlab="Number of Clusters",
         ylab="Within groups sum of squares")
  
