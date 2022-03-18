# Libraries
library(ggplot2)
library(dplyr)
library(plotly)

# Create data
ea <- Employee_absenteeism

EA <- ea %>% group_by(DepartmentName) #group by department name

data <- EA %>% summarise(
  LengthService = mean(LengthService),
  AbsentHours = mean(AbsentHours),
  Age = mean(Age)) #lets create an object that shows the averages of each dept

data <- data.frame(x = data$DepartmentName, y = data$AbsentHours)
data

data <- data[order(data$y,decreasing = TRUE),]
data
             
#ggplot barplot
p <- ggplot(data, aes(x = x, y = y)) + 
  geom_bar(stat = "identity", alpha=.6, width = .5) +
  theme(legend.position="none") +
  coord_flip() +
  ylab("Hours absent from work") +
  xlab("") +
  scale_y_discrete(labels = function(y) round(as.numeric(y), digits=2)) +
  theme_classic()
p
ggplotly(p)


library(RColorBrewer)
coul <- brewer.pal(12, "Set3") 
par(mar=c(5.1,10,4.1,2.1))
#base r bar plot
barplot(height = data$y, names = data$x, 
        main = "Hours absent from work",
        xlab = "Hours",
        ylab = "",
        las = 1,
        col = coul,
        cex.names = .8,
        xlim = c(0,100),
        horiz = TRUE)

