# Libraries
library(ggplot2)

# Create data
ea <- Employee_absenteeism

EA <- ea %>% group_by(DepartmentName) #group by department name

data <- EA %>% summarise(
  LengthService = mean(LengthService),
  AbsentHours = mean(AbsentHours),
  Age = mean(Age)) #lets create an object that shows the averages of each dept

data <- data.frame(x = data$DepartmentName, y = data$AbsentHours)

p <- data %>% 
  arrange(x) %>%
  mutate(y = factor(y, levels = y)) %>%
ggplot(aes(x=x, y=y)) +
  geom_point() + 
  geom_segment(aes(xend=x, yend=0), color = "grey") +
  geom_point(color = "blue", size = 4) +
  theme_light() +
  coord_flip() +
  theme(panel.grid.major = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank()) +
  scale_y_discrete(labels = function(y) round(as.numeric(y), digits=0)) +
  xlab("") +
  ylab("Hours absent from work")
p
                     