library(tidyverse)

data <- read.csv("bmi_data.csv")

#boxplot of regions and bmi
ggplot(data, aes(ParentLocation, FactValueNumeric)) + geom_boxplot()

ggplot(data, aes(Period, FactValueNumeric, col=Location)) + geom_point()


#Only look at countries in south east asia:
south_east_asia <- data %>%
  filter(ParentLocation == 'South-East Asia')
ggplot(south_east_asia, aes(Period, FactValueNumeric, col=Location)) + geom_point()
