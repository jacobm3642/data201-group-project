library(tidyverse)

data <- read.csv("bmi_data.csv")

ggplot(data, aes(ParentLocation, FactValueNumeric)) + geom_point()
