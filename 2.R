library(tidyverse)

data2 <- read.csv('clean_my_bmi.csv')

ggplot(data2, aes(social_class, first_zBMI, col=gender)) + geom_jitter(width=0.2)

data2$social_class <- as.factor(data2$social_class)

ggplot(data2, aes(social_class, first_zBMI)) + geom_boxplot(fill='beige', col='black') +
  theme_bw() +
  ggtitle('BMI for each social class (1 = poor, 5 = wealthy)') +
  labs(y='BMI')

ggsave('social class.png', width=20, height=10, limitsize=FALSE)                                                                       
