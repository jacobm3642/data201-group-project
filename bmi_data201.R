library(tidyverse)

data <- read.csv('bmi_data.csv')

#Tidying the data:

#Give the columns more meaningful names:
data_copy <- data %>%
  rename(gender = Dim1,
         gender_code = Dim1ValueCode,
         age_group = Dim2,
         age_group_code = Dim2ValueCode,
         average_bmi = FactValueNumeric,
         minimum_bmi = FactValueNumericLow,
         maximum_bmi = FactValueNumericHigh)

#Delete Unused Columns:
data_copy <- data_copy %>%
  subset(select = -c(Dim3.type,
         Dim3,
         Dim3ValueCode,
         DataSourceDimValueCode,
         DataSource,
         FactValueNumericPrefix,
         FactValueUoM,
         FactValueNumericLowPrefix,
         FactValueNumericHighPrefix))

#Make all column names have snake_case naming style:
data_copy <- data_copy %>%
  rename(indicator_code = IndicatorCode,
         indicator = Indicator,
         value_type = ValueType,
         parent_location_code = ParentLocationCode,
         parent_location = ParentLocation,
         location_type = Location.type,
         location_code = SpatialDimValueCode,
         location = Location,
         period_type = Period.type,
         period = Period,
         is_latest_year = IsLatestYear,
         value = Value,
         value_translation_id = FactValueTranslationID,
         comments = FactComments,
         language = Language,
         date_modified = DateModified)



#Investigate if different genders have increased bmi:
ggplot(subset(data_copy, !is.na(average_bmi)), aes(period, average_bmi, col=gender)) + geom_point(size=0.8) +
  ggtitle("Average bmi from 1975 to 2016")

###################FILTER FOR EACH REGION:######################################

####AFRICA:####

data_copy %>% 
  filter(parent_location == 'Africa') %>%
  ggplot(aes(period, average_bmi, col=gender)) + geom_point(size=0.8) + 
  ggtitle("Average bmi in Africa from 1975 to 2016")

data_copy %>%
  filter(parent_location == 'Africa',
         period == 1975) %>%
  ggplot(aes(gender, average_bmi)) + geom_boxplot() + 
  ggtitle('Average bmi for each gender in Africa recorded in 1975') + ylim(17.0, 30.0)

data_copy %>%
  filter(parent_location == 'Africa',
         period == 2016) %>%
  ggplot(aes(gender, average_bmi)) + geom_boxplot() +
  ggtitle('Average bmi for each gender in Africa recorded in 2016') + ylim(17, 30)

####AMERICA:####

data_copy %>% 
  filter(parent_location == 'Americas') %>%
  ggplot(aes(period, average_bmi, col=gender)) + geom_point(size=0.8) +
  ggtitle("Average bmi in America from 1975 to 2016") 

data_copy %>%
  filter(parent_location == 'Americas',
         period == 1975) %>%
  ggplot(aes(gender, average_bmi)) + geom_boxplot() + 
  ggtitle('Average bmi for each gender in America recorded in 1975') +  ylim(17, 30)

data_copy %>%
  filter(parent_location == 'Americas',
         period == 2016) %>%
  ggplot(aes(gender, average_bmi)) + geom_boxplot() + 
  ggtitle('Average bmi for each gender in America recorded in 2016') +  ylim(17, 30)


####EASTERN MEDITERRANIEAN####

data_copy %>% 
  filter(parent_location == 'Eastern Mediterranean') %>%
  ggplot(aes(period, average_bmi, col=gender)) + geom_point(size=0.8) +
  ggtitle("Average bmi in the Eastern Mediterranean from 1975 to 2016")

data_copy %>%
  filter(parent_location == 'Eastern Mediterranean',
         period == 1975) %>%
  ggplot(aes(gender, average_bmi)) + geom_boxplot() + 
  ggtitle('Average bmi for each gender in the Eastern Mediterranean recorded in 1975') +  ylim(17, 30)

data_copy %>%
  filter(parent_location == 'Eastern Mediterranean',
         period == 2016) %>%
  ggplot(aes(gender, average_bmi)) + geom_boxplot() + 
  ggtitle('Average bmi for each gender in the Eastern Mediterranean recorded in 2016') +  ylim(17, 30)


#####EUROPE#####

data_copy %>% 
  filter(parent_location == 'Europe') %>%
  ggplot(aes(period, average_bmi, col=gender)) + geom_point(size=0.8) +
  ggtitle("Average bmi in Europe from 1975 to 2016")

data_copy %>%
  filter(parent_location == 'Europe',
         period == 1975) %>%
  ggplot(aes(gender, average_bmi)) + geom_boxplot() + 
  ggtitle('Average bmi for each gender in Europe recorded in 1975') +  ylim(17, 30)

data_copy %>%
  filter(parent_location == 'Europe',
         period == 2016) %>%
  ggplot(aes(gender, average_bmi)) + geom_boxplot() + 
  ggtitle('Average bmi for each gender in Europe recorded in 2016') +  ylim(17, 30)


#####SOUTH-EAST ASIA#####

data_copy %>% 
  filter(parent_location == 'South-East Asia') %>%
  ggplot(aes(period, average_bmi, col=gender)) + geom_point(size=0.8) +
  ggtitle("Average bmi in South East Asia from 1975 to 2016")

data_copy %>%
  filter(parent_location == 'South-East Asia',
         period == 1975) %>%
  ggplot(aes(gender, average_bmi)) + geom_boxplot() + 
  ggtitle('Average bmi for each gender in South East Asia recorded in 1975') +  ylim(17, 30)

data_copy %>%
  filter(parent_location == 'South-East Asia',
         period == 2016) %>%
  ggplot(aes(gender, average_bmi)) + geom_boxplot() + 
  ggtitle('Average bmi for each gender in South East Asia recorded in 2016') +  ylim(17, 30)



#####WESTERN PACIFIC#####

data_copy %>% 
  filter(parent_location == 'Western Pacific') %>%
  ggplot(aes(period, average_bmi, col=gender)) + geom_point(size=0.8) +
  ggtitle("Average bmi in the Western Pacific from 1975 to 2016")

data_copy %>%
  filter(parent_location == 'Western Pacific',
         period == 1975) %>%
  ggplot(aes(gender, average_bmi)) + geom_boxplot() + 
  ggtitle('Average bmi for each gender in the Western Pacific recorded in 1975') +  ylim(17, 30)
  
  data_copy %>%
  filter(parent_location == 'Western Pacific',
         period == 2016) %>%
  ggplot(aes(gender, average_bmi)) + geom_boxplot() + 
  ggtitle('Average bmi for each gender in the Western pacific recorded in 2016') +  ylim(17, 30)





#boxplot of regions and bmi
ggplot(data, aes(ParentLocation, FactValueNumeric)) + geom_boxplot()
ggplot(data, aes(Period, FactValueNumeric, col=Location)) + geom_point()


#Only look at countries in south east asia:
south_east_asia <- data %>%
  filter(ParentLocation == 'South-East Asia')
ggplot(south_east_asia, aes(Period, FactValueNumeric, col=Location)) + geom_point()
