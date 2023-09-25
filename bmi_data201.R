library(tidyverse)

data <- read.csv

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
ggplot(data_copy, aes(period, average_bmi, col=gender)) + geom_point(size=0.8)
#filter for each region:
data_copy %>% 
  filter(parent_location == 'Africa') %>%
  ggplot(aes(period, average_bmi, col=gender)) + geom_point(size=0.8)

data_copy %>% 
  filter(parent_location == 'Americas') %>%
  ggplot(aes(period, average_bmi, col=gender)) + geom_point(size=0.8)

data_copy %>% 
  filter(parent_location == 'Eastern Mediterranean') %>%
  ggplot(aes(period, average_bmi, col=gender)) + geom_point(size=0.8)

data_copy %>% 
  filter(parent_location == 'Europe') %>%
  ggplot(aes(period, average_bmi, col=gender)) + geom_point(size=0.8)

data_copy %>% 
  filter(parent_location == 'South-East Asia') %>%
  ggplot(aes(period, average_bmi, col=gender)) + geom_point(size=0.8)

data_copy %>% 
  filter(parent_location == 'Western Pacific') %>%
  ggplot(aes(period, average_bmi, col=gender)) + geom_point(size=0.8)



#boxplot of regions and bmi
ggplot(data, aes(ParentLocation, FactValueNumeric)) + geom_boxplot()
ggplot(data, aes(Period, FactValueNumeric, col=Location)) + geom_point()


#Only look at countries in south east asia:
south_east_asia <- data %>%
  filter(ParentLocation == 'South-East Asia')
ggplot(south_east_asia, aes(Period, FactValueNumeric, col=Location)) + geom_point()
