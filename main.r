library(tidyverse)
library(dplyr)
source("./api-req.r")
source("./api_wrangler.r")
# devtools::install_github('jeremystan/tidyjson')
library(tidyjson)


bmi <- get_ind_data(read_csv("./bmi_data.csv"))
diabetes_plan <- get_ind_data(read_csv("./diabetes_data.csv"))

bmi <- bmi %>% rename(Country_code = SpatialDimValueCode)

diabetes_plan <- diabetes_plan %>% rename(Country_code = SpatialDimValueCode)

diabetes_plan <- diabetes_plan %>% mutate(Value = as.factor(Value))

diabetes_bmi <- merge(diabetes_plan, bmi, by = c("Country_code", "Year"))

diabetes_bmi <- diabetes_bmi %>% select(Value.x, FactValueNumeric, Country_code, Year)

diabetes_bmi <- diabetes_bmi %>% rename(plan_res =Value.x,bmi = FactValueNumeric)

diabetes_bmi <- diabetes_bmi %>% filter(plan_res == "Yes" | plan_res == "No")

diabetes_bmi_2013 <- subset(diabetes_bmi, Year == 2013)
diabetes_bmi_2015 <- subset(diabetes_bmi, Year == 2015)

plot_2013 <- ggplot(diabetes_bmi_2013, aes(x = factor(plan_res), y = bmi)) +
    geom_boxplot() +
    labs(title = "Boxplot for BMI in 2013",
         x = "If the country had a plan to combat diabetes",
         y = "BMI by country") +
    theme_minimal()

plot_2015 <- ggplot(diabetes_bmi_2015, aes(x = factor(plan_res), y = bmi)) +
    geom_boxplot() +
    labs(title = "Boxplot for BMI in 2015",
         x = "If the country had a plan to combat diabetes",
         y = "BMI by country") +
    theme_minimal()

# Arrange the plots in one panel
grid.arrange(plot_2013, plot_2015, ncol = 2)


#twobig %>%
#    filter(Value.x < 11, YEAR == 2011) %>%
#    ggplot(aes(x = Value.x, y = NumericValue.y , color = ParentLocation.x)) +
#    geom_point() +
#    scale_x_continuous(breaks = seq(min(twobig$Value.x), max(twobig$Value.x), by = 2)) +
#    labs(title = "Scatter Plot of BMI vs Health Care Spending by Percentage of GDP",
#         x = "Healthcare Spending by Percentage of GDP",
#         y = "Mean BMI")