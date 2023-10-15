library(tidyverse)
library(dplyr)
source("./api-req.r")
source("./api_wrangler.r")
# devtools::install_github('jeremystan/tidyjson')
library(tidyjson)

indc <- get_all_indicators()
human_devolpent_index <- get_ind_data("GHED_CHEGDP_SHA2011")
bmi <- get_ind_data("NCD_BMI_MEAN")
summary(bmi)

twobig <- merge(human_devolpent_index, bmi , by = c("country_codes", "YEAR"))

twobig <- twobig %>% mutate(Value.x = as.numeric(Value.x))

twobig %>%
    filter(Value.x < 11, YEAR == 2011) %>%
    ggplot(aes(x = Value.x, y = NumericValue.y , color = ParentLocation.x)) +
    geom_point() +
    scale_x_continuous(breaks = seq(min(twobig$Value.x), max(twobig$Value.x), by = 2)) +
    labs(title = "Scatter Plot of BMI vs Health Care Spending by Percentage of GDP",
         x = "Healthcare Spending by Percentage of GDP",
         y = "Mean BMI")