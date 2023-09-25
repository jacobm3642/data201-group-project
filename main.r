library(tidyverse)
library(dplyr)
source("./api-req.r")
# devtools::install_github('jeremystan/tidyjson')
library(tidyjson)

int <- get_all_indicators()

poilo <- get_health_data("AIR_42")

parsced <- poilo %>% gather_array %>% spread_all %>% select(Date, Low, NumericValue, SpatialDim)

parsced <- parsced %>% select(Date, Low, NumericValue, SpatialDim)

parsced %>% ggplot(aes(Low, NumericValue, col=factor(SpatialDim), show.legend = FALSE)) + geom_point() 

countrys <- get_all_countries() %>% spread_all()