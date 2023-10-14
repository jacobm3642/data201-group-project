library(tidyverse)
library(dplyr)
source("./api-req.r")
source("./api_wrangler.r")
# devtools::install_github('jeremystan/tidyjson')
library(tidyjson)

indc <- get_all_indicators()
ind <- get_ind_data("air_10")