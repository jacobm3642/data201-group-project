source("api-req.r")
library(tidyjson)
library(dplyr)

col_names_dict <- c("Country", "Year", "Sex", "Age Group", "ENVCAUSE")
col_names_type <- c("factor", "int", "factor", "factor", "factor")

get_ind_data_local <- function(indicator) {
    ind <- indicator
    if (is.null(ind)){
        print("cleaning failed api returned null")
        return(NULL)
    }
    # ind <- ind %>% gather_array %>% spread_all
    col_names <- names(ind)
    print(col_names)
    for (col in col_names){
        if (grepl("Type", col)) {
            type_col <- col
            ref_col <- gsub("Type", "", col)
            type <- ind[[type_col]][1]
            indices <- which(col_names_dict == type)
            if (is.na(type)) {
                ind <- ind %>% select(-c(type_col, ref_col))
            } else if (length(indices) > 0 && !is.na(col_names_type[indices[1]])) {
                type_label <- col_names_type[indices[1]]
                # why doesn't r have switch statements i hate this style but it works
                if (type_label == "factor") {
                    ind <- ind %>% mutate(!!ref_col := as.factor(!!sym(ref_col)))
                    ind <- ind %>% rename(!!type := !!sym(ref_col))
                    ind <- ind %>% select(-type_col)
                } else if (type_label == "int") {
                    ind <- ind %>% mutate(!!ref_col := as.integer(!!sym(ref_col)))
                    ind <- ind %>% rename(!!type := !!sym(ref_col))
                    ind <- ind %>% select(-type_col)
                } 
            }
        }
    }
    ind <- ind %>% rename(Country_code = SpatialDimValueCode)
    ind <- remove_all_na_cols(ind)
    return(ind)
}

# i stole it from one of my old projects
remove_all_na_cols <- function(data) {
    na_counts <- colSums(is.na(data))
    na_cols <- names(na_counts[na_counts == nrow(data)])
    data <- data[, !names(data) %in% na_cols]
    return(data)
}