library(httr)

get_all_countries <- function() {
    url <- "https://ghoapi.azureedge.net/api/DIMENSION/COUNTRY/DimensionValues"

    response <- GET(url)
  
    data <- content(response, "parsed")
    if (is.null(data) || is.null(data$value)) {
        cat("Error: No country data found.\n")
        return(NULL)
    }
    
    return(data$value)
}

get_all_indicators <- function() {
    url <- "https://ghoapi.azureedge.net/api/Indicator"
    
    response <- GET(url)

    cat(url)

    data <- content(response, "parsed")
    if (is.null(data) || is.null(data$value)) {
        cat("Error: No indicator data found.\n")
        return(NULL)
    }

    return(data$value)
}

get_health_data <- function(indicator, use_athena) {
    if (use_athena == 1) {
        base_url <- "http://apps.who.int/gho/athena/api/"
        url <- paste0(base_url, "GHO/", indicator, ".xml")
    } else if (use_athena == 0) {
        base_url <- "https://ghoapi.azureedge.net/api/"
        url <- paste0(base_url, indicator)
    } else {
        cat("Invalid argument use 1 for Athena API or 0 for Azure API.")
        return(NULL)
    }
    
    cat("URL:", url, "\n")  # For debugging
    
    response <- GET(url)
    
    if (http_error(response)) {
        cat("L1 Error:", http_status(response)$reason, "\n")
        return(NULL)
    } else {
        return(content(response, "parsed"))
    }
}
