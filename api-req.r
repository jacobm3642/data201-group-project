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

    data <- content(response, "parsed")
    if (is.null(data) || is.null(data$value)) {
        cat("Error: No indicator data found.\n")
        return(NULL)
    } 

    return(data$value)
}

get_health_data <- function(countries, indicators) {
    base_url <- "https://ghoapi.azureedge.net/api/"
    
    data_frames <- list()
    
    for (country in countries) {
        for (indicator in indicators) {
            url <- paste0(base_url, indicator)

            cat("URL:", url, "\n")  # For debugging

            response <- GET(url)
            http_status(response)

            if (http_error(response)) {
                cat("L1 Error:", http_status(response)$reason, "\n")
            } else {
                data <- content(response, "parsed")
                if (length(data) > 0) {
                    data_frames[[paste(indicator)]] <- data
                } else {
                    cat("No data found for", country, "and", indicator, "\n")
                }
            }
        }
    }
    
    combined_data <- do.call(rbind, data_frames) 
    
    return(combined_data)
}
