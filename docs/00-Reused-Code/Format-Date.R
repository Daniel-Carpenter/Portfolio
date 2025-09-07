
# Function factory that creates other functions to format dates quickly
formatDateFunctionFactory <- function(dateFormat) {
  
  function(x, parseDate = TRUE, ...) {
    
    # Parse the date if needed (default true). Requires Year, Month, and Date
    if (parseDate) {
      x = as.Date(x, format = "%Y-%m-%d")  # Assuming input dates are in "YYYY-MM-DD" format
    }
    
    # Format the date according to the specified dateFormat
    formattedDate <- format(x, dateFormat)
    return(formattedDate)
  }
}

# Create specific date format functions
formatMonthYearFull   <- formatDateFunctionFactory(dateFormat = '%B %Y')
formatMonthYearAbrv_Y <- formatDateFunctionFactory(dateFormat = '%b-%Y')
formatMonthYearAbrv_y <- formatDateFunctionFactory(dateFormat = '%b-%y')

# remove the function factory
rm(formatDateFunctionFactory)

# Example usage
# formatMonthYearFull('2020-01-05')
# [1] "January 2020"
# 
# formatMonthYearAbrv_Y('2020-01-05')
# [1] "Jan-2020"
# 
# formatMonthYearAbrv_y('2020-01-05')
# [1] "Jan-20"
