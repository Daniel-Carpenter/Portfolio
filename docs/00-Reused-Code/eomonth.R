library(lubridate)

eomonth <- function(x, setToBegOfMonth = FALSE, parseDate = TRUE, ...) {
  
  # Parse the date if needed (default true). Requires Year, Month, and Date
  if (parseDate) {
    x = as.Date(x, format = ...)
  }
  
  # if using floor date, then do not offset the month
  if (setToBegOfMonth) {
    fun = floor_date
    dayOffset = 0
    
  # Else get end of month
  } else {
    fun = ceiling_date
    dayOffset = 1
  }
  
  # **Calculate end or beginning of month**
  eomonthDate = fun(x, 'month') - days(dayOffset)
  
  return(eomonthDate)
}


# Examples
# eomonth('2020-01-04')
# [1] "2020-01-31"
# 
# eomonth('2020-01-04', setToBegOfMonth = T)
# [1] "2020-01-01"
# 
# eomonth('January 2020 04', parseDate = T, format = '%B %Y %d', setToBegOfMonth = T)
# [1] "2020-01-01"

