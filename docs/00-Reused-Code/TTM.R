library(zoo)

# TTM Function (average or sum)

ttm <- function(vector, numPeriods = 12, fun = mean, calcsExcludeNA = TRUE) {
  ttmVector <- zoo::rollapply(data  = vector,         # The vector of data
                              width = numPeriods,     # Number of periods to total or average, etc.
                              FUN   = fun,            # Could use sum
                              na.rm = calcsExcludeNA, # Remove NA when using data

                              # Defaults
                              fill  = NA,     # first `numPeriods` will show as NA
                              align = 'right' # Starts from the first period
                              )

  return(ttmVector)
}


# # Examples
#
# library(tidyverse)
#
# ## Fake data
# set.seed(2024)
# numRows <- 1000
# df_base <- data.frame(id = 1:numRows,
#                  var1 = rnorm(numRows, 10,   1),
#                  var2 = rnorm(numRows, 100,  10),
#                  var3 = rnorm(numRows, 1000, 100),
#                  var4 = rnorm(numRows, 10000,1000)
# )
#
# df_base$id = as.factor(df_base$id)
#
# ## TTM (MEAN) Single Column
# df_mean <- df_base |> mutate(var1RollingAverage = ttm(var1))
# df_mean |> select(var1, var1RollingAverage) # display
#
#
# ## TTM (SUM) Single Column
# df_sum <- df_base |> mutate(var1RollingSum = ttm(var1, fun = sum))
# df_sum |> select(var1, var1RollingSum) # display
#
# ## TTM (MEAN) All Numeric Columns
# df_base |> mutate(across(where(is.numeric), ttm))
#
# ## TTM (SUM) All Numeric Columns
# df_base |> mutate(across(where(is.numeric), ~ttm(.x, fun = sum)))
