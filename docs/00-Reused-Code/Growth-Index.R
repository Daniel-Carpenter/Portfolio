library(tidyverse)

# Index function - must define the date (or index) column
# If long format, must define the grouping columns

indexGrowth <- function(df, dateCol, 
                        groupingCols = NULL, # set to a vector of the grouping columns, if in long format
                        isDateCol    = TRUE # TRUE if using date column for ID, else not date type.
) {
  
  if (isDateCol) {
    idColFun <- as.Date
  } else(
    idColFun <- as.factor
  )
  
  # Ensure date col is a factor, so not grouped
  df[[dateCol]] <- idColFun(df[[dateCol]])
  
  df_index <- df |>
    
    # Group data so that nothing needs to get moved
    # ONLY NEEDED IF DATA IS IN LONG FORMAT!
    group_by(across(all_of(groupingCols))) |> 
    arrange( across(all_of(groupingCols))) |> 
    
    # Index each value relative to the first, for each column
    mutate(across(where(is.numeric), ~(. / first(.)) * 100))
  
  # write.csv(cbind(df, df_index), 'forValidation.csv')
  
  return(df_index)
}


# # Example 1: Wide format
# 
# ## Fake data
# set.seed(2024)
# numRows <- 1000
# df <- data.frame(id = 1:numRows,
#                  var1 = rnorm(numRows, 10,   1),
#                  var2 = rnorm(numRows, 100,  10),
#                  var3 = rnorm(numRows, 1000, 100),
#                  var4 = rnorm(numRows, 10000,1000)
# )
# 
# ## Index in wide format
# df_index <- indexGrowth(df, dateCol = 'id', 
#                         isDateCol = FALSE) # False since not using Date index
# 
# # Example 2: Long format
# 
# ## Convert wide to long
# numericCols = colnames(df)[2:ncol(df)]
# metricGroupingColName = 'metric'
# df_long <- df |>
#   pivot_longer(cols = all_of(numericCols),
#                names_to = metricGroupingColName,
#                values_to = 'value')
# 
# ## Index in long format
# df_indexFromLong <- indexGrowth(df_long,
#                                 dateCol = 'id',
#                                 groupingCols = metricGroupingColName, 
#                                 isDateCol = FALSE # False since not using Date index
#                                 )
