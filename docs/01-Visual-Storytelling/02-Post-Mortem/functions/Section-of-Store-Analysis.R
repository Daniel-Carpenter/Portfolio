# Load packages and functions
source('docs//00-Reused-Code/Read-Library-and-Functions.R')


set.seed(1024) # All data is randomized! Seed set for reproducibility

# ==============================================================================
# section_of_store_analysis()
# Function that gets daily data by Store Section (operating_income_per_store_per_day, Avg. Bet). Plots Pre/Post Comparison.
# Returns: Data frame with Store Section daily data (operating_income_per_store_per_day, Avg. Bet, etc.) and plots
# How to Run: Make sure to open Store Section 7 project, then run file in 'Data_Pipeline' folder
# ==============================================================================

# The only reason this function might fail is if you do not have `plyr` installed.
# Do not include in library on execution since will compete with dplyr and dbplyr and cause issues
source('docs//01-Visual-Storytelling//02-Post-Mortem//functions//get_data_is.R')


# ==============================================================================
# INPUTS
# ==============================================================================

section_of_store_analysis <- function(

  date_max = Sys.Date() - days(1),  # Max Date to pull from. Default yesterday
  date_min = Sys.Date() - 365*1,    # Min Date to pull from. Default 1 year ago from today,

  date_omit_max = '1970-01-01',  # Max of date range to omit (e.g. for bad data)
  date_omit_min = '1970-01-01',  # Min of date range to omit (e.g. for bad data)

  selected_stores = c('Flagship Store'),    # Business to pull data for. Default flasgship

  # Comparison information for Plots
  comparison_date        = floor_date(as.Date(date_max), 'month'), # Day of event to show comparison. Default beg. of max date month
  num_months_prior_to_avg   = 12,      # Max Store Section 3-Months to Average for Signups comparison  Default 12 months.
  include_this_month_in_avg = FALSE,   # Should you include this month in the weekday average? Default no and recommended no.,
  color_of_metric = 'yellow',          # Color to make the metric of interest

  # Highlighted Store Section of interest
  highlightedsection_of_stores     = c("Store Section 1"),
  highlightedsection_of_stores_name = 'Highlighted Sections',

  # section_of_stores Close to the highlighted section_of_store
  comparisonsection_of_stores     = c("Store Section 3", "Store Section 4", "Store Section 5"), # Holds list of section_of_stores to group and compare to the highlighted section_of_store
  comparisonsection_of_stores_name = 'Sections Near Highlighted Section', # Name to appear on chart
  includePeersection_of_stores = TRUE, # if you want to include non-smoking section_of_stores in analysis
  peersection_of_stores     = c("Store Section 6", "Store Section 7"), # section_of_stores that are non-smoking
  peer_section_of_stores_name = 'Peer Sections',

  # Name for all other section_of_stores
  othersection_of_stores_name = 'All Other Sections' # Other section_of_stores (Name to appear on chart)
) {
  # Source Reused functions
  if (!require("DescTools")) install.packages("DescTools");  library(DescTools) # %like% function (similar to `like` in SQL)


  # ==============================================================================
  # IMMEDIATE OVERRIDES TO INPUTS
  # ==============================================================================

  # Ensure date inputs are dates
  date_max         = as.Date(date_max)
  date_min         = as.Date(date_min)
  date_omit_max = as.Date(date_omit_max)
  date_omit_min = as.Date(date_omit_min)
  comparison_date  = as.Date(comparison_date)

  # months to average for the chart - separate from inputs above since 12 is likely
  # !!Will not change the labels!!
  MONTHS_TO_AVERAGE = 12


  # ==============================================================================
  # MAKE CONNECTION TO TABLES
  # ==============================================================================

  # Data conection
  conn.section_of_store_daily <- get_data_is()



  # ==============================================================================
  # PULL IN DATA:
  # ==============================================================================


  # Pull in Supplier Performance (Daily) ---------------------------------
  df_section_of_store_daily.base <- conn.section_of_store_daily |>

    # Filter the data to date ranges
    filter((date_of_trx >= date_min & date_of_trx < date_max)) |>

    # Do not include the omission dates if not NA (i.e., changed from the default)
    filter(!(date_of_trx >= date_omit_min & date_of_trx <= date_omit_max),

           # Keep businesses from inputs at top
           store_id %in% selected_stores
    ) |>

    # Select only needed columns
    select(-c(expense, avg_transactions_amt)) |>

    collect()  # Pull data in


  df_section_of_store_daily <- df_section_of_store_daily.base |>

    # Convert to date
    mutate(date_of_trx = date(date_of_trx),

           # Remove Whitespace on the Supplier Data
           section_of_store_id = str_trim(section_of_store_id)) |>

    # Group section_of_stores by the highlighted comparison section_of_stores, as defined in the inputs
    mutate(section_of_store_id_grouping = if_else(section_of_store_id %in% highlightedsection_of_stores, highlightedsection_of_stores_name,
                                                  if_else(section_of_store_id %in% comparisonsection_of_stores, comparisonsection_of_stores_name,
                                                          if_else(includePeersection_of_stores
                                                                  & section_of_store_id %in% peersection_of_stores, peer_section_of_stores_name,
                                                                  othersection_of_stores_name                                                                        )
                                                  )
    ) ) |>

    # Do not want data before entry with section_of_stores, since there is unwanted noise
    # E.g., coin testing going on
    filter(!(date_of_trx < comparison_date & section_of_store_id %in% highlightedsection_of_stores))


  # ==============================================================================
  # PLOTS
  # ==============================================================================

  # ------------------------------------------------------------------------------
  # operating_income_per_store_per_day | Store Comparison from Historical Averages
  # ------------------------------------------------------------------------------

  # DATA -------------------------------------------------------------------------

  # calculate the First and last day to average by weekday
  weekday_avg_r12 = floor_date(comparison_date - months(MONTHS_TO_AVERAGE),     unit='month')
  weekday_avg_r6  = floor_date(comparison_date - months(MONTHS_TO_AVERAGE / 2), unit='month')

  date_max_to_avg = as.Date(ifelse(include_this_month_in_avg,
                                   comparison_date,
                                   floor_date(comparison_date, unit='month') - days(1)),
                            origin="1970-01-01" # need to supply origin date
  )

  # Avg. Bet & operating_income_per_store_per_day Summary Data ===========================================

  # Post Entry operating_income_per_store_per_day Dataset --------------------------------------------
  df_plot_post_entry <- df_section_of_store_daily |>

    # Filter to date range
    filter(date_of_trx >= comparison_date) |>

    # Group by property and store class to get avg operating_income_per_store_per_day
    group_by(store_id, section_of_store_id_grouping) |>
    summarise(post_operating_income_per_store_per_day  = sum(operating_income)  / sum(num_days_store_was_open),
              post_discount  = sum(discounts) / sum(num_days_store_was_open),
              post_revenue  = sum(revenue)  / sum(transactions))

  # Prior 6 month average operating_income_per_store_per_day (relative to comparison date) ---------------
  df_plot_prior_6_mo_summary <- df_section_of_store_daily |>

    # Filter to date range
    filter(date_of_trx >= weekday_avg_r6 & date_of_trx <= date_max_to_avg) |>

    # Group by property and store class to get avg operating_income_per_store_per_day
    group_by(store_id, section_of_store_id_grouping) |>
    summarise(Prior6_operating_income_per_store_per_day = sum(operating_income)  / sum(num_days_store_was_open),
              Prior6_discount = sum(discounts) / sum(num_days_store_was_open),
              Prior6_revenue = sum(revenue)  / sum(transactions))

  # Prior 12 month average operating_income_per_store_per_day (relative to comparison date) --------------
  df_plotPrior12MoSummary <- df_section_of_store_daily |>

    # Filter to date range
    filter(date_of_trx >= weekday_avg_r12 & date_of_trx <= date_max_to_avg) |>

    # Group by property and store class to get avg operating_income_per_store_per_day
    group_by(store_id, section_of_store_id_grouping) |>
    summarise(Prior12_operating_income_per_store_per_day = sum(operating_income)  / sum(num_days_store_was_open),
              Prior12_discount = sum(discounts) / sum(num_days_store_was_open),
              Prior12_revenue = sum(revenue)  / sum(transactions))


  roundToNearest = 5

  # Join the 3 operating_income_per_store_per_day datasets together then put in single col for plots -----
  df_plot_summary <- df_plot_post_entry |>

    # Join the R6 mo-avg operating_income_per_store_per_day dataset
    left_join(df_plot_prior_6_mo_summary,
              by = c('store_id', 'section_of_store_id_grouping')) |>

    # Join the R12 mo-avg operating_income_per_store_per_day dataset
    left_join(df_plotPrior12MoSummary,
              by = c('store_id', 'section_of_store_id_grouping')) |>

    # For F/operating_income_per_store_per_day variables, round up to the nearest 5
    mutate_at(vars(ends_with('operating_income_per_store_per_day'), ends_with('discount')),
              ~plyr::round_any(., accuracy = roundToNearest, f = ceiling)) |>

    # For revenue variables, round to 2 sig figs
    mutate_at(vars(ends_with('revenue')),
              ~round(., 2)) |>

    # Put F/operating_income_per_store_per_day and revenue Values into single col
    # Add 2 cols that contain the comparison period and the metric.
    # Key is that Prior12 and MetricName are separated by '_'. See data
    pivot_longer(cols      = !c(store_id, section_of_store_id_grouping),
                 names_to  = c('comparison_period', 'METRIC'),
                 names_sep = '_',
                 values_to = 'VALUE') |>

    mutate(comparison_period = if_else(comparison_period == 'Prior12', '36-mo. Avg.',
                                       if_else(comparison_period ==  "Prior6", '12-mo. Avg.',
                                               "Post Entry"))) |>

    # Get rid of the rows if the Store Section did not exist before the comparison date
    drop_na()


  # PLOT revenue/F/operating_income_per_store_per_day Summary -------------------------------------------------


  # Change the names on the plots --------------------------------------------

  # Comparison Period: cleaned names
  df_plot_summary$comparison_period <- fct_relevel(df_plot_summary$comparison_period,
                                                   "36-mo. Avg.", "12-mo. Avg.", "Post Entry" )
  # Now Rename the levels
  renamedLabels.Legend <- c('Prior 36-mo. Avg.',  'Prior 12-mo. Avg.',   'Post-Entry')

  # Rename Values
  renamedLabels.SummaryFacets <- c("Op. Income per Store", "Discounts per Store", "Revenue per Store")
  df_plot_summary$METRIC    <- factor(df_plot_summary$METRIC  ,
                                     levels = unique(df_plot_summary$METRIC), # Old Names
                                     labels = renamedLabels.SummaryFacets    # New Names
  )

  # Plot Colors used in summary and store class chart
  PRE_COMPARISON_COLOR  = 'grey87'
  colorsOfLabels       <- c(PRE_COMPARISON_COLOR,
                            paste0(styles::scale_dc('fill', 'gray')),
                            paste0(styles::scale_dc('fill', color_of_metric)))


  DATE_FORMAT = '%b %d, %Y' # Date format for the plot caption


  # Plot of operating_income_per_store_per_day -------------------------------------------------------------
  plot.Summary <- df_plot_summary |>
    ggplot(aes(x     = comparison_period,
               color = comparison_period,
               fill  = comparison_period,
               y     = VALUE
    )
    ) +

    # Facet by property and store class
    facet_grid(rows = vars(METRIC),
               cols = vars(section_of_store_id_grouping),
               scales = 'free',    # Everything free
               space = "free_x") + # Shrink the x axis since some data

    geom_col(alpha = 0.75) +
    geom_text(aes(label = round(VALUE, digits = 2)), # show 2 significant trailing digits
              color = getTextCol(),
              nudge_y = df_plot_summary$VALUE * 0.06) +

    # Title, Axis's, etc.
    labs(x        = '\n Comparison Periods',
         y        = '',
         title    = 'Section of Stores See Consistent Shift\n',
         subtitle = paste0('Operating Income, Discounts, and Revenue per Store per Day Comparison\n'),
         caption  = paste0('\n"', comparisonsection_of_stores_name, '": ', paste(comparisonsection_of_stores, collapse = ", "),
                           if (includePeersection_of_stores){ paste0('. "', peer_section_of_stores_name, '": ', paste(peersection_of_stores, collapse = ", ")) },
                           '\n"', othersection_of_stores_name, '" are any other section of stores not defined in the ', highlightedsection_of_stores_name, ' or store section mentioned above.',
                           '\nPlease note potential for high volaitility until increased accumulation of days since exansion.',
                           '\nAverage Operating Income per Store per Day calculation:\nAverage Operating Income divided by count of all Active Stores; then divided by sum of days that each store was open.',
                           '\nPrior 12-mo. avg. period: ', format(weekday_avg_r12, DATE_FORMAT), ' - ', format(as.Date(comparison_date), DATE_FORMAT),
                           '; Prior 6-mo. avg. period: ', format(weekday_avg_r6, DATE_FORMAT), ' - ', format(as.Date(comparison_date), DATE_FORMAT),
                           '; Post-Entry period: '  ,  format(as.Date(comparison_date),    DATE_FORMAT), ' - ', format(date_max - days(1),     DATE_FORMAT),
                           if (date_omit_min != date_omit_max) { paste('\nDates exluded due to noise in data:', format(date_omit_min, DATE_FORMAT), '-', format(date_omit_max, DATE_FORMAT))}
         )
    ) +

    # Colors of the plots
    scale_color_manual(labels = renamedLabels.Legend, values = colorsOfLabels) +
    scale_fill_manual(labels = renamedLabels.Legend, values = colorsOfLabels) +
    scale_y_continuous(labels = comma) +

    # Grab the theme
    styles::theme_dc() + theme(panel.grid.major.x = element_blank())


  functionOutput <- list('data'  = df_section_of_store_daily,
                         'plots' = list('overview' = plot.Summary))

  return(functionOutput)
}

# section_of_storeComparison <- section_of_store_analysis(selected_stores = c('Flagship Store'),
#                                                         comparison_date = as.Date('2025-05-01'),
#                                                         date_omit_min = '2025-04-01',
#                                                         date_omit_max = '2025-04-03', # Exclusive
# )
# print(section_of_storeComparison$plots)
