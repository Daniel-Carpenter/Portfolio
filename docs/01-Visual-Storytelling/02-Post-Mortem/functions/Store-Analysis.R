# ==============================================================================
# store_analysis()
# Function that gets daily data (revenue_per_store_per_day, operatinve income per day). Plots Pre/Post Comparison.
# Returns: Data frame with daily dataset by store class and some plots.
# ==============================================================================

# The only reason this function might fail is if you do not have `plyr` installed.
# Do not include in library on execution since will compete with dplyr and dbplyr and cause issues
source('docs//01-Visual-Storytelling//02-Post-Mortem//functions//get_data_is.R')


# ==============================================================================
# INPUTS
# ==============================================================================

store_analysis <- function(
  date_max = Sys.Date() - days(1),  # Max Date to pull from. Default yesterday
  date_min = Sys.Date() - 365*1,    # Min Date to pull from. Default 1 year ago from today

  date_omit_max = '1970-01-01',  # Max of date range to omit (e.g. for bad data)
  date_omit_min = '1970-01-01',  # Min of date range to omit (e.g. for bad data)

  selected_stores = c('Flagship Store', 'Other Stores'), # Business to pull data for. Default

  # Comparison information for Plots
  comparison_date        = floor_date(as.Date(date_max), 'month'), # Day of event to show comparison. Default beg. of max date month
  num_months_prior_to_avg   = 12,      # Max Store Section 3-Months to Average for Signups comparison  Default 12 months.
  include_this_month_in_avg = FALSE,   # Should you include this month in the weekday average? Default no and recommended no.,
  color_of_metric = 'yellow'           # Color to make the metric of interest
) {

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

  conn.StoreDaily <- get_data_is()


  # ==============================================================================
  # PULL IN DATA:
  # ==============================================================================


  # Pull in Store 2/3 Performance (Daily) ---------------------------------
  df_StoreDaily <- conn.StoreDaily |>

    # Filter the data to date ranges
    filter(date_of_trx >= date_min & date_of_trx < date_max) |>

    # Do not include the omission dates if not NA (i.e., changed from the default)
    filter(!(date_of_trx >= date_omit_min & date_of_trx <= date_omit_max),

           # Keep businesses from inputs at top
           store_id %in% selected_stores
    ) |>

    # Drop Unneeded columns - feel free to change this if you need this data
    select(-c(expense, discounts)) |>

    # Get operating_income_per_store_per_day per unit per day: EGM Days Active = Num Distinct EGMS * Days in Month, so can just divide
    mutate(revenue_per_store_per_day = revenue / num_days_store_was_open,
           operating_income_per_store_per_day = operating_income / num_days_store_was_open) |>

    # collect() |> # Pull data in

    # Convert to date
    mutate(date_of_trx = date(date_of_trx))


  # Summary of all properties in sample --------------------------------------
  AGGREGATION_NAME = 'Aggregate'

  df_StoreDailySummaryOfSites <- df_StoreDaily |>

    # Group by date
    group_by(date_of_trx,
             store_id = AGGREGATION_NAME, # Note that the source prop is now aggregation of all sites in sample
             tier_id) |>

    # Sum all variables from col distinct_num_stores to col transactions
    summarise_at(vars(distinct_num_stores:transactions), sum) |>

    # Calculate avg transaction amt, revenue_per_store_per_day, and operating_income_per_store_per_day
    mutate(avg_transactions_amt = sum(revenue) / sum(transactions),    # avg transaction amt
           revenue_per_store_per_day      = sum(revenue) / sum(num_days_store_was_open), # Coin in per unit per day
           operating_income_per_store_per_day      = sum(operating_income) / sum(num_days_store_was_open)) # operating_income_per_store_per_day per unit per day


  # Now add the site aggreagation to the base dataset
  df_StoreDaily <- df_StoreDaily |>
    bind_rows(df_StoreDailySummaryOfSites)


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

  # Avg. Revenue & operating_income_per_store_per_day Summary Data ===========================================

  # Post Expansion operating_income_per_store_per_day Dataset --------------------------------------------
  df_plot_post_expansion <- df_StoreDaily |>

    # Filter to date range
    filter(date_of_trx >= comparison_date) |>

    # Group by property and store class to get avg operating_income_per_store_per_day
    group_by(store_id) |>
    summarise(post_operating_income_per_store_per_day  = sum(operating_income) / sum(num_days_store_was_open),
              post_revenue  = sum(revenue) / sum(transactions))

  # Prior 6 month average operating_income_per_store_per_day (relative to comparison date) ---------------
  df_plot_prior_6_mo_summary <- df_StoreDaily |>

    # Filter to date range
    filter(date_of_trx >= weekday_avg_r6 & date_of_trx <= date_max_to_avg) |>

    # Group by property and store class to get avg operating_income_per_store_per_day
    group_by(store_id) |>
    summarise(Prior6_operating_income_per_store_per_day = sum(operating_income) / sum(num_days_store_was_open),
              Prior6_revenue = sum(revenue) / sum(transactions))

  # Prior 12 month average operating_income_per_store_per_day (relative to comparison date) --------------
  df_plotPrior12MoSummary <- df_StoreDaily |>

    # Filter to date range
    filter(date_of_trx >= weekday_avg_r12 & date_of_trx <= date_max_to_avg) |>

    # Group by property and store class to get avg operating_income_per_store_per_day
    group_by(store_id) |>
    summarise(Prior12_operating_income_per_store_per_day = sum(operating_income) / sum(num_days_store_was_open),
              Prior12_revenue = sum(revenue) / sum(transactions))


  roundToNearest = 5

  # Join the 3 operating_income_per_store_per_day datasets together then put in single col for plots -----
  df_plot_summary <- df_plot_post_expansion |>

    # Join the R6 mo-avg operating_income_per_store_per_day dataset
    left_join(df_plot_prior_6_mo_summary,
              by = c('store_id')) |>

    # Join the R12 mo-avg operating_income_per_store_per_day dataset
    left_join(df_plotPrior12MoSummary,
              by = c('store_id')) |>

    # For operating_income_per_store_per_day variables, round up to the nearest 5
    mutate_at(vars(ends_with('operating_income_per_store_per_day')),
              ~plyr::round_any(., accuracy = roundToNearest, f = ceiling)) |>

    # For revenue variables, round to 2 sig figs
    mutate_at(vars(ends_with('revenue')),
              ~round(., 2)) |>

    # Put operating_income_per_store_per_day and revenue Values into single col
    # Add 2 cols that contain the comparison period and the metric.
    # Key is that Prior12 and MetricName are separated by '_'. See data
    pivot_longer(cols      = !store_id,
                 names_to  = c('comparison_period', 'METRIC'),
                 names_sep = '_',
                 values_to = 'VALUE') |>

    # Create a string version to truncate values for plot labels
    mutate(STR_VALUE = paste0(VALUE))


  # PLOT revenue/operating_income_per_store_per_day Summary -------------------------------------------------


  # Change the names on the plots --------------------------------------------

  # Comparison Period: cleaned names
  df_plot_summary$comparison_period <- fct_relevel(df_plot_summary$comparison_period,
                                                   "Prior12", "Prior6", "Post" )
  ## Now Rename the levels
  renamedLabels.Summary <- c('36-mo. Avg.',  '12-mo. Avg.', 'Post Entry')
  renamedLabels.Legend <- c('Prior 36-mo. Avg.',  'Prior 12-mo. Avg.',   'Post-Expansion')


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
               cols = vars(store_id),
               scales = 'free_y') +      # make y-axis scale relative to property

    # Create the columns
    geom_col(alpha = 0.75) +

    # Add the labels
    geom_text(label = round(df_plot_summary$VALUE, digits = 2),
              color = getTextCol(),
              nudge_y = df_plot_summary$VALUE * 0.04) +

    # Title, Axis's, etc.
    labs(x        = '\nComparison Periods',
         y        = '\n',
         title    = 'KPI Overview',
         subtitle = 'revenue and operating_income_per_store_per_day Comparison from Historical Averages\n',
         caption  = paste0('\nPlease note potential for high volaitility until increased accumulation of days since exansion.',
                           '\noperating_income_per_store_per_day Calculation: operating_income_per_store_per_day divided by count of all Active Entities; then divided by sum of days that each store was open.',
                           '\nPrior 12-mo. avg.: ', format(weekday_avg_r12,DATE_FORMAT), ' - ', format(as.Date(comparison_date), DATE_FORMAT),
                           '\nPrior 6-mo. avg.: ', format(weekday_avg_r6, DATE_FORMAT), ' - ', format(as.Date(comparison_date), DATE_FORMAT),
                           '\nPost-Expansion Period: '  ,  format(as.Date(comparison_date),    DATE_FORMAT), ' - ', format(date_max - days(1),     DATE_FORMAT),
                           if (date_omit_min != date_omit_max) { paste('\nDates exluded due to noise in data:', format(date_omit_min, DATE_FORMAT), '-', format(date_omit_max - days(1), DATE_FORMAT))}
         )
    ) +

    # Label order
    scale_x_discrete(labels = renamedLabels.Summary) +

    # Colors of the plots
    scale_color_manual(labels = renamedLabels.Legend, values = colorsOfLabels) +
    scale_fill_manual(labels = renamedLabels.Legend, values = colorsOfLabels) +
    scale_y_continuous(labels = comma) +

    # Grab the theme
    styles::theme_dc() + theme(panel.grid.major.x = element_blank())


  # operating_income_per_store_per_day by store Store Data ===============================================

  # Post Expansion operating_income_per_store_per_day Dataset --------------------------------------------
  df_plotPostExpansionoperating_income_per_store_per_day <- df_StoreDaily |>

    # Filter to date range
    filter(date_of_trx >= comparison_date) |>

    # Group by property and store class to get avg operating_income_per_store_per_day
    group_by(store_id, tier_id) |>
    summarise(post_operating_income_per_store_per_day  = sum(operating_income) / sum(num_days_store_was_open))

  # Prior 6 month average operating_income_per_store_per_day (relative to comparison date) ---------------
  df_plot_prior_6_op_income_per_day <- df_StoreDaily |>

    # Filter to date range
    filter(date_of_trx >= weekday_avg_r6 & date_of_trx <= date_max_to_avg) |>

    # Group by property and store class to get avg operating_income_per_store_per_day
    group_by(store_id, tier_id) |>
    summarise(Prior6_operating_income_per_store_per_day   = sum(operating_income) / sum(num_days_store_was_open))

  # Prior 12 month average operating_income_per_store_per_day (relative to comparison date) --------------
  df_plotPrior12Mooperating_income_per_store_per_day <- df_StoreDaily |>

    # Filter to date range
    filter(date_of_trx >= weekday_avg_r12 & date_of_trx <= date_max_to_avg) |>

    # Group by property and store class to get avg operating_income_per_store_per_day
    group_by(store_id, tier_id) |>
    summarise(Prior12_operating_income_per_store_per_day  = sum(operating_income) / sum(num_days_store_was_open))


  roundToNearest = 5

  # Join the 3 operating_income_per_store_per_day datasets together then put in single col for plots -----
  df_plotoperating_income_per_store_per_day <- df_plotPostExpansionoperating_income_per_store_per_day |>

    # Join the R6 mo-avg operating_income_per_store_per_day dataset
    left_join(df_plot_prior_6_op_income_per_day,
              by = c('store_id', 'tier_id')) |>

    # Join the R12 mo-avg operating_income_per_store_per_day dataset
    left_join(df_plotPrior12Mooperating_income_per_store_per_day,
              by = c('store_id', 'tier_id')) |>

    # Put all three operating_income_per_store_per_day comparison columns into single col for plotting
    pivot_longer(cols      = ends_with('operating_income_per_store_per_day'),
                 names_to  = 'comparison_period',
                 values_to = 'operating_income_per_store_per_day_VALUE') |>

    # Tidy up the names
    mutate(operating_income_per_store_per_day_VALUE = plyr::round_any(operating_income_per_store_per_day_VALUE, roundToNearest, f = ceiling))


  # PLOT -------------------------------------------------------------------------


  # Change the names on the plots --------------------------------------------

  # Comparison Period: cleaned names
  df_plotoperating_income_per_store_per_day$comparison_period <- fct_relevel(df_plotoperating_income_per_store_per_day$comparison_period,
                                                                             "Prior12_operating_income_per_store_per_day", "Prior6_operating_income_per_store_per_day", "post_operating_income_per_store_per_day" )

  # Plot of operating_income_per_store_per_day -------------------------------------------------------------
  plot.operating_income_per_store_per_day <- df_plotoperating_income_per_store_per_day |>
    ggplot(aes(x     = comparison_period,
               color = comparison_period,
               fill  = comparison_period,
               y     = operating_income_per_store_per_day_VALUE
    )
    ) +

    # Facet by property and store class
    facet_grid(rows = vars(tier_id),
               cols = vars(store_id),
               scales = 'free_y') +      # make y-axis scale relative to property

    # Create the columns
    geom_col(alpha = 0.75) +

    # Add the labels
    geom_text(label = round(df_plotoperating_income_per_store_per_day$operating_income_per_store_per_day_VALUE, digits = 2),
              color = getTextCol(),
              nudge_y = max(df_plotoperating_income_per_store_per_day$operating_income_per_store_per_day_VALUE) * 0.04) +

    # Title, Axis's, etc.
    labs(x        = '\nComparison Periods',
         y        = 'Average Operating Income\nper Store per Day over Period\n',
         title    = 'Post-Expansion Decrease in Average Operating Income per Store per Day\n',
         subtitle = 'Comparison from Historical Averages\n',
         caption  = paste0('\nPlease note potential for high volaitility until increased accumulation of days since exansion.',
                           '\nAverage Operating Income per Store per Day calculation:\nAverage Operating Income divided by count of all Active Stores; then divided by sum of days that each store was open.',
                           '\nPrior 12-mo. avg.: ', format(weekday_avg_r12,DATE_FORMAT), ' - ', format(as.Date(comparison_date), DATE_FORMAT),
                           '\nPrior 6-mo. avg.: ', format(weekday_avg_r6, DATE_FORMAT), ' - ', format(as.Date(comparison_date), DATE_FORMAT),
                           '\nPost-Expansion Period: '  ,  format(as.Date(comparison_date),    DATE_FORMAT), ' - ', format(date_max - days(1),     DATE_FORMAT),
                           if (date_omit_min != date_omit_max) { paste('\nDates exluded due to noise in data:', format(date_omit_min, DATE_FORMAT), '-', format(date_omit_max, DATE_FORMAT))}
         )
    ) +

    # Label order
    scale_x_discrete(labels = renamedLabels.Summary) +

    # Colors of the plots
    scale_color_manual(labels = renamedLabels.Legend, values = colorsOfLabels) +
    scale_fill_manual(labels = renamedLabels.Legend, values = colorsOfLabels) +
    scale_y_continuous(labels = comma) +

    # Grab the theme
    styles::theme_dc() + theme(panel.grid.major.x = element_blank())


  functionOutput <- list('data'  = df_StoreDaily,
                         'plots' = list('overview' = plot.Summary,
                                        'operating_income_per_store_per_day'   = plot.operating_income_per_store_per_day))

  return(functionOutput)
}

# storeStoreTrends <- store_analysis(selected_stores = c('Flagship Store', 'Other Stores'),
#                                    comparison_date = as.Date('2025-05-01'),
#                                    date_omit_min = '2025-04-01',
#                                    date_omit_max = '2025-04-03', # Exclusive
# )
# print(storeStoreTrends$plots)
