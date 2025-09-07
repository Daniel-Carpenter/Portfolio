# Load packages and functions
source('docs//00-Reused-Code/Read-Library-and-Functions.R')


set.seed(1024) # All data is randomized! Seed set for reproducibility

# ==============================================================================
# supplier_analysis()
# Function that gets daily data by Supplier revenue_per_store_per_day. Plots Pre/Post Comparison
# Returns: Data frame with supplier daily data (operating_income_per_store_per_day), etc. Pre/post plot.
# How to Run: Make sure to open Store Section 7 project, then run file in 'Data_Pipeline' folder
# ==============================================================================

# The only reason this function might fail is if you do not have `plyr` installed.
# Do not include in library on execution since will compete with dplyr and dbplyr and cause issues
source('docs//01-Visual-Storytelling//02-Post-Mortem//functions//get_data_is.R')


# ==============================================================================
# INPUTS
# ==============================================================================

supplier_analysis <- function(

  date_max = Sys.Date() - days(1),  # Max Date to pull from. Default yesterday
  date_min = Sys.Date() - 365*1,    # Min Date to pull from. Default 1 year ago from today

  selected_stores = c('Flagship Store'),    # Business to pull data for. Default flagship

  # Comparison information for Plots
  comparison_date        = floor_date(as.Date(date_max), 'month'), # Day of event to show comparison. Default beg. of max date month

  date_omit_max = '1970-01-01',  # Max of date range to omit (e.g. for bad data)
  date_omit_min = '1970-01-01',  # Min of date range to omit (e.g. for bad data)

  num_months_prior_to_avg   = 12,      # Max Store Section 3-Months to Average for subscriberss comparison  Default 12 months.
  include_this_month_in_avg = FALSE,   # Should you include this month in the weekday average? Default no and recommended no.,
  color_of_metric = NA                 # Color to make the metric of interest
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

  # Supplier Groupings
  largerSuppliers <- c("Supplier 1", "Supplier 2", "Supplier 3")
  df_largerSuppliers <- as.data.frame(largerSuppliers)
  df_largerSuppliers <- cbind(df_largerSuppliers, TRUE)
  colnames(df_largerSuppliers) <- c('supplier_id', 'isLargeSupplier')

  # months to average for the chart - separate from inputs above since 12 is likely
  # !!Will not change the labels!!
  MONTHS_TO_AVERAGE = 12


  # ==============================================================================
  # MAKE CONNECTION TO TABLES
  # ==============================================================================

  # Supplier Daily
  conn.SupplierDaily <- get_data_is()



  # ==============================================================================
  # PULL IN DATA:
  # ==============================================================================


  # Pull in Supplier Performance (Daily) ---------------------------------
  df_SupplierDaily.base <- conn.SupplierDaily |>

    # Filter the data to date ranges
    filter(date_of_trx >= date_min & date_of_trx < date_max) |>

    # Or Do not include the omission dates if not NA (i.e., changed from the default)
    filter(!(date_of_trx >= date_omit_min & date_of_trx <= date_omit_max),

           # Keep businesses from inputs at top
           store_id %in% selected_stores
    ) |>

    # Select only neede columns
    select(-c(expense, discounts, avg_transactions_amt)) |>

    collect()  # Pull data in


  df_SupplierDaily <- df_SupplierDaily.base |>

    # Convert to date
    mutate(date_of_trx = date(date_of_trx),

           # Remove Whitespace on the Supplier Data
           supplier_id = str_trim(supplier_id)) |>

    # Flag is the supplier is one of the larger suppliers
    left_join(df_largerSuppliers,
              by = 'supplier_id') |>
    mutate(VENDOR_grouping = if_else(is.na(isLargeSupplier),
                                     'Other',        # Group as other if not defined as a larger supplier
                                     supplier_id)) |> # Use the larget supplier as it's own group
    select(-isLargeSupplier) # get rid of since now using supplier grouping col




  # ==============================================================================
  # PLOTS
  # ==============================================================================

  # ------------------------------------------------------------------------------
  # operating_income_per_store_per_day | Supplier Comparison from Historical Averages
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

  # Post Expansion operating_income_per_store_per_day Dataset --------------------------------------------
  df_plot_post_expansion <- df_SupplierDaily |>

    # Filter to date range
    filter(date_of_trx >= comparison_date) |>

    # Group by property and store class to get avg operating_income_per_store_per_day
    group_by(store_id, VENDOR_grouping) |>
    summarise(post_operating_income_per_store_per_day  = sum(operating_income) / sum(num_days_store_was_open),
              post_AvgBet  = sum(revenue) / sum(transactions))

  # Prior 6 month average operating_income_per_store_per_day (relative to comparison date) ---------------
  df_plot_prior_6_mo_summary <- df_SupplierDaily |>

    # Filter to date range
    filter(date_of_trx >= weekday_avg_r6 & date_of_trx <= date_max_to_avg) |>

    # Group by property and store class to get avg operating_income_per_store_per_day
    group_by(store_id, VENDOR_grouping) |>
    summarise(Prior6_operating_income_per_store_per_day = sum(operating_income) / sum(num_days_store_was_open),
              Prior6_AvgBet = sum(revenue) / sum(transactions))

  # Prior 12 month average operating_income_per_store_per_day (relative to comparison date) --------------
  df_plotPrior12MoSummary <- df_SupplierDaily |>

    # Filter to date range
    filter(date_of_trx >= weekday_avg_r12 & date_of_trx <= date_max_to_avg) |>

    # Group by property and store class to get avg operating_income_per_store_per_day
    group_by(store_id, VENDOR_grouping) |>
    summarise(Prior12_operating_income_per_store_per_day = sum(operating_income) / sum(num_days_store_was_open),
              Prior12_AvgBet = sum(revenue) / sum(transactions))


  roundToNearest = 5

  # Join the 3 operating_income_per_store_per_day datasets together then put in single col for plots -----
  df_plot_summary <- df_plot_post_expansion |>

    # Join the R6 mo-avg operating_income_per_store_per_day dataset
    left_join(df_plot_prior_6_mo_summary,
              by = c('store_id', 'VENDOR_grouping')) |>

    # Join the R12 mo-avg operating_income_per_store_per_day dataset
    left_join(df_plotPrior12MoSummary,
              by = c('store_id', 'VENDOR_grouping')) |>

    # For operating_income_per_store_per_day variables, round up to the nearest 5
    mutate_at(vars(ends_with('operating_income_per_store_per_day')),
              ~plyr::round_any(., accuracy = roundToNearest, f = ceiling)) |>

    # For AvgBet variables, round to 2 sig figs
    mutate_at(vars(ends_with('AvgBet')),
              ~round(., 2)) |>

    # Put operating_income_per_store_per_day and AvgBet Values into single col
    # Add 2 cols that contain the comparison period and the metric.
    # Key is that Prior12 and MetricName are separated by '_'. See data
    pivot_longer(cols      = !c(store_id, VENDOR_grouping),
                 names_to  = c('comparison_period', 'METRIC'),
                 names_sep = '_',
                 values_to = 'VALUE') |>

    # Value as string for the labels
    mutate(STR_VALUE = as.character(VALUE))


  # PLOT AvgBet/operating_income_per_store_per_day Summary -------------------------------------------------


  # Change the names on the plots --------------------------------------------

  # Comparison Period: cleaned names
  df_plot_summary$comparison_period <- fct_relevel(df_plot_summary$comparison_period,
                                                   "Prior12", "Prior6", "Post" )
  ## Now Rename the levels
  renamedLabels.Summary <- c('Prior 36-mo Avg.',  'Prior 12-mo. Avg.',   'Post Entry')

  # store_id to Actual Names
  df_plot_summary$store_id <- factor(df_plot_summary$store_id,
                                     levels = selected_stores,  # Old Names
                                     labels = selected_stores # New Names
  )

  # Rename Values for avg transaction amt and operating_income_per_store_per_day
  renamedLabels.SummaryFacets <- c('operating_income_per_store_per_day', 'avg transaction amt')
  df_plot_summary$METRIC    <- factor(df_plot_summary$METRIC  ,
                                      levels = unique(df_plot_summary$METRIC), # Old Names
                                      labels = renamedLabels.SummaryFacets    # New Names
  )
  # Sort and rename the legend values
  renamedLabels.Legend <- c(largerSuppliers, 'Other')
  df_plot_summary$VENDOR_grouping    <- factor(df_plot_summary$VENDOR_grouping  ,
                                               levels = renamedLabels.Legend, # Old Names
                                               labels = renamedLabels.Legend  # New Names
  )

  # Plot Colors used in summary and store class chart
  PRE_COMPARISON_COLOR  = 'grey87'
  colorsOfLabels       <- c(PRE_COMPARISON_COLOR,
                            paste0(styles::scale_dc('fill', 'gray')),
                            paste0(styles::scale_dc('fill', color_of_metric)))


  DATE_FORMAT = '%b %d, %Y' # Date format for the plot caption

  # Unique number of supplier groupings (for coloring)
  uniqueNumVendGroupings = length(unique(df_plot_summary$VENDOR_grouping))

  df_plot_summary <- df_plot_summary |> filter(METRIC != 'avg transaction amt')

  # Plot Comparison of supplier share
  plot.Summary <- df_plot_summary |>
    ggplot(aes(x    = comparison_period,
               y    = VALUE,
               fill = VENDOR_grouping)) +
    geom_bar(position="fill", stat="identity",
             alpha = 0.75) +

    # Label for operating_income_per_store_per_day
    geom_text(aes(label =paste0('Op. Inc. / Store / Day:\n', VALUE)),
              position =position_fill(vjust=0.5),
              colour = getTextCol(), size=3.25) +
    # Label order
    scale_x_discrete(labels = renamedLabels.Summary) +
    scale_y_continuous(labels = percent) +
    # facet_grid(rows = vars(METRIC)) +

    # Title, Axis's, etc.
    labs(x        = '\nComparison Periods',
         y        = 'Supplier % of Average Operating Income\nper Store per Day\n',
         title    = "Suppliers of Store Performance Drop Proportionally Post Entry\n",
         subtitle = 'Operating Income per Store per Day Supplier Comparison from Historical Averages\n',
         caption  = paste0('\nPlease note potential for high volaitility until increased accumulation of days since exansion.',
                           '\nAverage Operating Income per Store per Day calculation:\nAverage Operating Income divided by count of all Active Stores; then divided by sum of days that each store was open.',
                           '\nPrior 12-mo. avg.: ', format(weekday_avg_r12,DATE_FORMAT), ' - ', format(as.Date(comparison_date), DATE_FORMAT),
                           '\nPrior 6-mo. Avg.: ', format(weekday_avg_r6, DATE_FORMAT), ' - ', format(as.Date(comparison_date), DATE_FORMAT),
                           '\nPost Entry Period: '  ,  format(as.Date(comparison_date),    DATE_FORMAT), ' - ', format(date_max - days(1),     DATE_FORMAT),
                           if (date_omit_min != date_omit_max) { paste('\nDates exluded due to noise in data:', format(date_omit_min, DATE_FORMAT), '-', format(date_omit_max, DATE_FORMAT))}
         )
    ) +

    # Grab the theme
    styles::theme_dc() + theme(panel.grid.major.x = element_blank()) +
    styles::scale_fill_dc() +
    styles::scale_color_dc()

  functionOutput <- list('data'  = df_SupplierDaily,
                         'plots' = list('overview' = plot.Summary))

  return(functionOutput)
}

# supplierShare <- supplier_analysis(selected_stores = c('Flagship Store'),
#                                comparison_date = as.Date('2025-05-01'),
#                                date_omit_min = '2025-04-01',
#                                date_omit_max = '2025-04-03', # Exclusive
# )
# print(supplierShare$plots)
