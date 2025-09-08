# Load packages and functions
source('docs//00-Reused-Code/Read-Library-and-Functions.R')


set.seed(1024) # All data is randomized! Seed set for reproducibility


# ==============================================================================
# # subscriber_drivetime_analysis()
# Function that gets daily new subscriber drivetime data (in hours). Plots trends + Multi-period comparisons
# Returns: Data frame with daily avg. drivetimes (hours) detail and a table with
#          monthly drive time trends from new member signups.
#          Time-series trend and drivetime density plots
# Source Data: RANDOM DATA
# ==============================================================================

# Need this for default date in function
source('docs//01-Visual-Storytelling//02-Post-Mortem//functions//get_data_subscriber.R')


# ==============================================================================
# INPUTS
# ==============================================================================

subscriber_drivetime_analysis      <- function(
    date_max = Sys.Date() - days(1), # Max Date to pull from. Default today
    date_min = Sys.Date() - round(365*3),   # Min Date to pull from. Default 4 years ago from today

    selected_stores = c(1), # Business to pull data for. Default 1

    # Comparison information for Plots
    comparison_date1       = floor_date(as.Date(date_max), 'month'), # Day of event to show comparison. Default beg. of max date month
    comparison_date1Name   = 'Post-Competitor Entering Market',

    comparison_date2       = Sys.Date() - 365, # 2ns Day of event to show comparison.
    comparison_date2Name   = 'Steady-State Operations',

    num_prior_months_to_avg   = 12,       # N-Months to Average for Signups comparison  Default 12 months.
    include_this_mo_in_avg = FALSE,    # Should you include this month in the weekday average? Default no and recomended no.
    metric_color = 'blue'              # Color to make the metric of interest
) {

  # Load or install some packages for this file:
  if (!require("zoo"))     install.packages("zoo");      library(zoo)     # for rolling 12 average


  # ==============================================================================
  # IMMEDIATE OVERRIDES TO INPUTS
  # ==============================================================================

  # Ensure date inputs are dates
  date_max         = as.Date(date_max)
  date_min         = as.Date(date_min)
  comparison_date1 = as.Date(comparison_date1)
  comparison_date2 = as.Date(comparison_date2)


  # Get Name of the business
  selected_stores <- selected_stores

  MONTHS_TO_AVERAGE = 12 # months needed to average trailing n month's value

  nMonthsBeforeComparisonDate = 6 # Months before the comparison date to flag
  nMonthsPriorToCompDate1 = comparison_date1 - months(nMonthsBeforeComparisonDate)


  # ==============================================================================
  # MAKE CONNECTION TO BASE TABLE
  # ==============================================================================

  # New Member Signups Drive Time Connection
  conn.subscriberSignupsDriveRaw <- get_data_subscriber(
    date_min        = date_min,
    date_max        = date_max,
    comparison_date = comparison_date1
  )

  # update data
  conn.subscriberSignupsDriveRaw <- conn.subscriberSignupsDriveRaw |>

    mutate(drivetime_minutes = if_else(
      date_of_trx > comparison_date1,
      rbimodal(n = nrow(conn.subscriberSignupsDriveRaw), mean_1 = 20, sd_1 = 10, mean_2 = 160, sd_2 = 10),
      drivetime_minutes
      )
    )



  # ==============================================================================
  # PULL IN DATA and MANIPULATE DATA
  # ==============================================================================

  # Labels for the comparison periods defined in the inputs
  label_comparison_date1Name = comparison_date1Name                  # Post Competitor Entering Market
  label_comparison_date2Name = paste0('Pre-', comparison_date2Name)  # Post comparison date 2
  label_comparison_date3Name = paste0('Post-', comparison_date2Name) # Pre  comparison date 2
  label_comparison_date4Name = paste0('Prior-', nMonthsBeforeComparisonDate, ' months') # E.g. prior 12 months before Competitor Entering Market

  labels.comparisonPeriods <- c(label_comparison_date1Name, # Combination and ordered
                                label_comparison_date4Name,
                                label_comparison_date3Name,
                                label_comparison_date2Name)

  # ----------------------------------------------------------------------------
  # PERCENTAGE OF PATRONS WITH NO DRIVE TIME DATA
  # ----------------------------------------------------------------------------

  # Percent of subscribers who do not have drive time data
  percentSubscribersNoDrivetime = percent(0.1)


  # ----------------------------------------------------------------------------
  # DRIVE TIME MINUTES RAW
  # ----------------------------------------------------------------------------

  # Get the subscriber signup by drive time minutes data
  df_subscriber_signups_drive_raw.base <- conn.subscriberSignupsDriveRaw |>

    # Filter the data to date ranges define at top
    filter(date_of_trx >= date_min & date_of_trx < date_max,
           store_id %in% selected_stores,
    ) |>

    # Do not pull in these columns
    select(-c(gender, age_bin_id, drivetime_bin_id)) |>

    # Flag the data for comparison periods                                                                         # E.g.,
    mutate(comparison_period = if_else(date_of_trx >= comparison_date1, label_comparison_date1Name,                 # Post Competitor Entering Market
                                       if_else(date_of_trx >= nMonthsPriorToCompDate1, label_comparison_date4Name, # N months before exanding
                                               if_else(date_of_trx >= comparison_date2, label_comparison_date3Name, # Post comparison date 2
                                                       label_comparison_date2Name)                                  # Pre  comparison date 2
                                       )
    ),
    # get the hours it takes to drive to the facility (not minutes)
    DRIVETIME_HOURS = drivetime_minutes / 60)


  # Weekend vs Weekday
  VALUE_OF_MONDAY = 1
  VALUE_OF_FRIDAY = 5
  df_subscriber_signups_drive_raw <- df_subscriber_signups_drive_raw.base |>

    # If the day of the week is Friday, Saturday, or Sunday, then Weekend. Else Weekday
    mutate(WEEKDAY_NUM = lubridate::wday(date_of_trx, week_start = getOption("lubridate.week.start", VALUE_OF_MONDAY)),
           DAY_TYPE = if_else(WEEKDAY_NUM >= VALUE_OF_FRIDAY,
                              'Weekend', 'Weekday')
    ) |>
    select(-WEEKDAY_NUM) # get rid of the weekday num since do not want to group this


  # ----------------------------------------------------------------------------
  # DRIVE TIME MINUTES MONTHLY AVERAGES
  # ----------------------------------------------------------------------------

  # Make a dataset grouped by month (not day)
  df_subscriber_signups_drive_monthly <- df_subscriber_signups_drive_raw |>

    # Add a variable to capture the end of the month from the signup date
    mutate(month_end_date = ceiling_date(date(date_of_trx), unit = 'month') - days(1)) |>

    # Group by end of month
    group_by(across(-c(subscriber_id, date_of_trx,
                       comparison_period, drivetime_minutes, DRIVETIME_HOURS))) |>
    summarise(avg_drivetime_in_hours = mean(DRIVETIME_HOURS)) |>


    # Get rolling n month average (default 12)
    mutate(DRIVETIME_HOURS_R12_AVG = rollmean(avg_drivetime_in_hours, MONTHS_TO_AVERAGE,na.pad=TRUE, align="right"),

           # Flag the data for comparison periods
           comparison_period = if_else(month_end_date >= comparison_date1, label_comparison_date1Name,                 # Post Competitor Entering Market
                                       if_else(month_end_date >= nMonthsPriorToCompDate1, label_comparison_date4Name, # N months before exanding
                                               if_else(month_end_date >= comparison_date2, label_comparison_date3Name, # Post comparison date 2
                                                       label_comparison_date2Name)                                  # Pre  comparison date 2
                                       )
           )
    )

  # ==============================================================================
  # Plots
  # ==============================================================================

  DATE_FORMAT = '%b %d, %Y' # Date format for the plot caption
  PRE_COMPARISON_COLOR  = 'grey87'

  # Ordering and naming of the comparison period labels
  df_subscriber_signups_drive_raw$comparison_period <- factor(df_subscriber_signups_drive_raw$comparison_period,
                                                          levels = labels.comparisonPeriods,
                                                          labels = labels.comparisonPeriods)


  # ------------------------------------------------------------------------------
  # OVERVIEW Plot
  # ------------------------------------------------------------------------------

  bufferAxis = 0.05
  min_ignore_nas <- function(x) ifelse( !all(is.na(x)), min(x, na.rm=T), NA) # min_ignore_nas ignoring NA
  max_ignore_nas <- function(x) ifelse( !all(is.na(x)), max(x, na.rm=T), NA) # max_ignore_nasx ""


  last_full_mo = floor_date(date_max, 'month') - days(1) # The last month with complete data

  # Create the plot ------------------------------------------------------------

  # Create a base plot to add onto for the overview
  basePlot.overview <- df_subscriber_signups_drive_monthly |> filter(month_end_date <= last_full_mo) |>
    ggplot(aes(x = month_end_date)) +
    theme_get() +
    labs(title    = paste0('Average Drive Time to Flagship Location Rises, but Trend Misleads\n'),
         subtitle = paste0('New Subscriber Signups',
                           '\nMonthly Average Hours Traveled from Subscriber Residency to Flagship Store\n'),
         x='\nMonth of Year',
         y='Monthly Average Drive Time (Hours)\n',
         caption = paste0('\nData shown through ', format(last_full_mo, DATE_FORMAT))) +
    scale_y_continuous(labels = scales::comma) +

    # Make the x axis label in a digestible format (Apr-01)
    scale_x_date(date_labels = "%b-%y") +

    # Break out facets by the weekend or weekday
    facet_grid(rows = vars(DAY_TYPE), scales = 'free_y')



  # Create the overview plot ----------------------------------------------------
  plot_overview <- basePlot.overview +

    # Hone in on the data by limiting the axis
    coord_cartesian(ylim = c(min_ignore_nas(coalesce(df_subscriber_signups_drive_monthly$DRIVETIME_HOURS_R12_AVG)) * (1 - bufferAxis),
                             max_ignore_nas(coalesce(df_subscriber_signups_drive_monthly$DRIVETIME_HOURS_R12_AVG)) * (1 + bufferAxis))) +

    # Column of raw number of signups
    geom_col(aes(y  = avg_drivetime_in_hours, fill = 'Average Hours Traveled'), alpha=0.4) +
    scale_fill_manual(values = c("Average Hours Traveled" = paste0(getFillCol('blue')))) + # Legend fill

    # Line of rolling 12 mo. avg. number of signups
    geom_line(aes(y = DRIVETIME_HOURS_R12_AVG, color = 'TTM Average Hours Traveled'), # maps to the legend colors
              alpha=0.9, lwd=1, linetype = 'solid') +

    # Get a linear trend of the average drive time to compare to the rolling 12 average trend
    geom_smooth(aes(y = avg_drivetime_in_hours,
                    color    = 'Predicted Trend'), # maps to the legend colors
                linetype = 'dotdash',
                fill     = NA, #'grey90',
                method   = 'lm') +

    scale_color_manual(values = c("R12 Average Hours Traveled" = paste0(getLineCol('blue')),
                                  "Predicted Trend"            = paste0(getFillCol('charcoal')))) # Legend Colors


  # ------------------------------------------------------------------------------
  # Distribution Comparison Plot
  # ------------------------------------------------------------------------------

  caption_var = paste0('\n', labels.comparisonPeriods[1], ' Date: ', format(comparison_date1,DATE_FORMAT),
                       '\n', labels.comparisonPeriods[2], ' Date Range: ', ' (Relative to ', format(comparison_date1,DATE_FORMAT), ') ', format(nMonthsPriorToCompDate1, DATE_FORMAT), ' - ', format(comparison_date1,DATE_FORMAT),
                       '\n', labels.comparisonPeriods[3], ' Date Range: ', format(comparison_date2,DATE_FORMAT), ' - ', format(nMonthsPriorToCompDate1,DATE_FORMAT),
                       '\n', labels.comparisonPeriods[4], ' Date Range: ', format(date_min,DATE_FORMAT), ' - ', format(comparison_date2,DATE_FORMAT),
                       '\n"Weekend" includes Friday, Saturday, and Sunday. All other days are on a "Weekday"',
                       '\n Note that there are approximately ', percentSubscribersNoDrivetime, ' of subscribers who do not have drive time data in this sample.'
  )

  # Base plot for distance distributions ---------------------------------------

  # Name of the two boundaries
  market1Name = 'Market 1: Within 2 Hour Drive'
  market2Name = 'Market 2: More than 2 Hour Drive'

  df_plot_base_drive_dist <- df_subscriber_signups_drive_raw |>
    mutate(LOCATION_BOUNDARY = if_else(DRIVETIME_HOURS < 2,
                                       market1Name,
                                       market2Name))

  # _size before and after the boundary (Value)
  market1_size = round(sum(df_plot_base_drive_dist$LOCATION_BOUNDARY == market1Name), -4)
  market2_size = round(sum(df_plot_base_drive_dist$LOCATION_BOUNDARY == market2Name), -4)

  # _size before and after the boundary (percentage of total) - note as text for output
  market1_sizePerc = percent(market1_size / (market1_size + market2_size))
  market2_sizePerc = percent(market2_size / (market1_size + market2_size))

  plot_base_drive_dist <- df_plot_base_drive_dist |>
    ggplot(aes(x = DRIVETIME_HOURS,
               y = comparison_period)) +
    labs(title    = paste0("Lost Key Market, and Less Subscribers Travel from Farther Away\n"),
         subtitle = paste0("Subscribers' Historically Travel from Two Primary Markets's\n"),
         x=paste0('Drive Time to Flagship Store from Subscriber Residency (Hours)'),
         y='',
         caption = caption_var
    ) +
    facet_grid(cols = vars(DAY_TYPE))

  # Distance distribution by weekday/weekend  ----------------------------------
  plot_drive_dist <- plot_base_drive_dist +

    geom_violin(fill = getFillCol('blue'),
                color = getLineCol('blue'), alpha = 0.75)


  # Distance distribution by weekday/weekend AND Market ------------------------
  plot_drive_distMarkets <- plot_drive_dist +
    geom_boxplot(fill = 'grey98',
                 color = getLineCol('blue'), alpha = 0.4) +
    facet_wrap(LOCATION_BOUNDARY~DAY_TYPE,
               nrow = 2, scales = 'free_x') +
    labs(caption = paste0(caption_var,
                          '\n\nNote scales of Market 1 and 2 are independant of each other for clearer viewing. ',
                          '\nTotal New Subscribers from ',format(date_min,DATE_FORMAT), ' - ', format(date_max,DATE_FORMAT), ', ',
                          'Market 1: ~', comma(market1_size), ' (', market1_sizePerc, '); ',
                          'Market 2: ~', comma(market2_size), ' (', market2_sizePerc, ')'
    )
    )


  # The output of the function
  output <- list('data'  = list('detail' = df_subscriber_signups_drive_raw,
                                'monthly_trend'              = df_subscriber_signups_drive_monthly),
                 'plots' = list('overview'                   = plot_overview,
                                'drive_distributions'        = plot_drive_dist,
                                'drive_distributions_market' = plot_drive_distMarkets
                 )
  )

  return(output)
}


# # # If you want to test the function in this file:
# drivetime_distribution <- subscriber_drivetime_analysis(comparison_date1 = ('2025-06-01'))
#
#
# drivetime_distribution
