# Load packages and functions
source('docs//00-Reused-Code/Read-Library-and-Functions.R')


set.seed(1024) # All data is randomized! Seed set for reproducibility

# ==============================================================================
# Function that gets new member (subscriber) new_subscribers data by demographic bin. Plots trends + Pre/Post Comparison
# Returns: Data frame with daily subscriber new_subscribers by age bin, drive time bin, or gender. Depends on inputs
# Source Data: RANDOM DATA
# ==============================================================================

# Need this for default date in function
if(!require("lubridate")) install.packages("lubridate"); library(lubridate) # for rolling 12 average
source('docs//01-Visual-Storytelling//02-Post-Mortem//functions//get_data_subscriber.R')


# ==============================================================================
# INPUTS
# ==============================================================================

subscriber_demographic_analysis <- function(
    date_max = Sys.Date() - days(1), # Max Date to pull from. Default today
    date_min = Sys.Date() - round(365*3),   # Min Date to pull from. Default 4 years ago from today

    date_omit_max = '1970-01-01',  # Max of date range to omit (e.g. for bad data)
    date_omit_min = '1970-01-01',  # Min of date range to omit (e.g. for bad data)

    selected_stores = c(1),   # Business to pull data for. Default 1

    # Data to pull (will pull at least the plot)
    include_gender      = FALSE,
    include_ages        = FALSE,
    include_drivetimes  = FALSE,

    # Demographics to plot Input
    demographic_to_plot = 'Drivetime Bin Name', # Gender, Drivetime Bin Name, Age Bin Name Default 'Drivetime Bin Name'

    # Comparison information for Plots
    comparison_date        = floor_date(as.Date(date_max), 'month'), # Day of event to show comparison. Default beg. of max date month
    num_prior_months_to_avg   = 12,    # N-Months to Average for New Subscribers comparison  Default 12 months.
    include_this_mo_in_avg = FALSE,    # Should you include this month in the weekday average? Default no and recomended no.
    metric_color = 'blue',             # Color to make the metric of interest

    dir_data = 'docs//01-Visual-Storytelling//02-Post-Mortem//data//'
) {

  # Load or install some packages for this file:
  if (!require("zoo")) install.packages("zoo");      library(zoo)     # for rolling 12 average


  # DATA
  df_drivetime_bins <- fread(paste0(dir_data, 'drivetime_bins.csv'))
  df_age_bins       <- fread(paste0(dir_data, 'age_bins.csv'))
  df_gender_bins    <- fread(paste0(dir_data, 'gender_bins.csv'))



  # ==============================================================================
  # IMMEDIATE OVERRIDES TO INPUTS
  # ==============================================================================

  # Ensure date inputs are dates
  date_max         = as.Date(date_max)
  date_min         = as.Date(date_min)
  date_omit_max = as.Date(date_omit_max)
  date_omit_min = as.Date(date_omit_min)
  comparison_date  = as.Date(comparison_date)

  # Include if need to plot
  if (demographic_to_plot == 'Gender')             include_gender      = TRUE
  if (demographic_to_plot == 'Age Bin Name')       include_ages        = TRUE
  if (demographic_to_plot == 'Drivetime Bin Name') include_drivetimes  = TRUE


  # ==============================================================================
  # MAKE CONNECTION TO BASE TABLE
  # ==============================================================================

  # New Subscribers Connection
  conn.subscriberSignups <- get_data_subscriber(
    date_min        = date_min,
    date_max        = date_max,
    comparison_date = comparison_date
  )


  # ==============================================================================
  # PULL IN DATA and MANIPULATE DATA
  # ==============================================================================

  df_new_subscriber_demo <- c() # will append each demographic to this

  # Get the subscriber signup data
  df_new_subscriber <- conn.subscriberSignups |>

    # Filter the data to date ranges define at top
    filter(date_of_trx >= date_min & date_of_trx <= date_max,
           store_id %in% selected_stores
    )



  # ----------------------------------------------------------------------------
  # Include Gender if needed
  # ----------------------------------------------------------------------------

  if (include_gender) {
    df_gender <- df_new_subscriber |>
      group_by(store_id, date_of_trx, gender) |> # Group by gender
      summarise(num_new_subscribers = n()) |>

      # Join the age bin names
      left_join(df_gender_bins,
                by = 'gender_BIN_ID') |>

      # Pivot to single column
      pivot_longer(cols      = gender,
                   names_to  = "DEMOGRAPHIC",
                   values_to = "DEMOGRAPHIC_VALUE")

    # Call the query and append to the main dataset
    df_new_subscriber_demo <- rbind(df_new_subscriber_demo, df_gender)
  }



  # ----------------------------------------------------------------------------
  # Include Age bins if needed
  # ----------------------------------------------------------------------------

  if (include_ages) {

    df_ages <- df_new_subscriber |>
      group_by(store_id, date_of_trx, age_bin_id) |> # Group by age bin
      summarise(num_new_subscribers = n()) |>

      # Join the age bin names
      left_join(df_age_bins,
                by = 'age_bin_id') |>
      select(-age_bin_id) |> # remove since now have the name

      # Pivot to single column
      pivot_longer(cols      = AGE_BIN_NAME,
                   names_to  = "DEMOGRAPHIC",
                   values_to = "DEMOGRAPHIC_VALUE")

    # Call the query and append to the main dataset
    df_new_subscriber_demo <- rbind(df_new_subscriber_demo, df_ages)

  }

  # ----------------------------------------------------------------------------
  # Include Drive Time data if needed
  # ----------------------------------------------------------------------------

  if (include_drivetimes) {

    # Drive Time Bin Names Connection
    df_driveTimes <- df_new_subscriber |>
      group_by(store_id, date_of_trx, drivetime_bin_id) |> # Group by drive time id
      summarise(num_new_subscribers = n()) |>

      # Join the age bin names
      left_join(df_drivetime_bins,
                by = 'drivetime_bin_id') |>
      select(-drivetime_bin_id) |> # remove since now have the name

      # Pivot to single column
      pivot_longer(cols      = DRIVETIME_BIN_NAME,
                   names_to  = "DEMOGRAPHIC",
                   values_to = "DEMOGRAPHIC_VALUE")

    df_new_subscriber_demo <- rbind(df_new_subscriber_demo, df_driveTimes)

  }

  # Convert the name of the deomgraohic column to title
  df_new_subscriber_demo <- df_new_subscriber_demo |>
    mutate(DEMOGRAPHIC = str_to_title(str_replace_all(DEMOGRAPHIC, '_', ' ')))


  # ==============================================================================
  # Plots
  # ==============================================================================

  DATE_FORMAT = '%b %d, %Y' # Date format for the plot caption

  # Name on the plot
  demographicName = str_replace_all(demographic_to_plot, ' Bin Name', '')


  # Labels sorting on plots
  genderLabels <- c('M', 'F') # Assume unknown last

  ageLabels <- c("Under 21", "22 to 34",
                 "35 to 44", "45 to 54",
                 "55 to 64", "65 and older")

  driveLabels <- c('< 20 mins.', '< 1 hr.',
                   '< 1.5 hrs.', '< 2 hrs.',
                   '< 3 hrs.',   '> 3 hrs.,',
                   "Unknown")

  df_new_subscriber_demo$DEMOGRAPHIC_VALUE <- factor(df_new_subscriber_demo$DEMOGRAPHIC_VALUE,
                                                     levels = c(genderLabels, ageLabels, driveLabels))

  # Make a dataset grouped by month (not day)
  df_monthly_new_subscribers_demo <- df_new_subscriber_demo |>

    # Add a variable to capture the end of the month from the signup date
    mutate(month_end_date = ceiling_date(date(date_of_trx), unit = 'month') - days(1)) |>

    # Only show one demographic
    filter(DEMOGRAPHIC == demographic_to_plot)


  # ------------------------------------------------------------------------------
  # OVERVIEW Plot
  # ------------------------------------------------------------------------------

  # months to average for the chart - seperate from inputs above since 12 is likely
  # !!Will not change the labels!!
  MONTHS_TO_AVERAGE = 12

  # Dataset for the overview Plot ----------------------------------------------
  df_overview_plot <- df_monthly_new_subscribers_demo |>

    # Group by month by indirectly not groupung by date, also re count the new_subscribers by month
    group_by(across(c(-date_of_trx, -num_new_subscribers, -DEMOGRAPHIC_VALUE))) |>
    summarise(num_new_subscribers = sum(num_new_subscribers)) |>
    mutate(num_new_subscribers_R12_AVG = rollmean(num_new_subscribers, MONTHS_TO_AVERAGE,na.pad=TRUE, align="right")) |>
    drop_na()

  # Data for the Labels --------------------------------------------------------

  last_full_mo = floor_date(date_max, 'month') - days(1) # The last month with complete data

  label_Data <- df_overview_plot |>  filter(month_end_date == last_full_mo |
                                              month_end_date == last_full_mo - days(365))# Row of data for the month

  # Create the # new_subscribers label for the segmentation plot (not rolling average)
  label_most_recent_month_signup <- geom_label_repel(data=label_Data,

                                                     # Create the label
                                                     aes(label=paste0(
                                                       format(month_end_date, '%b-%y'),       # Apr-22
                                                       ':\nSignups: ', comma(round(num_new_subscribers,-2))), # New Subscribers: 123
                                                       y=num_new_subscribers+0.10*num_new_subscribers),     # Set the y axis coordinate
                                                     # nudge_x =-30,
                                                     position=position_jitter(width=0,height=10),
                                                     color = getLineCol('charcoal'), alpha = 0.75, # Color and transparency level
                                                     label.size = NA
  )

  # Create the # new_subscribers label for the segmentation plot (rolling average)
  createR12Label <- function(df, includePadding=TRUE) {
    label <- geom_label_repel(data=df,
                              # Create the label with or without some padding
                              aes(label=paste0(ifelse(includePadding, 'R12 avg.\n', 'R12 avg: '), # R12 avg: 123
                                               comma(round(num_new_subscribers_R12_AVG, -2), accuracy=1)),
                                  y=num_new_subscribers_R12_AVG + max(df_overview_plot$num_new_subscribers_R12_AVG) * 0.2), # Set the y axis coordinate
                              position=position_jitter(width=0,
                                                       height=ifelse(includePadding, 10, 0)),
                              color = getLineCol('charcoal'), alpha = 0.75,                        # Color and transparency level
                              label.size = NA

    )
    return(label)
  }

  # Label for rolling 12 avg
  label_avgR12 <- createR12Label(label_Data)


  # Create the plot ------------------------------------------------------------

  # Create a base plot to add onto for the overview
  basePlot.overview <- df_overview_plot |> filter(month_end_date <= last_full_mo) |>
    ggplot(aes(x = month_end_date)) +
    theme_get() +
    labs(title    = 'Number of New Subscriber New Subscribers Decrease After Competitor Entry',
         subtitle = paste0(''),
         x='Month of Year', y='Num. New Subscribers'
    )
  scale_y_continuous(labels = scales::comma)


  # Create the oveview plot ----------------------------------------------------
  plot_overview <- basePlot.overview +

    # Column of raw number of new_subscribers
    geom_col(aes(y  = num_new_subscribers, fill = 'Number of New Subscribers'), alpha=0.4) +
    scale_fill_manual(values = c("Number of New Subscribers" = paste0(getFillCol('blue')))) + # Legend

    # Line of rolling 12 mo. avg. number of new_subscribers
    geom_line(aes(y = num_new_subscribers_R12_AVG, color = 'R12 avg. Num. New Subscribers'), alpha=0.9, lwd=1) +
    scale_color_manual(values = c("R12 avg. Num. New Subscribers" = paste0(getLineCol('blue')))) + # Legend

    label_most_recent_month_signup +  # Most recent month signup label raw data
    label_avgR12 +
    scale_y_continuous(labels = comma)

  plot_overview

  # ------------------------------------------------------------------------------
  # Demographic Segmentation
  # ------------------------------------------------------------------------------


  # Pie Chart Plot -------------------------------------------------------------

  # Create the data for the pie chart
  df_segment_pie <- df_monthly_new_subscribers_demo |>
    group_by(DEMOGRAPHIC_VALUE) |>
    summarise(num_new_subscribers = sum(num_new_subscribers)) |>
    drop_na()

  # Calculate the percentage of each group
  df_segment_pie$percent_num_new_subscribers = df_segment_pie$num_new_subscribers / sum(df_segment_pie$num_new_subscribers)

  hole_size = 2.5 # the size of the hole

  # Create the pie chart
  plot_segment_pie <- ggplot(df_segment_pie,
                             aes(x    = hole_size,
                                 y    = percent_num_new_subscribers,
                                 fill = DEMOGRAPHIC_VALUE)) +

    # The actual plots
    geom_col() +
    coord_polar(theta = "y") +

    # Formatting
    xlim(c(0.2, hole_size + 0.5)) +
    theme_void() +
    theme(legend.position = 'top',
          text = element_text(colour = '#666666')) +
    scale_fill_manual(values=c('#ffffff',
                               '#ddecf0',
                               '#bed8df',
                               '#a4c4cc',
                               '#8db0b8',
                               '#7a9ba3'
    )) +
    labs(fill = paste0("New Subscribers by\n",demographicName),
         caption = 'Period over entire duration of leftmost chart.')


  # Trend Plot  ----------------------------------------------------------------

  # Dataset for the demographic Plot
  df_segment_plot_data <- df_monthly_new_subscribers_demo |>

    # Group by month by indirectly not groupung by date, also re count the new_subscribers by month
    group_by(across(c(-date_of_trx, -num_new_subscribers))) |>
    summarise(num_new_subscribers = sum(num_new_subscribers)) |>
    mutate(num_new_subscribers_R12_AVG = rollmean(num_new_subscribers, MONTHS_TO_AVERAGE, na.pad=TRUE, align="right")) |>
    drop_na()

  # Data for the labels
  label_Data <- df_segment_plot_data |>  filter(month_end_date == max(month_end_date) |
                                                  month_end_date == max(month_end_date) - days(365))# Row of data for the month
  # Create the R12 avg label
  label_avgR12 <- createR12Label(label_Data, includePadding = FALSE)


  # Create a base plot to add onto for the segemntation ------------------------
  basePlot.segment <- df_segment_plot_data |>
    ggplot(aes(x = month_end_date)) +
    theme_get() +
    facet_grid(rows   = vars(DEMOGRAPHIC_VALUE),
               scales = 'free_y') +
    labs(title    = paste0('Number of New Subscriber New Subscribers by ', demographicName, '\n'),
         subtitle = paste0(''),
         x='Month of Year', y='Num. New Subscribers') +
    scale_y_continuous(labels = comma)


  # Create the plot ------------------------------------------------------------
  plot_segment <- basePlot.segment +

    # Column of raw number of new_subscribers
    geom_col(aes(y  = num_new_subscribers, fill = 'Number of New Subscribers'), alpha=0.4) +
    scale_fill_manual(values = c("Number of New Subscribers" = paste0(getFillCol('blue')))) + # Legend

    # Line of rolling 12 mo. avg. number of new_subscribers
    geom_line(aes(y = num_new_subscribers_R12_AVG, color = 'R12 avg. Num. New Subscribers'), alpha=0.9, lwd=1) +
    scale_color_manual(values = c("R12 avg. Num. New Subscribers" = paste0(getLineCol('blue')))) + # Legend

    # Rolling 12 month avg label
    label_avgR12


  # ----------------------------------------------------------------------------
  # Daily Average Comparison - Cumulative New Subscribers vs. 12-Mo. avg. Trend
  # ----------------------------------------------------------------------------

  # calculate the First and last day to average by weekday
  weekday_avgSinceDate = floor_date(comparison_date - months(MONTHS_TO_AVERAGE), unit='month')
  date_maxTo_avg = as.Date(ifelse(include_this_mo_in_avg,
                                  comparison_date,
                                  floor_date(comparison_date, unit='month') - days(1))
  )

  df_avgBase <- df_new_subscriber_demo |>

    # Average from x months ago
    filter(date_of_trx >= weekday_avgSinceDate) |>

    # Group by the day and exlcude demographic (this is for an overview)
    group_by(across(c(-DEMOGRAPHIC, -DEMOGRAPHIC_VALUE, -num_new_subscribers))) |>
    summarise(num_new_subscribers = sum(num_new_subscribers)) |>

    # Add the weekday name to dataset
    mutate(WEEKDAY_NAME = lubridate::wday(date_of_trx, label = TRUE))


  # Create the weekday average table -------------------------------------------
  df_weekday_avg <- df_avgBase |>

    # Filter out to max date in not including this month
    filter(date_of_trx <= date_maxTo_avg) |>

    # Get the average new_subscribers per weekday by store
    group_by(across(c(-date_of_trx, -num_new_subscribers))) |>
    summarise(AVG_num_new_subscribers = round(mean(num_new_subscribers)))


  # Change before and after comparison date ------------------------------------
  df_avg_new_subscribers_comparison_detail <- df_avgBase |>

    # Add 1/0 flag for day of Competitor Entering Market or not
    mutate(isAfterComparisonDate = date_of_trx >= comparison_date) |>

    # Sort by store, date, amd name
    # this is important for the boxplot daily comparison. want to highlight recent dates
    arrange(store_id, date_of_trx, WEEKDAY_NAME)


  # Compare weekday average to actuals  ----------------------------------------
  df_avg_new_subscribers_comparison_trend <- df_avg_new_subscribers_comparison_detail |>

    # Only dates after the comparison date
    filter(isAfterComparisonDate,

           # Do not include the omission dates if not NA (i.e., changed from the default)
           !between(date_of_trx, date_omit_min, date_omit_max)
    ) |>

    # Join to main data set
    left_join(df_weekday_avg,
              by = c('store_id', 'WEEKDAY_NAME')) |>

    # Add cumulative sum for historical avg and actual
    mutate(cum_sum_actual = cumsum(num_new_subscribers),
           cum_sum_avg    = cumsum(AVG_num_new_subscribers),

           # % Difference from cumulative sums
           diff_in_cum_sums  = ( cum_sum_actual - cum_sum_avg ) / cum_sum_avg,

           # If positive variance then one col, else if negative the other col
           posDifInCum_sums = if_else(diff_in_cum_sums > 0, diff_in_cum_sums, 0),
           negDifInCum_sums = if_else(diff_in_cum_sums < 0, diff_in_cum_sums, 0)
    )




  # Plot Trend of actuals vs. Average ------------------------------------------

  legend.labels = c(paste0(num_prior_months_to_avg, '-mo. Weekday avg. New Subscribers'),
                    'Post-Competitor Entering Market New Subscribers')

  PRE_COMPARISON_COLOR  = 'grey87' # Color for the comparison

  plot_new_subscribers_cumulative_var <- df_avg_new_subscribers_comparison_trend |>
    ggplot(aes(x = as.Date(date_of_trx) ) ) +

    # Line showing the cumulative variance from the average performance
    # This is hear to connect the pos and neg variance
    geom_line(aes(y = diff_in_cum_sums),
              color = getLineCol(metric_color),
              linetype = 'solid',
              size  = 1.25
    ) +

    # Horizontal line emohasizing the baseline performance (not change = 0 = horizontal intercept)
    geom_hline(yintercept = 0,
               color = getLineCol('charcoal')) +

    # Label emphasizing basline = 0%
    annotate("text", x = max(df_avg_new_subscribers_comparison_trend$date_of_trx) - days(4), y = 0.03, label = "Historical Baseline",
             color = getLineCol('charcoal')) +

    # Make the x axis label in a digestible format (Apr-01)
    scale_x_date(date_labels = "%d-%b") +

    # Make the y axis in comma seperated values
    scale_y_continuous(labels = percent) +

    labs(title    = paste0('Daily New Subscribers Since Competitor Entering Market Drop Below ', num_prior_months_to_avg,
                           '-Month Weekday Average\n' ),
         subtitle = paste0('See Caption for Calculation\n'),
         x = 'Date', y = '% Change | Cumulative New Subscribers vs.\n12 mo. avg. New Subscribers',
         caption = paste0('\nCalculation: (Cumulative New Subscribers - Cumulative 12-mo. avg New Subscribers by Weekday) / Cumulative 12-mo. avg New Subscribers by Weekday.\n',
                          '"Baseline" refers to 12-mo. avg. New Subscribers by Weekday.'
         )
    ) +
    theme_get()
  # plot_new_subscribers_cumulative_var

  # Plot daily actuals vs. historical new_subscribers by DOW ---------------------------

  SPACING_AMT = 0.15

  # Internal scatter of daily (facet day of week). Grey out prior and highlight current
  plot_new_subscribers_detail <- df_avg_new_subscribers_comparison_detail |>
    ggplot(aes(x     = WEEKDAY_NAME,
               y     = num_new_subscribers,
               color = isAfterComparisonDate)
    ) +

    geom_boxplot(fill  = 'grey99', color = 'grey92', alpha = 0.99) +

    # Daily new_subscribers for each point, jitter amount ensures little overlap for viewing ease
    geom_point(position = position_jitter(height = 0, width = SPACING_AMT),
               size = 4, alpha = 0.50) +


    # Highlight the points after the comparison date, add legend title
    scale_color_manual(values = c(PRE_COMPARISON_COLOR,
                                  paste0(getLineCol(metric_color))),
                       labels = c('Pre-Competitor Entering Market', 'Post-Competitor Entering Market')) +

    labs(title    = 'Daily New Subscribers Since Competitor Entering Market Drop Below Historical Average\n',
         subtitle = paste0('Comparison Data from Rolling ',num_prior_months_to_avg, '-Months\n'),
         x = 'Day of Week', y = 'Number of Daily New Subscribers',
         caption = paste0('No adjustments made to pre-Competitor Entering Market data. \nIncludes holidays and other any potential event.',
                          '\nPost-Competitor Entering Market Date: ', format(comparison_date, DATE_FORMAT),
                          '\nMax date in sample: ', format(date_max - days(1), DATE_FORMAT),
                          if (date_omit_min != date_omit_max) { paste('\n Dates exluded due to noise in data:', format(date_omit_min, DATE_FORMAT), '-', format(date_omit_max, DATE_FORMAT))}
         )
    ) +


    # Make the y axis in comma seperated values
    scale_y_continuous(labels = comma) +

    # Get old theme and turn off x-axis gridlines
    theme_get() + theme(panel.grid.major.x = element_blank())


  # ------------------------------------------------------------------------------
  # Combine Plots and Output
  # ------------------------------------------------------------------------------

  # Overview
  dashboard_overview <- plot_grid(plot_overview, plot_segment_pie,
                                  nrow = 1, rel_widths = c(2,1))
  # Daily Comparison
  # dashboard_dailySignups <- plot_grid(plot_singupsTrend, plot_new_subscribers_detail,
  #                                     nrow = 2, rel_heights = c(1,1.5))

  # The output of the function
  output <- list('data'  = df_new_subscriber_demo,
                 'plots' = list('overview'     = dashboard_overview,
                                'demographics' = plot_segment,
                                'cumulativeVariance' = plot_new_subscribers_cumulative_var,
                                'detail'       = plot_new_subscribers_detail))

  return(output)
}

# new_subscribers <- subscriber_demographic_analysis(
#   selected_stores = c(1),
#   comparison_date = as.Date('2025-06-01')
# )
# print(new_subscribers$plots)
