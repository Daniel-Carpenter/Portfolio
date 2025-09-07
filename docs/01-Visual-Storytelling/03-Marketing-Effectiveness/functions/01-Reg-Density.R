# Load packages and functions
source('docs//00-Reused-Code//Read-Library-and-Functions.R')
library(lmtest)


set.seed(1024) # All data is randomized! Seed set for reproducibility



# Function that generates plots for marketing analysis
marketing_effectiveness <- function(

  # FY 24 start through Jan 24
  startDate       = as.Date('2021-11-01'),
  highlightedDate = as.Date('2022-12-01'),
  endDate         = as.Date('2023-12-01'),

  periodGrouping = 'week' # 'month'
) {

  periodGroupingLabel = ifelse(periodGrouping == 'week', 'Weekly', 'Monthly')
  pointsAlphaLevel    = ifelse(periodGrouping == 'week', 0.15, 0.15)
  pointsSizeLevel     = ifelse(periodGrouping == 'week', 2.5,  3)



  # RANDOM DATA ==================================================================

  MILLIONS = 1e6

  # Sequence from start to end date
  PERIOD_PTTM = seq.Date(from = startDate, to = highlightedDate, by = periodGrouping)
  numPeriods_PTTM = length(PERIOD_PTTM)

  set.seed(1024)

  df_period_PTTM <- data.frame(
    Period       = PERIOD_PTTM,
    Discounts    = runif(n = numPeriods_PTTM, 4, 9),
    Revenue      = rnorm(n = numPeriods_PTTM, mean = 50, sd = 5),
    Gross_Profit = rnorm(n = numPeriods_PTTM, mean = 30, sd = 3),
    Net_Income   = rnorm(n = numPeriods_PTTM, mean = 20, sd = 2)
  )

  # Sequence from start to end date
  PERIOD_TTM = seq.Date(from = highlightedDate, to = endDate, by = periodGrouping)
  numPeriods_TTM = length(PERIOD_TTM)

  df_period_TTM <- data.frame(
    Period       = PERIOD_TTM,
    Discounts    = runif(n = numPeriods_TTM, 9, 12),
    Revenue      = rnorm(n = numPeriods_TTM, mean = 50, sd = 5),
    Gross_Profit = rnorm(n = numPeriods_TTM, mean = 30, sd = 3),
    Net_Income   = rnorm(n = numPeriods_TTM, mean = 15, sd = 5)
  )

  # Base data frame, no edits
  df_base = bind_rows(df_period_PTTM, df_period_TTM)

  df <-  df_base |>
    mutate(
      # Scale all numeric columns to millions
      across(where(is.numeric), ~ .x * MILLIONS)
    )


  df[['Discounts_TTM']]    <- zoo::rollapply(df_base$Discounts, 12, sum, na.rm = TRUE, fill = NA, align = 'right')
  df[['Gross_Profit_TTM']] <- zoo::rollapply(df_base$Gross_Profit, 12, sum, na.rm = TRUE, fill = NA, align = 'right')
  df[['Revenue_TTM']]      <- zoo::rollapply(df_base$Revenue, 12, sum, na.rm = TRUE, fill = NA, align = 'right')


  df <- df |>
    mutate(DiscountsPerGross_Profit_TTM  = Discounts_TTM / Gross_Profit_TTM,
           RevenuePerDiscounts           = Revenue_TTM   / Discounts_TTM,
           Net_IncomePerDiscountsNominal = Net_Income    / Discounts,
    ) |>
    drop_na()



  # LOLLIPOP =--------------------------------------------------------------------

  millionsOfDollars <- function(x) dollar(x / 1e6, prefix = "$", suffix = " MM")
  eomonthFormat = '%B %Y'


  createGraph <- function(
    column, filename,
    axisLabelFun      = millionsOfDollars,
    lollipopColor     = 'base',
    yAxisNudgeFactor  = 0.3,
    includeLabel      = TRUE,
    includeRangeLabel = FALSE,
    fixedNudgeLeader  = NULL
  ) {

    # Calculate the minimum value of the specified column and reduce it by 10%
    minValueOfColumn <- min(df |> select({{column}}), na.rm = TRUE)
    yAxisMinValue  <- minValueOfColumn  * 0.9
    yLabelNudgeAmt <- minValueOfColumn * yAxisNudgeFactor

    # Calculate the max value of the specified column and reduce it by 10%
    maxValueOfColumn <- max(df |> select({{column}}), na.rm = TRUE)
    yAxisMaxValue <- maxValueOfColumn  * 1.15


    # Min max labels
    if (includeRangeLabel) {
      labelRange <- df |>
        filter({{column}} == minValueOfColumn |
                 {{column}} == maxValueOfColumn)

      dataLabelRange <- list(

        # leader line
        geom_segment(data = labelRange,
                     aes(x    = Period,
                         xend = Period,
                         y    = ({{column}} + yLabelNudgeAmt) * 0.98,
                         yend = {{column}} + ifelse(is.null(fixedNudgeLeader), {{column}} * 1.015, fixedNudgeLeader)
                     ),
                     color = scale_dc('gray', 'gray5'),
                     alpha = 0.5,
                     linewidth  = 0.75,
                     arrow = arrow(type = 'closed', length = unit(3, 'pt')),
                     show.legend = FALSE
        ),

        # Add data label
        geom_text(data = labelRange,
                  aes(label = paste0(dollar(round({{column}}, 0)), '\n', format(Period, '%b-%y')),
                      x = Period),
                  vjust = 0,
                  nudge_y = yLabelNudgeAmt,
                  size = 4,
                  color = scale_dc('color', lollipopColor),
                  show.legend = FALSE
                  # fontface = 'bold'
        )
      )
    } else{
      dataLabelRange <- list()
    }

    # Last period label
    if (includeLabel) {


      dataLabel <- list(

        # leader line
        geom_segment(data = df |> filter(Period == highlightedDate),
                     aes(x    = Period,
                         xend = Period,
                         y    = ({{column}} + yLabelNudgeAmt) * 0.98,
                         yend = {{column}} * 1.03
                     ),
                     color = scale_dc('gray', 'gray5'),
                     alpha = 0.5,
                     linewidth  = 0.75,
                     arrow = arrow(type = 'closed', length = unit(3, 'pt')),
                     show.legend = FALSE
        ),

        # Add data label
        geom_text(data = df |> filter(Period == highlightedDate),
                  aes(label = paste0('Competitor\nExpand\nMarket'),
                      x = Period),
                  vjust = 0,
                  hjust = 1,
                  nudge_y = yLabelNudgeAmt,
                  size = 4,
                  color = scale_dc('text'),
                  fontface = 'bold',
                  show.legend = FALSE
        )
      )
    } else {
      dataLabel = list()
    }


    plot_out <- df |>
      ggplot(aes(x = Period,
                 y = {{column}}
      )) +

      # Labels
      labs(title    = paste(filename, ifelse(includeLabel, paste('Shifts in', format(highlightedDate, eomonthFormat)), ''), '\n'),
           subtitle = 'Aggregate Trailing Twelve Month (TTM) Totals by Line\n',
           x        = '\nMonth-End Date',
           y        = paste(filename, '\n'),
           caption = ""
      ) +

      # Build a lollipop chart
      geom_segment(aes(x    = Period,
                       xend = Period,
                       y    = yAxisMinValue,
                       yend = {{column}}
      ),
      color = scale_dc('color', lollipopColor),
      alpha = 0.5,
      linewidth  = 0.75) +

      geom_point(alpha = 0.9,
                 size  = 2,
                 color = scale_dc('color', lollipopColor)
      ) +

      dataLabel + # last period label
      dataLabelRange +  # min max label

      # Brand
      scale_y_continuous(labels = axisLabelFun, limits = c(yAxisMinValue, yAxisMaxValue)) +
      scale_fill_dc() +
      scale_color_dc() +
      styles::theme_dc() +
      theme(legend.position = 'none')
  }


  # Discounts
  plot_discount_trend <- createGraph(
    Discounts_TTM, 'Discounts Trend',
    lollipopColor    = 'blue',
    yAxisNudgeFactor = 0.15
  )

  # Discounts % of Gross Profit
  plot_discount_per_gp <- createGraph(
    DiscountsPerGross_Profit_TTM, 'Discounts as Percentage of Gross Profit Trend',
    axisLabelFun     = percent,
    lollipopColor    = 'gray',
    yAxisNudgeFactor = 0.15
  )

  # Revenue per $1 of Discounts
  plot_rev_per_gp <- createGraph(
    RevenuePerDiscounts, 'Revenue per $1 of Discounts Trend',
    lollipopColor     = 'green',
    axisLabelFun      = dollar,
    includeLabel      = FALSE,
    includeRangeLabel = TRUE,
    yAxisNudgeFactor  = 0.15,
    fixedNudgeLeader  = 0.25
  )



  # SCATTER 1 ----------------------------------------------------------------------

  # Start of TTM period
  ttmDate      <- max(df$Period) - years(1)
  priorTTMDate <- max(df$Period) - years(2)

  df_plot <- df |>
    mutate(yearNum = as.factor(year(Period)),
           isTTM   = if_else(Period >= ttmDate, 'TTM', 'Prior Periods'),
           isHighlightedDate   = if_else(Period >= highlightedDate, format(highlightedDate, eomonthFormat), 'Prior Periods')
    )

  df_plot$isHighlightedDate = factor(df_plot$isHighlightedDate, levels = unique(df_plot$isHighlightedDate))


  # confidence interval
  ci = 0.95

  plot_scatter <- df_plot |>

    # Mapping
    ggplot(aes(x = Discounts,
               y = Net_Income,
               fill  = isTTM,
               color = isTTM
    )) +

    # Linear trends and CI
    geom_smooth(method = 'lm',
                level = ci) +

    # Points
    geom_point(
      size = pointsSizeLevel,
      alpha = pointsAlphaLevel
    ) +

    # Axis formatting
    scale_x_continuous(labels = millionsOfDollars) +
    scale_y_continuous(labels = millionsOfDollars) +

    # Brand
    scale_fill_dc() +
    scale_color_dc() +
    styles::theme_dc() +
    theme(legend.box = 'vertical'
    ) +

    # Labels
    labs(title    = 'Discounts Rise while Incremental Net Income Diminishes\n',
         subtitle = paste('Trailing Twelve Months (TTM) vs. Pre-TTM Comparison | Aggregated',
                          periodGroupingLabel, '\n'),
         x        = '\nDiscounts',
         y        = 'Net Income\n',
         caption = paste0('\n**"TTM" periods defined as: ',
                          format(ttmDate, eomonthFormat), ' through ', format(max(df_plot$Period), eomonthFormat), '.',
                          '\n***Highly correlated data help identify higher-level changes, but do not infer causal relationships.',
                          "\n****Shaded regions display the linear trends' ",
                          ci*100, "% confidence intervals."
         )
    )



  # HYPOTHESIS TEST --------------------------------------------------------------

  if (periodGrouping == 'week') {


    df_plotTest <- df_plot |>
      filter(Period >= priorTTMDate) |>
      mutate(comparisonPeriod = if_else(Period >= ttmDate, 'TTM', 'Prior TTM'),
      )

    # Calculate medians for each comparisonPeriod
    medianOfPeriods <- df_plotTest |>
      group_by(comparisonPeriod) |>
      summarise(medianValue = median(Net_IncomePerDiscountsNominal, na.rm = TRUE),
                variance = var(Net_IncomePerDiscountsNominal, na.rm = TRUE),
                obs = n())

    obsPerGroup = mean(medianOfPeriods$obs)


    ## Net_Income PER Discounts DENSITY ----------------------------------------------
    # Density charts displaying difference in TTM vs. Prior TTM

    plot_density_void <- df_plotTest |>
      ggplot(aes(
        x = Net_IncomePerDiscountsNominal,
        color = comparisonPeriod,
        fill  = comparisonPeriod
      )) +

      # DEnsity plots
      geom_density(alpha = 0.2) +

      # Median values
      geom_vline(data = medianOfPeriods,
                 aes(xintercept = medianValue,
                     color = comparisonPeriod),
                 linetype = "dashed",
                 linewidth = 1.25,
                 alpha = 1
      ) +

      scale_x_continuous(labels = dollar) +
      scale_fill_dc() +
      scale_color_dc() +
      styles::theme_dc() +

      labs(title    = 'Net Income Decreases per $1 of Discounts\n',
           subtitle = paste('Trailing Twelve Months (TTM) vs. Prior TTM Comparison | Aggregated',
                            periodGroupingLabel, '\n'),
           x        = '\nNet Income per $1 of Discounts\n(Millions of Dollars)',
           y        = 'Density\n',
           caption = paste0('\n**"TTM" periods defined as: ',
                            format(ttmDate, eomonthFormat), ' through ', format(max(df_plot$Period), eomonthFormat), '.',
                            '\n***"Prior TTM" periods defined as: ',
                            format(priorTTMDate, eomonthFormat), ' through ', format(ttmDate - days(1), eomonthFormat), '.',
                            '\n****Observations per group: ', round(obsPerGroup), ' (weeks per year).'
           )
      )

    # Add the text on lines
    plot_density <- plot_density_void +

      geom_text(data = medianOfPeriods,
                aes(x = medianValue,
                    y = c(0.6, 0.35),
                    label = paste0(comparisonPeriod, '\nMedian\n', dollar(round(medianValue, 1)))),
                hjust = c(0, 1),
                nudge_x = 0.1 * c(1, -1),
                size = 4.5,
                fontface = 'bold',
                show.legend = FALSE
      )


    ## T-TEST - Net_Income PER Discounts  --------------------------------------------

    df_test <- df_plotTest |>
      ungroup() |>
      select(Period, comparisonPeriod, Net_IncomePerDiscountsNominal)

    df_ttm <- df_plotTest |>
      filter(comparisonPeriod == 'TTM')

    df_priorPeriods <- df_plotTest |>
      filter(comparisonPeriod != 'TTM')

    # Ensure equal sample size across groups
    nrow(df_ttm) == nrow(df_priorPeriods)


    ### t test dataset  ----------------------------------------------------------

    # df_testFinal <- data.frame(TTM       = df_ttm$Net_IncomePerDiscountsNominal,
    #                            PRIOR_TTM = df_priorPeriods$Net_IncomePerDiscountsNominal
    # )

    # # Perform the t-test
    # autoTTest(df_testFinal$TTM,
    #           df_testFinal$PRIOR_TTM,
    #           alpha = 1 - ci
    # )
  }

  plot_density_void <- plot_density_void + theme_void_thumbnail()

  ggsaveToPNGThumbnail(
    plot_density_void,
    filename = 'docs/01-Visual-Storytelling/03-Marketing-Effectiveness/img/thumbnail'
  )

  return(list(
    'plot_scatter'         = plot_scatter,
    'plot_density'         = plot_density,
    'plot_discount_trend'  = plot_discount_trend,
    'plot_discount_per_gp' = plot_discount_per_gp,
    'plot_rev_per_gp'      = plot_rev_per_gp
  ))
}


# marketing_effectiveness()
