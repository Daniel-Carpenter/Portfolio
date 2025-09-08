# Load packages and functions
source('docs//00-Reused-Code/Read-Library-and-Functions.R')


set.seed(1024) # All data is randomized! Seed set for reproducibility



# Function that generates a plot for historical capital allocation trends
allocation_history <- function(
    START_FISCAL_YEAR_SHORT = '19', # Shaded region start year in lollipop plot
    END_FISCAL_YEAR_SHORT   = '23', # Placeholder for potential end-year logic
    LABEL_SIZE              = 2.5   # Text label size for charts
) {

  # RANDOM DATA GENERATION =======================================================
  # Creates synthetic time series with upward allocation trend and category splits
  create_random_data <- function(net_income_base) {
    years <- 2016:2025
    n     <- length(years)
    t     <- seq_len(n)

    # Allocation rate rises log-shaped from 40% to 90% of total
    rate_raw <- log(t)
    rate     <- 0.4 + 0.5 * (rate_raw - min(rate_raw)) / (max(rate_raw) - min(rate_raw))

    # Total amounts (MM$) and category splits: retention, uses, investments
    totals_mn     <- runif(n, 80, 200)
    alloc_out     <- rate * totals_mn
    retention_mn  <- (1 - rate) * totals_mn
    uses_share    <- runif(n, 0.1, 0.9)
    uses_mn       <- alloc_out * uses_share
    investments_mn <- alloc_out * (1 - uses_share)

    # Helper to assemble tidy tables
    build_div <- function(name, vals) {
      tibble::tibble(
        metric_name       = 'nominal_cash_allocation_usd',
        entity            = name,
        fiscal_year       = years,
        fiscal_year_short = as.factor(substring(years, 3)),
        value             = vals
      )
    }

    # Nominal distribution (MM$)
    df_allocations_nominal_distribution <- bind_rows(
      build_div('Retention\nof Cash', retention_mn),
      build_div('Uses\nof Cash', uses_mn),
      build_div('Investments', investments_mn)
    ) |> mutate(entity = as.factor(entity))

    # Percent-of-total allocation series
    df_allocations_percent_of_total <- tibble::tibble(
      metric_name       = 'percent_of_total_allocations',
      entity            = 'Allocations of Cash Rise Unsustainably',
      fiscal_year       = years,
      fiscal_year_short = as.factor(substring(years, 3)),
      value             = rate
    ) |> mutate(entity = as.factor(entity))

    # Weighted average share calculation
    df_allocations_weighted_avg <- df_allocations_nominal_distribution |>
      select(-metric_name, -fiscal_year) |>
      pivot_wider(names_from = 'entity', values_from = 'value') |>
      mutate(total_allocations = rowSums(across(where(is.numeric))))

    sources_total     <- sum(df_allocations_weighted_avg |> select(starts_with('Uses')), na.rm = TRUE)
    total_cash_allocs <- sum(df_allocations_weighted_avg |> select(starts_with('total_')), na.rm = TRUE)
    percent_from_sources <- (total_cash_allocs - sources_total) / total_cash_allocs

    list(
      'df_allocations_nominal_distribution' = df_allocations_nominal_distribution,
      'df_allocations_percent_of_total'     = df_allocations_percent_of_total,
      'percent_from_sources'                = percent_from_sources
    )
  }

  # DATA PREPARATION =============================================================
  # Load synthetic dataset and extract components
  est_net_income <- create_random_data('Net Income')
  df_allocations_nominal_distribution <- est_net_income$df_allocations_nominal_distribution
  df_allocations_percent_of_total     <- est_net_income$df_allocations_percent_of_total
  percent_from_sources_net_income     <- est_net_income$percent_from_sources

  # PLOT DEFAULTS ================================================================
  # Theme elements for consistent alignment and text styling
  alignment_left <- theme(
    plot.caption        = element_text(hjust = 0, face = "italic"),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  )

  # LOLLIPOP PLOT CREATION =======================================================
  # Helper to build weighted-average line + label layers
  plot_wt_avg_line <- function(text_title = 'Wt. Avg.',
                               percent_from_sources,
                               color_name,
                               line_type_name = 'longdash',
                               text_over_line = TRUE) {
    geom_wt_avg_text <- geom_text(
      x     = last(
        unique(df_allocations_percent_of_total$fiscal_year_short)
      ),
      y     = percent_from_sources * (1 + if_else(text_over_line, 0.05, -0.05)),
      label = paste0(text_title, ": ", scales::percent(percent_from_sources)),
      color = styles::scale_dc('color', color_name),
      size  = 3.5,
      alpha = 0.1,
      hjust = 1
    )
    geom_wt_avg_line <- geom_hline(
      yintercept = percent_from_sources,
      color      = styles::scale_dc('fill', color_name),
      linetype   = line_type_name,
      linewidth  = 0.725
    )
    list('geom_wt_avg_text' = geom_wt_avg_text,
         'geom_wt_avg_line' = geom_wt_avg_line)
  }

  # Apply weighted average line builder
  net_income_color <- 'orange'
  geom_plot_wt_avg_net_income <- plot_wt_avg_line(
    'Weighted Avg.',
    percent_from_sources_net_income,
    color_name = net_income_color,
    text_over_line = FALSE
  )

  # Lollipop chart for percent allocations
  caption_covid_area <- 'Gray region highlights periods after change in investment framework policy.'
  caption_wt_avg_line <- 'Line shows the weighted-average percentage cash allocation rate.'

  plot_income_trend <- df_allocations_percent_of_total |>
    ggplot(aes(x = fiscal_year_short, y = value)) +
    labs(
      title   = paste0('Figure 1:\n', str_wrap(
        unique(df_allocations_percent_of_total$entity), 300
      ), '\n'),
      x       = '\nFiscal Year',
      y       = paste0(str_wrap(
        unique(df_allocations_percent_of_total$entity), width = 40
      ), '\n'),
      caption = paste0(
        '\n\n*  ',
        caption_covid_area,
        '.',
        '\n** ',
        caption_wt_avg_line,
        '.'
      )
    ) +
    annotate(
      "rect",
      xmin = START_FISCAL_YEAR_SHORT,
      xmax = Inf,
      ymin = -Inf,
      ymax = Inf,
      alpha = 1 / 5,
      fill = 'grey80'
    ) +
    geom_plot_wt_avg_net_income$geom_wt_avg_text +
    geom_plot_wt_avg_net_income$geom_wt_avg_line +
    geom_segment(
      aes(
        x = fiscal_year_short,
        xend = fiscal_year_short,
        y = 0,
        yend = value
      ),
      color = styles::scale_dc('fill', 'blue'),
      alpha = 0.5,
      linewidth = 0.75
    ) +
    geom_point(aes(y = value, color = 'Nominal'),
               alpha = 0.9,
               size = 2.5) +
    scale_color_manual(
      name = 'Legend',
      breaks = c('Nominal', 'Weighted Avg.'),
      values = c(
        'Nominal' = styles::scale_dc('color', 'blue'),
        'Weighted Avg.' = styles::scale_dc('fill', net_income_color)
      )
    ) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
    styles::theme_dc(legend.position = 'none')



  # BOX PLOT CREATION ============================================================
  # Box plot of nominal allocations per entity over time with outlier annotation
  min_fiscal_year <- min(df_allocations_nominal_distribution$fiscal_year)
  max_fiscal_year <- max(df_allocations_nominal_distribution$fiscal_year)
  caption_time_frame <- paste('Data aggregated annually from fiscal year',
                              min_fiscal_year,
                              'to',
                              max_fiscal_year)

  y_axis_upper_limit <- quantile(df_allocations_nominal_distribution$value, 1)
  df_median_of_nominal_dist <- df_allocations_nominal_distribution |>
    group_by(entity) |>
    summarise(median_val = round(median(value), 1), .groups = 'drop')

  outliers_uses    <- boxplot.stats((
    df_allocations_nominal_distribution |> filter(entity %like% '%Uses%')
  )$value)$out
  outliers_sources <- boxplot.stats((
    df_allocations_nominal_distribution |> filter(entity %like% '%Sources%')
  )$value)$out
  outliers_invest  <- boxplot.stats((
    df_allocations_nominal_distribution |> filter(entity %like% '%Invest%')
  )$value)$out
  outliers_vec     <- c(outliers_uses, outliers_sources, outliers_invest)
  df_outliers      <- df_allocations_nominal_distribution[df_allocations_nominal_distribution$value %in% outliers_vec, ]

  COLORS <- c('blue', 'purple', 'green')

  plot_entity_boxplot <- df_allocations_nominal_distribution |>
    left_join(df_median_of_nominal_dist, by = 'entity') |>
    ggplot(aes(
      x = reorder(entity, desc(value)),
      y = value,
      color = reorder(entity, desc(value)),
      fill  = reorder(entity, desc(value))
    )) +
    labs(
      title   = 'Figure 2 :\nRetention of Cash Wanes\n',
      x       = '',
      y       = 'Total Allocation of Cash (Millions of Dollars)\n',
      caption = paste0('\n* ', caption_time_frame, '.')
    ) +
    geom_boxplot(size = 0.9) +
    scale_fill_manual(values = styles::scale_dc('fill', COLORS)) +
    scale_color_manual(values = styles::scale_dc('color', COLORS)) +
    scale_y_continuous(labels = scales::comma,
                       limits = c(0, y_axis_upper_limit)) +
    styles::theme_dc() +
    theme(legend.position = 'none')


  # Prepare plots for website thumbnails
  plot_entity_boxplot_void <- plot_entity_boxplot + theme_void_thumbnail()

  plot_income_trend_void <- plot_income_trend + theme_void_thumbnail()

  plots_void <- cowplot::plot_grid(plot_income_trend_void, plot_entity_boxplot_void, rel_widths = c(2.75, 2))

  ggsaveToPNGThumbnail(
    plots_void,
    filename = 'docs/01-Visual-Storytelling/01-Capital-Allocation/img/thumbnail',
  )

  # ggsaveToPNGThumbnail(
  #   plots_void,
  #   filename = 'docs/01-Visual-Storytelling/img/thumbnail',
  # )


  # Now add the text after export thumbail
  plot_entity_boxplot <- plot_entity_boxplot +

  geom_text(
    aes(
      x = entity,
      y = median_val,
      label = paste('Median:\n', median_val, 'MM')
    ),
    position = position_nudge(y = -5),
    size = LABEL_SIZE,
    hjust = 0.5
  ) +
    geom_text(
      data = df_outliers,
      aes(
        x = str_wrap(entity, width = 15),
        color = str_wrap(entity, width = 15),
        y = value,
        label = fiscal_year
      ),
      position = position_nudge(x = 0.06),
      hjust = 0,
      size = LABEL_SIZE
    )


  # COMBINE PLOTS ================================================================
  return(
    'plot_allocation_history' = cowplot::plot_grid(plot_income_trend, plot_entity_boxplot, rel_widths = c(2.75, 2))
  )
}


# out <- allocation_history()
#
# ggsave(out, filename = 'temp.png',
#        height = 10,
#        width = 10,
#        scale = 1,
#        dpi = 80
#        )
