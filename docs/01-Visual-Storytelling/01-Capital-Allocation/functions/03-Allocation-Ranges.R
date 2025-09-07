# Load packages and functions
source('docs//00-Reused-Code/Read-Library-and-Functions.R')


set.seed(1024) # All data is randomized! Seed set for reproducibility



# Function that generates a plot of allocation ranges
allocation_ranges <- function(

  # Number of historical years to show before forecast; plotting color palette
  SHOW_N_YEARS_REALIZED = 4,
  PLOT_COLORS           = c('blue', 'green', 'purple')
) {
  # UTILITIES ====================================================================
  # Simple rounding helpers for axis limits
  round_down_to_k <- function(x, k)
    floor(x / k) * k
  round_up_to_k   <- function(x, k)
    ceiling(x / k) * k

  # MAIN FUNCTION ================================================================
  generate_within_range_plots <- function() {
    # DATA GENERATION ============================================================
    # Create a 5-year time series (2022–2026) with upward trend for Uses of Cash
    # and downward trends for Retention and Investments. Ensure all sum to 100%.
    division_labels <- c("Uses of Cash", "Retention of Cash", "Investments")
    division_levels <- paste0("Percent ", division_labels)

    years_vec <- 2022:2026
    n         <- length(years_vec)
    t_index   <- seq_len(n)
    s         <- if (n > 1)
      (t_index - 1) / (n - 1)
    else
      rep(0, n)

    percent_sources <- 0.60 - 0.40 * s  # Retention down
    percent_invest  <- 0.30 - 0.20 * s  # Investments down
    percent_uses    <- 1 - percent_sources - percent_invest  # Uses up

    n_forecast <- 1
    n_realized <- length(years_vec) - n_forecast
    desc_vec   <- c(rep("Realized", n_realized), rep("Forecast", n_forecast))

    df_allocations_base <- tibble::tibble(
      Fiscal_Year              = years_vec,
      Description              = desc_vec,
      `Percent Uses of Cash`   = percent_uses,
      `Percent Retention of Cash` = percent_sources,
      `Percent Investments`    = percent_invest
    )

    # TARGET RANGES ==============================================================
    # Define user-specified lower and upper bounds for each division
    lower_bounds <- c(0.6, 0.3, 0.25)
    upper_bounds <- c(0.4, 0.2, 0.3)

    df_ranges_base <- tibble::tibble(
      Division   = rep(division_levels, each = 2),
      Range_Name = rep(c("Lower_Bound", "Upper_Bound"), times = length(division_levels)),
      Value      = as.numeric(t(
        mapply(function(lo, up)
          c(lo, up), lower_bounds, upper_bounds)
      ))
    )

    df_ranges <- df_ranges_base |>
      pivot_wider(names_from = 'Range_Name', values_from = where(is.numeric))

    # DATA PREPARATION ===========================================================
    # Reshape allocations to long format, join range bounds, apply clean factor labels
    order_levels <- function(column) {
      unique_vals <- unique(column)
      sorted_vals <- c("Realized", unique_vals[!grepl("Realized", unique_vals)])
      factor(column, levels = sorted_vals, ordered = TRUE)
    }

    num_years_of_forecast   <- sum(stringr::str_detect(df_allocations_base$Description, 'Forecast'))
    num_fiscal_years_in_data <- SHOW_N_YEARS_REALIZED + num_years_of_forecast

    df_allocations <- df_allocations_base |>
      filter(Fiscal_Year > (last(Fiscal_Year) - num_fiscal_years_in_data)) |>
      mutate(
        Fiscal_Year       = as.factor(Fiscal_Year),
        Description       = order_levels(Description),
        Fiscal_Year_Short = as.factor(as.numeric(
          stringr::str_sub(as.character(Fiscal_Year), -2, -1)
        ))
      ) |>
      pivot_longer(
        cols = where(is.numeric),
        names_to = 'Division',
        values_to = 'Allocation_Value'
      ) |>
      left_join(df_ranges, by = 'Division') |>
      drop_na() |>
      mutate(
        Division = factor(
          stringr::str_replace_all(Division, '_', ' '),
          levels = division_levels,
          labels = stringr::str_wrap(division_labels, 19)
        )
      )

    # PLOTTING PARAMETERS ========================================================
    # Build labels, axis limits, and shared plot aesthetics
    end_fiscal_year   <- last(df_allocations$Fiscal_Year)
    labels_p1 <- labs(
      title   = 'Historical Allocations Recently Fall Outside Anticipated Allocation Ranges\n',
      x       = '\nFiscal Year',
      y       = 'Percentage Allocation\n',
      caption = '\n*** "Percentage Allocation:" Allocation relative to Sources of Cash'
    )

    ROUND_TO_K <- 0.1
    y_axis_limits_p1 <- c(min(0, round_down_to_k(
      min(df_allocations$Allocation_Value), ROUND_TO_K
    )), max(0.7, round_up_to_k(
      max(df_allocations$Allocation_Value), ROUND_TO_K
    )))

    get_aesthetic_layers <- function(realized_shape = 19,
                                     forecast_shape_1 = 1,
                                     forecast_shape_2 = 7) {
      list(
        styles::theme_dc() +
          theme(
            legend.text = element_text(size = 12),
            legend.box  = 'vertical',
            panel.grid.major.x = element_blank(),
            panel.grid.minor.x = element_blank()
          ),
        scale_color_manual(
          values = styles::scale_dc('color', PLOT_COLORS),
          guide = 'none'
        ),
        scale_fill_manual(
          values  = styles::scale_dc('fill', PLOT_COLORS),
          guide = 'none'
        ),
        scale_shape_manual(
          values = c(realized_shape, forecast_shape_1, forecast_shape_2),
          guide  = guide_legend(override.aes = list(color = styles::scale_dc('gray', 'gray5')))
        ),
        scale_x_discrete(labels = unique(df_allocations$Fiscal_Year)),
        scale_y_continuous(
          labels = scales::percent,
          limits = y_axis_limits_p1,
          breaks = seq(y_axis_limits_p1[1], y_axis_limits_p1[2], by = 0.1)
        )
      )
    }

    # LABEL POSITIONS ============================================================
    # Determine midpoint for placing range labels
    get_middle_factor <- function(factor_column) {
      levels(factor_column)[ceiling(length(levels(factor_column)) / 2)]
    }

    df_label <- df_allocations |>
      filter(Fiscal_Year == get_middle_factor(Fiscal_Year)) |>
      mutate(label_position = (Upper_Bound - Lower_Bound) / 2 + Lower_Bound)

    # PLOT CREATION ==============================================================
    # Render range rectangles, actual allocations, and labels by division
    LABEL_SIZE     <- 4
    size_of_points <- 8

    plot_allocations_percent <- df_allocations |>
      ggplot(aes(x = Fiscal_Year)) +
      geom_rect(
        aes(
          xmin = as.numeric(Fiscal_Year) - 0.5,
          xmax = as.numeric(Fiscal_Year) + 0.5,
          ymin = Lower_Bound,
          ymax = Upper_Bound,
          fill = Division
        ),
        alpha = 0.6
      ) +
      geom_text(
        data = df_label,
        aes(
          x = Fiscal_Year,
          y = label_position,
          label = stringr::str_wrap("Anticipated Allocation Range", nchar('Anticipated Allocation') - 1),
          color = Division
        ),
        size = LABEL_SIZE,
        hjust = 0.5,
        alpha = 0.6
      ) +
      geom_point(
        aes(
          y = Allocation_Value,
          color = Division,
          fill = Division,
          shape = Description
        ),
        size = size_of_points,
        alpha = 0.8
      ) +
      geom_text(
        data = df_allocations |> filter(Fiscal_Year == last(df_allocations$Fiscal_Year)),
        aes(
          y = Allocation_Value,
          color = Division,
          label = scales::percent(Allocation_Value, accuracy = 0.1)
        ),
        vjust = 0,
        nudge_y = c(-0.045, rep(0.03, 2)),
        size = 3
      ) +
      facet_grid( ~ Division) +
      labels_p1 +
      get_aesthetic_layers()

    plot_allocations_percent
  }

  # RUN FUNCTION ================================================================
  generate_within_range_plots()
}


# allocation_ranges()
