# Load packages and functions
source('docs//00-Reused-Code/Read-Library-and-Functions.R')


set.seed(1024) # All data is randomized! Seed set for reproducibility



# Function that generates plot for investment opportunity
investment_opportunity <- function(

  BEG_PORTFOLIO_VALUE     = 100000000, # Starting portfolio value
  ANNUAL_PERCENT_INCREASE = 0.05,
  DISTRIBUTION_RATE       = 0.04,
  BEG_YEAR                = 2030, # Starting fiscal year
  END_YEAR                = 2046, # Ending fiscal year
  NUM_YEARS               = END_YEAR - BEG_YEAR # Number of years in the projection
) {

  # READ AND CLEAN RAW FILES =====================================================

  # Read data
  df_base <- data.frame(
    Fiscal_Year = BEG_YEAR:END_YEAR,
    Ending_Portfolio_Value = BEG_PORTFOLIO_VALUE * (1 + ANNUAL_PERCENT_INCREASE)^(0:NUM_YEARS) # Start at 1000 and increase by 5% each year,
  ) |>
    mutate(
      Sustainable_Annual_Distribution = Ending_Portfolio_Value * DISTRIBUTION_RATE
    )



  # PLOTS ========================================================================

  ## PLOT FUNCTIONS --------------------------------------------------------------

  # Returns every other row from a data frame (e.g., even or odd indexed rows)
  filter_every_other_row <- function(df_base, keep_odd = TRUE) {
    idx <- seq_len(nrow(df_base))
    df_base[idx %% 2 == if (keep_odd) 1 else 0, ]
  }


  ## DATA PREP -------------------------------------------------------------------

  # Data points for ever other
  df_points <- df_base |>
    filter_every_other_row() |>

    # Custom text label for first and all other points
    mutate(
      formatted_value = styles::kDollarsFormat(Ending_Portfolio_Value, roundToDigit = 0),
      is_edge_year    = Fiscal_Year %in% c(first(Fiscal_Year), last(Fiscal_Year)),
      label_text = if_else(
        is_edge_year,
        paste0("NAV of\n", formatted_value, "\nin FY '", str_sub(Fiscal_Year, -2)),
        formatted_value
      )
    )


  color_white  = 'white'
  color_text = styles::scale_dc('gray', 'gray3')

  # Secondary color point
  color_chosen_2nd = 'purple'
  color_fill_2nd   = styles::scale_dc('fill',  color_chosen_2nd)
  color_accent_2nd = styles::scale_dc('color', color_chosen_2nd)

  # Focal point
  color_fill_focal   = color_white
  color_accent_focal = color_accent_2nd


  maxPortfolioEndingValue = max(df_base$Ending_Portfolio_Value)
  topOfChart = maxPortfolioEndingValue * 1.1



  ## ________________----
  ## BUBBLE PLOT - SUS. DISTRIBUTION --------------------------------------------------

  df_plot_sus_distr <- df_base |>

    ### Mapping ------------------------------------------------------------------
  ggplot(aes(
    x = Fiscal_Year,
    y = 0
  )) +


    ### Geometry ------------------------------------------------------------------

  #### Points ----
  geom_point(data = df_points,
             aes(size = Sustainable_Annual_Distribution),
             color  = color_accent_focal,
             fill   = color_fill_focal,
             pch    = 21, # Type of point that allows us to have both color (border) and fill.
             stroke = 1   # The width of the border
  ) +

    # Increase base Size
    scale_size_continuous(range = c(0.01, 22), labels = kDollarsFormat) +

    #### Text Label ----
  geom_label(data = df_points,
             aes(label = kDollarsFormat(Sustainable_Annual_Distribution, roundToDigit = 0)),
             color         = color_text,
             fill          = alpha(color_white, 0.65),
             size          = 3.5,
             fontface      = "bold",
             show.legend   = FALSE,
             label.size    = NA,
             label.padding = unit(0.15, 'lines')
  ) +


    ### Labels - (Titles, Axis, Captions) ------------------------------------------
  labs(
    title    = 'Perpetual Annual Distribution | Investments\n',
    subtitle = 'By waiting to use Investments, the annual amount that can be withdrawn while preserving the full\nbalance would be ...\n\n',
    x        = NULL,
    y        = 'Perpetual Annual\nDistribution\n'
  ) +


    ### Theme and Styling --------------------------------------------------------
  styles::scale_color_dc() +
    styles::theme_dc() +
    theme(

      # Whitespace
      plot.margin = margin(t = 5, r = 5, b = 0, l = 5),      # minimize bottom margin
      axis.text.x = element_text(
        size   = 7,
        face   = "italic",
        color  = styles::scale_dc('gray', 'gray5'), # small axis on top
        margin = margin(t = 0) # remove spacing above x labels
      ),
      axis.ticks.length = unit(0, "pt"),                    # remove tick spacing
      plot.caption = element_blank(),                       # ensure no caption space

      # remove background geometry
      legend.position    = 'none',
      axis.text.y        = element_blank(),
      axis.line.x          = element_blank(),
      axis.ticks         = element_blank(),
      panel.grid.major.y = element_blank(),
      plot.subtitle = element_text(
        face = "italic",
        color = styles::scale_dc('gray', 'gray5')
      ),

    ) +

    ### Axis Formatting and Lengths ----------------------------------------------
  scale_x_continuous(breaks = df_points$Fiscal_Year, position = 'top')

  df_plot_sus_distr # display



  ## ________________----
  ## AREA PLOT - NAV -------------------------------------------------------------

  df_plot_nav <- df_base |>

    ### Mapping ------------------------------------------------------------------
  ggplot(aes(
    x = Fiscal_Year,
    y = Ending_Portfolio_Value
  )) +


    ### Geometry -----------------------------------------------------------------

  #### Area ----
  geom_area(
    fill  = color_fill_2nd,
    alpha = .4) +

    #### Line ----
  geom_line(
    color = color_accent_2nd,
    alpha = 0.75,
    linewidth = 1
  ) +

    #### Points ----
  geom_point(
    data   = df_points,
    fill   = color_accent_2nd,
    color  = color_white,
    size   = 3,
    pch    = 21, # Type of point that allows us to have both color (border) and fill.
    stroke = 1.5 # The width of the border
  ) +

    #### Arrows ----
  geom_segment(
    data = df_points,
    aes(yend = topOfChart                  ,                             # top
        y    = Ending_Portfolio_Value + 0.04 * maxPortfolioEndingValue), # bottom
    arrow = arrow(type = 'closed', length = unit(2/5, 'lines')),
    color = styles::scale_dc('gray', 'gray7')
  ) +

    #### Text Label ----
  geom_label_repel(
    aes(
      label = label_text),
    data          = df_points,
    vjust         = 1,            # Align top edge of label to the point
    nudge_y       = -(maxPortfolioEndingValue * 0.075),        # Move label below the point
    color         = color_text,
    fill          = alpha(color_white, 0.4),
    size          = 3,
    fontface      = "bold",
    # segment.size  = 0.15,
    segment.color = "transparent", # no leader line
    hjust         = 0.5,
    direction     = "y",
    show.legend   = FALSE,
    label.size    = NA,
  ) +


    ### Labels - (Titles, Axis, Captions) ------------------------------------------
  labs(
    x        = '\nFiscal Year',
    y        = 'Net Asset\nValue (NAV)\n',
    caption  = '\nData is adjusted for inflation.'

  ) +


    ### Theme and Styling --------------------------------------------------------
  styles::scale_color_dc() +
    styles::theme_dc() +
    theme(
      panel.grid.major.y = element_blank(),
      axis.text.y       = element_blank(),
      axis.ticks.y      = element_blank()
    ) +

    ### Axis Formatting and Lengths ----------------------------------------------
  scale_y_continuous(labels = kDollarsFormat) +
    scale_x_continuous(breaks = df_points$Fiscal_Year)

  # df_plot_nav # display



  ## COMBINE PLOTS ---------------------------------------------------------------

  return(
    # Combines two ggplots vertically using patchwork, with height split
    (df_plot_sus_distr + theme(plot.margin = margin(5, 10, 0, 10))) /
      (df_plot_nav + theme(plot.margin = margin(0, 10, 5, 10))) +
      plot_layout(heights = c(15, 85))
  )
}


# investment_opportunity()
