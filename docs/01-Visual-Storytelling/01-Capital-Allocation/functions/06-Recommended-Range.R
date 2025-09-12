# Load packages and functions
source('docs//00-Reused-Code/Read-Library-and-Functions.R')


set.seed(1024) # All data is randomized! Seed set for reproducibility


# Function to generate the recommended-range plot
recommended_range <- function(

  ## Working directory
  wrkDir = 'docs//01-Visual-Storytelling//01-Capital-Allocation//data/',

  ## Excel file to read
  xlFileName   = 'Allocations Database.xlsx',
  xlSheetName  = 'Anticipated Ranges - Main',
  xlSheetName2 = 'Anticipated Ranges - Annotation',

  # Plot controls
  titles_facets       = c('Percent Growth', 'Nominal Growth', 'Total Budget'),
  offset_palette      = 5,
  label_nudge_y       = 0.1,
  label_nudge_x       = 0.025,
  label_nudge_x_rect  = 0.0095,
  limits_x_axis       = c(0.9, 1.1),
  size_points         = 13,
  lower_value         = 1,
  upper_value         = 2,
  total_budget_offset = 0.026
) {

  # Label position multipliers per facet
  upper_label_position_rel = c(rep(2.2, 2), 1 + total_budget_offset)
  lower_label_position_rel = c(rep(1.225, 2), 1 - total_budget_offset)


  # READ DATA ====================================================================
  # Pull datasets and helpers from Excel.

  df_base <- read_excel(
    path  = paste0(wrkDir, xlFileName),
    sheet = xlSheetName
  )

  df_base_reference_lines <- read_excel(
    path  = paste0(wrkDir, xlFileName),
    sheet = xlSheetName2
  )


  # PREP DATA ====================================================================
  # Normalize labels, pivot long, build label values, and set facet order.

  df <- df_base |>
    mutate(
      Range_Name = as.factor(str_replace_all(Range_Name, "_", " ")),
      Method = as.factor(str_wrap(Method, 18)),
    ) |>

    pivot_longer(where(is.numeric),
                 names_to = 'Title',
                 values_to = 'Value'
    ) |>

    mutate(
      Label_Value = if_else(
        str_detect(Title, 'Percent'),
        percent(Value, accuracy = 0.1),
        styles::kDollarsFormat(Value, roundToDigit = 1)
      )
    ) |>

    mutate(
      Title = factor(str_replace_all(Title, "_", ' '),
                     levels = titles_facets,
                     labels = paste0(titles_facets, '\n')
      ),
      label_position = ifelse(
        str_detect(Range_Name, 'Min'),
        Value * lower_label_position_rel,
        Value * upper_label_position_rel
      ),
    )


  ## PREP ANNOTATION DATA --------------------------------------------------------
  # Compute ymin/ymax for highlight rects per facet.

  df_annotation_positions <- df |>
    group_by(Title) |>
    summarise(
      ymin = min(Value, na.rm = TRUE),
      ymax = max(Value, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      xmin = 1 - label_nudge_x_rect,
      xmax = 1 + label_nudge_x_rect
    )


  ## PREP ZERO LINES DATA --------------------------------------------------------
  # Format reference line labels and join with rect spans.

  df_reference_lines <- df_base_reference_lines |>
    mutate(
      Title = factor(
        str_replace_all(Title, "_", ' '),
        levels = titles_facets,
        labels = paste0(titles_facets, '\n')
      )
    ) |>
    mutate(
      Label_Value = if_else(
        str_detect(Title, 'Percent'),
        percent(Value, accuracy = 0.1),
        styles::kDollarsFormat(Value, roundToDigit = 1)
      )
    ) |>
    left_join(df_annotation_positions, by = 'Title')


  # CREATE PLOT ==================================================================
  # Build layered ggplot with rects, points, labels, ref lines, and themes.

  dark_palette = styles::scale_dc(
    'color',
    length(unique(df$Title)),
    colorOffset = offset_palette,
    darkenPaletteForTextGeoms = T
  )

  plot_out <- df |>
    ggplot(aes(x = 1, y = Value)) +

    # Background highlight rects
    geom_rect(
      data = df_annotation_positions,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = Title),
      inherit.aes = FALSE
    ) +

    # Points
    geom_point(
      aes(color = Title),
      size = size_points
    ) +

    # Method labels
    geom_text(
      aes(label = Method),
      nudge_x = c(-label_nudge_x, NA, label_nudge_x),
      color = styles::scale_dc('text'),
      size = 3,
      hjust = rep(c(1, NA, 0), 2),
    ) +

    # Min/Max labels
    geom_text(
      aes(
        x = 1,
        y = label_position,
        label = paste0(substr(Range_Name, 0, 3), "\n", Label_Value)
      ),
      color = rep(dark_palette, length(unique(df$Method))),
      size = 4,
      fontface = 'bold',
    ) +

    # Reference line labels
    geom_text(
      data = df_reference_lines,
      aes(
        label = paste0(
          'Reference:\n', Label,
          ifelse(Value == 0, '', paste0('\n', Label_Value))
        )
      ),
      nudge_x = c(rep(label_nudge_x, 2), -label_nudge_x),
      hjust = c(rep(0, 2), 1),
      color = dark_palette,
      size = 3,
      fontface = 'italic'
    ) +

    # Reference line segments
    geom_segment(
      data = df_reference_lines,
      aes(
        x = xmin - label_nudge_x * 0.33,
        xend = xmax + label_nudge_x * 0.33,
        y = Value,
        yend = Value,
        color = Title
      ),
      inherit.aes = FALSE,
      linewidth = 1.25,
    ) +

    # Facets
    facet_wrap(
      ~ Title,
      scales = 'free_y',
      nrow = 1
    ) +

    # Labels
    labs(
      title = 'Recommending Reduced Budget Growth\n',
      caption = paste0(
        rep('<br>', ),
        'Ranges of Growth:<br>',
        '**Long-Run Avg. Net Income Growth**',
        ' up to the ',
        '**Anticipated Inflation**'
      )
    ) +

    # Axis formatting
    scale_x_continuous(limits = limits_x_axis) +

    # Styles
    styles::scale_color_dc(colorOffset = offset_palette) +
    styles::scale_fill_dc(colorOffset = offset_palette) +
    styles::theme_dc(borderMode = 'facet') +

    # Theme cleanup
    theme(
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      panel.grid.major.y = element_blank(),
      strip.text = element_text(face = 'bold', size = 13),
      plot.caption = element_markdown()
    ) +

    coord_cartesian(clip = "off")

  return(plot_out)
}

# recommended_range()
