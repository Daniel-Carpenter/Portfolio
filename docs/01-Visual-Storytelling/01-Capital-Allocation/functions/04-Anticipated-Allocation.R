# Load packages and functions
source('docs//00-Reused-Code/Read-Library-and-Functions.R')


set.seed(1024) # All data is randomized! Seed set for reproducibility



# Function that generates plots for anticipated allocations
anticipated_allocation <- function(
    SHOW_N_YEARS_REALIZED = 5, # Number of realized or budgeted years to show (will always show forecast)
    xlFileName                         = 'docs//01-Visual-Storytelling//01-Capital-Allocation//data/Allocations Database.xlsx',
    xlSheetName_Database               = 'Database',
    xlSheetName_ranges = 'Allocation Ranges'
) {

  # READ AND CLEAN RAW FILES =====================================================

  # Function to generate plots for net income or  bases ===============
  generateAnticipatedGrowthPlots <- function() {
    # Title for net income name
    netIncomeBaseTitle = 'Realized Net Income without Adjustments'



    # Read in data from excel sheet =======================

    ## 1 - Realized and anticipated allocations ---------------------------------------
    df_allocations_base <- read_excel(path  = paste0(xlFileName), sheet = xlSheetName_Database) |> drop_na()


    ## 2 - Expected Growth Formula - percentage allocations for Uses of Cash ----
    df_ranges_base <- read_excel(path  = paste0(xlFileName), sheet = xlSheetName_ranges)



    # Minor Data Prep ==============================================================

    # Filters data for fiscal year - want forecast and recent history as defined at top of file
    numYearsOfForecast = sum(str_detect(df_allocations_base$Description, 'Forecast'))
    numFiscalYearsInData   = SHOW_N_YEARS_REALIZED + numYearsOfForecast

    # Consolidate colors, text for within or outside of general allocation bounds
    anticipated_rangesColors = c(WITHIN_BOUNDS_COLOR  = 'green',
                                  OUTSIDE_BOUNDS_COLOR = 'red')
    anticipated_rangesText   = c(WITHIN_BOUNDS_TEXT   = 'Within Expected Growth Formula',
                                  OUTSIDE_BOUNDS_TEXT  = 'Outside Expected Growth Formula')

    # Indices of above
    WITHIN_BOUNDS_IDX = 1
    OUTSIDE_BOUNDS_IDX = 2

    # Consolidate ranges into single vector
    anticipated_ranges = unique(df_ranges_base$Percent_Allocation)

    # Indices to reference of the allocation framework
    LOWER_BOUND_IDX = 3
    UPPER_BOUND_IDX = 1


    # Begin data processing ------------------------------------------------------
    df_allocations <- df_allocations_base |>

      # Only include N months of data
      filter(Fiscal_Year > (last(Fiscal_Year) - numFiscalYearsInData)) |>

      # Convert fiscal year to factor
      mutate(
        Fiscal_Year = as.factor(Fiscal_Year),

        # Create an abbreviated fiscal year number
        # I.e., rightmost two characters of fiscal year, then converted to numeric -> factor
        Fiscal_Year_Short = as.factor(as.numeric(str_sub(
          as.character(Fiscal_Year), -2
        ))),

        # Get the difference from the anticipated allocation and the realized allocation
        # E.g., Positive values: Allocated less than the framework suggests
        anticipateExceedsRealized = factor(
          Anticipated_Percent_Allocation_to_Use > Percent_Allocation_to_Use,
          levels = c(TRUE, FALSE),
          labels = c('Anticipated Allocation Exceeds Realized Allocation', 'Converse')
        ),

        # Add description whether realized was within, above, or below range
        # If within the upper and lower bound of Uses of Cash allocation
        boundsDescription = if_else(
          Percent_Allocation_to_Use <  anticipated_ranges[UPPER_BOUND_IDX]
          &
            Percent_Allocation_to_Use >= anticipated_ranges[LOWER_BOUND_IDX] ,
          anticipated_rangesText[WITHIN_BOUNDS_IDX],
          # then label as outside bounds
          anticipated_rangesText[OUTSIDE_BOUNDS_IDX]   # else label as within bounds
        ),

        # Convert the bounds text description to a sorted factor for plotting
        boundsDescription = factor(boundsDescription, levels = anticipated_rangesText, labels = anticipated_rangesText)
      ) |>

      # Pivot such that all numeric columns are in a single column
      pivot_longer(
        cols      = where(is.numeric),
        names_to  = 'Allocation_Metric',
        values_to = 'Allocation_Value'
      ) |>

      # Labels for Shapes: Realized Allocation vs. Anticipated Growth Rate
      mutate(
        Allocation_Type = if_else(
          str_detect(Allocation_Metric, 'Realized'),
          'Realized',
          'Anticipated'
        ),

        Allocation_Type = factor(
          Allocation_Type,
          levels = c('Realized', 'Anticipated'),
          labels = c(
            'Realized or Budgeted Allocation',
            'Est. Allocation using Anticipated Growth Rate'
          )
        )
      )




    # PLOT 1 - NOMINAL GROWTH OF ALLOCATIONS - Anticipated vs. REALIZED =============

    ## Plot Variables --------------------------------------------------------------

    ## fiscal years in graph to help with plotting
    startFiscalYear              = first(df_allocations$Fiscal_Year)          # start
    endFiscalYear                = last(df_allocations$Fiscal_Year)           # end of series
    numFiscalYearsInPlot         = length(unique(df_allocations$Fiscal_Year)) # Total number
    labelPosition_LastFiscalYear = length(unique(df_allocations$Fiscal_Year)) + 0.4



    ## SHARED LAYERS BETWEEN PLOTS -------------------------------------------------

    # Function to get common elements ---
    getSharedLayers <- function() {
      # Base theme and Theme and stack the legendx
      theTheme <- styles::theme_dc() +
        theme(legend.text = element_text(size = 11), legend.box = 'vertical')

      # THe Colors and Shapes of points
      theShapes <- scale_shape_manual(values = c(19, 10),
                                      guide  = guide_legend(override.aes = list(color = styles::scale_dc('gray', 'gray5'))))

      # Scale of axis
      xAxisScale <- scale_x_discrete(labels = unique(df_allocations$Fiscal_Year))


      return(list(theTheme, theShapes, xAxisScale))
    }



    ## Plot Variables --------------------------------------------------------------

    ## Quick toggles for graph elements
    sizeOfPoints        = 5
    MILLIONS_CONVERSION = 1000000
    noGrowthLineColor   = 'grey80'
    noGrowthTextColor   = 'grey50'
    LABEL_SIZE          = 3.5
    SIZE_OF_POINTS      = 7

    ## Labels on plots
    titleVar_p1    = paste0('Uses of Cash Grow Fasted than Expected Growth Formula\n')
    subtitleVar_p1 = 'Figure 2: Annual Allocation Growth | Bounds show annual change potential using the Expected Growth Formula.'
    titleVar_p0    = 'Actual Growth Recently Outpaces Exected Allocation\n'

    df_allocationsNomGrowth <- df_allocations |>

      ## Filter to only nominal variables
      filter(str_detect(Allocation_Metric, 'Nominal')) |>

      ## Convert to millions
      mutate(
        Allocation_Value = Allocation_Value / MILLIONS_CONVERSION,

        ## Isolate the bound type (min or max)
        boundType = if_else(
          str_detect(Allocation_Metric, 'Max'),
          'Max_Nominal_Growth',
          if_else(
            str_detect(Allocation_Metric, 'Min'),
            'Min_Nominal_Growth',
            'Growth'
          )
        )
      ) |>

      ## Do not include the anticipated bounds
      filter(!(
        str_detect(Allocation_Metric, 'Anticipated')
        &
          str_remove_all(boundType, '_Nominal_Growth') %in% c('Min', 'Max')
      ))



    ### Function to create the text and arrows--------------------------------------
    ### Also adds arrow that points to the upper or lower bound
    add_genAllocationTextAndLine <- function(RangeName,
                                             y_nudge           = textBegin_nudge,
                                             textColor         = arrowAndTextColor,
                                             labelSize         = LABEL_SIZE,
                                             arrowAndTextColor = 'grey50',
                                             arrowStart_nudge  = 1,
                                             segmentEnd_nudge  = arrowStart_nudge + max(df_allocationsNomGrowth$Allocation_Value) *
                                               0.3,
                                             textBegin_nudge   = segmentEnd_nudge + 3,
                                             lineSegmentSize   = 0.2,
                                             arrowSize         = 0.1,
                                             label_nudge       = 0.125) {
      #### Set the titles based on the inputs --------------------------------------
      if (RangeName == 'Upper') {
        titlePrefix = 'Max'
        flipScaler = 1

      } else {
        titlePrefix = 'Min'
        flipScaler = -1
      }

      # Get the y values where the text will be plotted
      df_boundsForTextOnGraph <<- df_allocationsNomGrowth |>
        filter(Fiscal_Year == first(Fiscal_Year),
               str_detect(boundType, titlePrefix))

      yTextPosition = df_boundsForTextOnGraph$Allocation_Value



      #### Get the description and the percentage for the bounds -------------------

      # Filter to the correct range
      df_ranges <- df_ranges_base |>
        filter(str_detect(Range, RangeName))

      # Get the values
      percentBound      = as.numeric(paste0(df_ranges[['Percent_Allocation']]))
      percentBoundTitle = paste0(df_ranges[['Range']])


      #### Create the text and line objects for the bound --------------------------
      outputText = annotate(
        'text',
        x      = as.numeric(startFiscalYear) - label_nudge,
        y      = yTextPosition + flipScaler * y_nudge,
        label  = paste0(
          titlePrefix,
          '. Potential Growth (at ',
          percentBound * 100,
          '%)'
        ),
        color  = textColor,
        hjust  = 0,
        size   = labelSize
      )

      outputLine = annotate(
        "segment",
        x     = as.numeric(startFiscalYear) - label_nudge,
        xend  = as.numeric(startFiscalYear) - label_nudge,
        y     = yTextPosition + flipScaler * segmentEnd_nudge,
        yend  = yTextPosition + flipScaler * arrowStart_nudge,
        color = arrowAndTextColor,
        size  = lineSegmentSize,
        arrow = arrow(length = unit(arrowSize, "cm"), type = "closed")
      )

      return(list(outputText, outputLine))
    }



    ### Function to add the general allocation growth ranges -----------------------
    add_genAllocationGrowthRanges <- function(lineColor               = styles::scale_dc('gray', 'gray5'),
                                              rangeLineType           = "longdash",
                                              sizeOfLineBrackets      = 0.45,
                                              sizeOfUpperBracketWidth = 0.3) {
      # Get the data and filter it to only growth
      df <- df_allocationsNomGrowth %>% filter(boundType != 'Growth')

      # Part A - Plot the Range: i.e., line between points
      theRange <- geom_line(
        data = df,
        aes(y = Allocation_Value),
        color    = lineColor,
        size     = sizeOfLineBrackets,
        linetype = rangeLineType
      )

      # Part B - Add error bars
      theBrackets <- geom_errorbar(
        data = df,
        aes(ymin = Allocation_Value, ymax = Allocation_Value),
        width = sizeOfUpperBracketWidth,
        size  = sizeOfLineBrackets,
        color = lineColor
      )

      return(list(theRange, theBrackets))
    }



    # Function to add the ranges and text
    # The reason for making this is to be able to omit these elements when creating another plot
    # Without the allocation ranges (for simplicity)
    add_anticipatedRangesAndText <- function(includeRangesAndText = TRUE) {
      if (includeRangesAndText) {
        return(list(
          # The Nominal Growth Ranges (Min and Max Brackets)
          add_genAllocationGrowthRanges(),

          # Add the text and arrow for the upper/lower bound
          map(c('Upper', 'Lower'), add_genAllocationTextAndLine)
        ))
      } else {
        return(NULL)
      }
    }



    # Function that returns shared aesthetics of the graph
    # If the layer order didn't matter, a function would not be the best approach.
    # Normally I would choose to have a BASE object that shared the layers,
    # BUT, you must add the points AFTER the ranges, which the ranges are removed in one graph
    add_sharedAesthetics <- function(titleVar    = NULL,
                                     subtitleVar = NULL,
                                     captionVar  = NULL,
                                     xAxisVar    = NULL,
                                     sizeOfPoints = SIZE_OF_POINTS,
                                     growthTextPosition = -5) {
      # Filter to only growth variables
      df <- df_allocationsNomGrowth |> filter(boundType == 'Growth')

      return(
        list(
          # 0.1 - Add a No Growth Line
          geom_hline(
            yintercept = 0,
            color = noGrowthLineColor,
            linetype = 'solid',
            linewidth = 0.6
          ),

          # 0.2 - Add a No Growth Line
          annotate(
            'text',
            x = labelPosition_LastFiscalYear,
            y = growthTextPosition,
            label = 'No Annual Growth Line',
            hjust = 1,
            size = LABEL_SIZE,
            color = noGrowthTextColor
          ),


          # 1.1 - Plot the points: i.e., Anticipated and Base Allocations
          geom_point(
            data = df,
            aes(
              y     = Allocation_Value,
              shape = Allocation_Type,
              color = boundsDescription
            ),
            size = sizeOfPoints
          ),

          # 1.2 - Include lines to connect the anticipate and realized values for ease of viewing
          geom_line(
            data = df,
            aes(y     = Allocation_Value, color = boundsDescription),
            size  = 0.45
          ),

          # Y Axis formatting
          scale_y_continuous(labels = comma),

          # Labels
          labs(
            title    = titleVar,
            subtitle = paste0(subtitleVar, '\n'),
            x        = xAxisVar,
            y        = paste0('Growth of Nominal Allocations\n(Millions of Dollars)\n'),
            caption  = captionVar
          ),

          # Add the shared elements of plots
          getSharedLayers()
        )
      )
    }



    ## I. CREATE PLOT WITH RANGES -------------------------------------------------

    plot_allocationsNomGrowthRanges <- df_allocationsNomGrowth |>

      # 1 - Plot x-Axis
      ggplot(aes(x = Fiscal_Year)) +

      # 2 - Add the Expected Growth Formula growth ranges
      add_anticipatedRangesAndText(includeRangesAndText = TRUE) +

      # 3 - Anticipated vs. realized points, and
      #     Aesthetics like labels, shared layers, scales, etc.
      add_sharedAesthetics(
        titleVar    = titleVar_p0,
        subtitleVar = subtitleVar_p1,
        x = '\nFiscal Year',
        growthTextPosition = -2,
        captionVar = paste0(
          '\n\n* "',
          str_flatten(anticipated_rangesText, '" and "'),
          '" coloring is based only on realized allocations, not anticipated allocations.'
        )
      ) +

      # Color scale
      scale_color_manual(values = styles::scale_dc('fill', paste0(anticipated_rangesColors)),
                         labels = anticipated_rangesText)


    plot_allocationsNomGrowthRanges # display



    # PLOT 2 - INDEX GROWTH ======================================================

    df_allocationsGrowthPlotRaw <- df_allocations |>
      filter(
        str_detect(Allocation_Metric, 'Percent_Growth'),
        !str_detect(Allocation_Metric, 'Income')
      ) |>

      # Remove uneeded data
      select(Fiscal_Year,
             Description,
             Fiscal_Year_Short,
             Allocation_Type,
             Allocation_Value) |>

      # Pivot into single column
      pivot_wider(names_from = Allocation_Type, values_from = Allocation_Value)

    # Set last two columns's and first rows to 1
    numCols = ncol(df_allocationsGrowthPlotRaw)
    FIRST_ROW = 1
    LAST_TWO_COL_IDX = numCols:(numCols - 1)

    # Set first numeric row to index value 1
    df_allocationsGrowthPlotRaw[FIRST_ROW, LAST_TWO_COL_IDX] <- 1

    # Accumulate the growth from the index
    df_allocationsGrowthPlotRaw[, LAST_TWO_COL_IDX] <- map(df_allocationsGrowthPlotRaw[, LAST_TWO_COL_IDX], cumsum)


    df_allocationsGrowthPlot <- df_allocationsGrowthPlotRaw |>

      # Put back into a single column
      pivot_longer(
        cols = where(is.numeric),
        names_to  = 'Allocation_Type',
        values_to = 'Allocation_Value'
      ) |>

      # COnvert anything as a string to a categorical
      mutate(
        across(where(is.character), as.factor) ,

        Allocation_Type = factor(
          Allocation_Type,
          levels = sort(unique(Allocation_Type), decreasing = TRUE),
          labels = sort(unique(Allocation_Type), decreasing = TRUE)
        )
      )



    ## II. CREATE PLOT INDEX PLOT ------------------------------------------------

    paletteOffset = 5

    plot_allocationsGrowthPlot <- df_allocationsGrowthPlot |>

      # Map data to variables
      ggplot(
        aes(
          x     = Fiscal_Year,
          y     = Allocation_Value,
          color = Allocation_Type,
          group = Allocation_Type
        )
      ) +

      # Create the line
      geom_line(linewidth = 0.85) +

      # Add points
      geom_point(
        aes(fill = Allocation_Type),
        pch = 21,
        color = 'white',
        stroke = 1,
        size = 3
      ) +

      # Get shared layers from last graph
      getSharedLayers() +

      # Manually color the lines
      scale_fill_dc(colorOffset = paletteOffset, overrideWithAccent = T) +
      scale_color_dc(colorOffset = paletteOffset) +

      # Labels on Graph
      labs(
        title    = titleVar_p1,
        subtitle = paste0(
          'Figure 1: Index of Anticipated Growth vs. Actual Growth of Uses of Cash\n'
        ),
        x        = NULL,
        y        = 'Index of Growth Measure\n\n',
        caption = NULL
      ) +

      # Y Axis fomatting as percent
      scale_y_continuous(labels = percent)

    plot_allocationsGrowthPlot # display



    # EXPORT PLOTS ===============================================================

    ## Create the a PDF for a raw copy - used to use when Copy/Paste into PPT with Header ---

    # Combine plots into a list
    combinedPlotsRaw <- list('index_growth'             = plot_allocationsGrowthPlot,
                             'ranges' = plot_allocationsNomGrowthRanges)

    # Combine plots onto a grid
    combinedPlotsFormatted <- plot_grid(plotlist    = combinedPlotsRaw,
                                        rel_heights = c(1.05, 1),
                                        ncol        = 1)

    return(combinedPlotsRaw)
  }

  # Run the function
  generateAnticipatedGrowthPlots()
}


# anticipated_allocation()
