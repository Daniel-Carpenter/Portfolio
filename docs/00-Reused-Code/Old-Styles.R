# ==============================================================================
# Goal: Streamline colors used that align with the Old Style Guide
# ==============================================================================

# Install ggplot2 since you will be using it
if(!require(ggplot2)) install.packages('ggplot2'); library(ggplot2)


# Fill Colors ------------------------------------------------------------------

  # Function for formatting the output of the fill, line, and base colors
  # This is done separately just to reduce noise in the file

  # Mainly, this just either displays the names of the colors or not when returing
  # It also allows you to return 1 or all of the colors
  formatReturnColors <- function(colors, thePalette, displayNames) {
    # Return the ENTIRE vector of colors
    if (is.null(colors)) {

      # If you need want to see which colors are associated with a hex code
      # Only use this if you just need to know the names
      if (displayNames)
        return (thePalette)

      # Else just return the hex code, this is best for functionality
      else
        return (paste0(thePalette))
    }


    # Return a SINGLE color
    else {

      # If you need want to see which colors are associated with a hex code
      # Only use this if you just need to know the names
      if (displayNames)
        return (thePalette[colors])

      # Else just return the hex code, this is best for functionality
      else
        return (paste0(thePalette[colors]))
    }
  }

# Fill Colors ------------------------------------------------------------------

  # For example, the fill colors on a bar chart. These are displayed in the style guide

  # Function to retrieve all, some, or one fill color
  # Default returns the entire list of fill colors
  getFillCol <- function(..., displayNames = FALSE) {
    colors <- c(...) # get colors from function params

    # Define the fill colors
    fillPalette <- c('blue'       = '#A8D3DE', 'red'     = '#F2A896',
                     'green'      = '#C2DCD1', 'purple'  = '#C7BBDB',
                     'orange'     = '#F7D2B4', 'yellow'  = '#FEE6BA',
                     'lightPurple'= '#D0C3C5', 'charcoal'= '#A2ACB7')

    # Based on inputs, return one or all colors, either displaying the names or not
    return ( formatReturnColors(colors, fillPalette, displayNames) )
  }


# Line Colors ------------------------------------------------------------------

  # For example, the lines (or accents) on a chart. These are just a bit darker than the fills

  # Function to retreive all, some, or one line color
  # Default returns the entire list of line colors
  getLineCol <- function(..., displayNames = FALSE) {
    colors <- c(...) # get colors from function params

    # Define the line colors
    linePalette <- c('blue'       = '#7A9BA3', 'red'     = '#B27A6D',
                     'green'      = '#8EA199', 'purple'  = '#9289A1',
                     'orange'     = '#B69A83', 'yellow'  = '#BBA988',
                     'lightPurple'= '#988F90', 'charcoal'= '#767D86')

    # Based on inputs, return one or all colors, either displaying the names or not
    return ( formatReturnColors(colors, linePalette, displayNames) )
  }


# Base Colors ------------------------------------------------------------------

  # For example, this would be the navy backgrounds on the slide decks

  # Function to retrieve all, some, or one base color
  # Default returns the entire list of base colors
  getBaseCol <- function(..., displayNames = FALSE) {
    colors <- c(...) # get colors from function params

    # Define the line colors
    basePalette <- c('navy'  = '#3F4953',
                     'navy1' = '#394149',
                     'navy2' = '#2D343B')

    # Based on inputs, return one or all colors, either displaying the names or not
    return ( formatReturnColors(colors, basePalette, displayNames) )
  }


# Other Colors -----------------------------------------------------------------

  getTextCol       <- function() { return('#666666') } # Text Color
  getBorderCol     <- function() { return('#E6E6E6') } # Border Color
  getBackgroundCol <- function() { return('#FAFAFA') } # Background Color
  getGrayedOutCol  <- function() { return('grey87' ) } # COnsistent Grey for graying out objects


# ggplot Theme -----------------------------------------------------------------

    Old_Theme <- theme(

      # All Font  Size
      text                = element_text(size  = 11),

      # Panel
      panel.background    = element_rect(fill     = 'white',
                                         colour   = 'white'),
      panel.border        = element_rect(fill     = NA,
                                         color    = getBorderCol()),
      panel.grid.major.x  = element_blank(),
      panel.grid.major.y  = element_line(linetype = "solid",
                                         color    = "grey95",
                                         size     = 0.2),

      # Main Title
      plot.title          = element_text(colour = getTextCol(),
                                         size   = 16,
                                         hjust  = 0),
      plot.subtitle       = element_text(colour = getTextCol(),
                                         size   = 13,
                                         hjust  = 0),

      # Axis Titles
      axis.text           = element_text(colour = getTextCol(),
                                         size   = 11),
      axis.title          = element_text(colour = getTextCol(),
                                         size   = 12),

      # Facet Titles
      strip.text          = element_text(colour = getTextCol(),
                                         size   = 12),
      strip.background    = element_rect(fill   = "#EAEAEA",
                                         color  = getBorderCol(),
                                         size   = 0.40),

      # Caption Font
      plot.caption        = element_text(colour = getTextCol(),
                                         size   = 8.5,
                                         hjust  = 1),

      # Legend
      legend.background   = element_rect(fill   = "grey99",
                                         colour = "grey85",
                                         size   = 0.40),
      legend.text         = element_text(colour = getTextCol()),
      legend.position     = "top",
      legend.title        = element_blank(),

      # Margin
      plot.margin         = margin(t = 15,
                                   b = 30,
                                   r = 30,
                                   l = 30,
                                   unit = "pt")
    )

    # Set Theme Active - Use `theme_get()` when using ggplot
    theme_set(Old_Theme)


# Remove any variables that only should be encapsulated in this file
rm(Old_Theme)
