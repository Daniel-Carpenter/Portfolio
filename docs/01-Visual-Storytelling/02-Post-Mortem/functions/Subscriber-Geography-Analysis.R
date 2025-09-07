# Load packages and functions
source('docs//00-Reused-Code//Read-Library-and-Functions.R')
library(leaflet)   # Create html maps
library(htmltools) # For hovering tool tip creation
library(ggmap)
library(sf)


set.seed(1024) # All data is randomized! Seed set for reproducibility



# Function that generates a map of US states with citizen data
# Broad steps
# Load in shapes for county (or city) level # https://www2.census.gov/geo/tiger/TIGER2023/COUSUB/?C=M;O=A
# Load in population per county (or city)
# Toggle for subscribers per capita
# Highlight border around tribal territory
subscriber_geography_analysis <- function(
    currentWkDir = 'docs//01-Visual-Storytelling//02-Post-Mortem//',
    dataDirEnd = 'data//',
    color = 'blue'
) {

  # Set up color palette
  fill_branded  =  styles::scale_dc('color', color)
  color_branded =  styles::scale_dc('color', color)
  palette_continuous = c(
    colorspace::lighten(fill_branded, 0.5),
    colorspace::lighten(fill_branded, 0.35),
    color_branded
  )

  # Name of the data directory
  dataDir = paste0(currentWkDir, dataDirEnd)

  # Google maps API key
  # Need this if updating the coordinates
  # register_google(key = '')

  stateNamesOmit <- c()


  # Data Load ====================================================================

  # Read all CSV files in a directoryName with the same header
  readCSVsFromDir <- function(directoryName) {

    # List all CSV files in the directoryName
    csvFiles <- list.files(directoryName, pattern = "\\.csv$", full.names = TRUE)

    # Read each CSV file into a list of data frames
    data_frames <- map(csvFiles, read_csv)

    # Optional: Combine all data frames into one (if they have the same header)
    combinedCSVs <- bind_rows(data_frames)

    return(combinedCSVs)
  }


  # Citizen states
  df_subscriberstates = readCSVsFromDir(paste0(dataDir, 'Heatmap//')) |>
    janitor::clean_names("lower_camel")

  # Shape file for state and native american maps (see R file in directory for shape file combination)
  mapShapes <- readRDS(paste0(dataDir, 'Map-Elements//map-shapes.rds'))



  # Data Prep ====================================================================

  ## Change certain words to NA value --------------------------------------------
  replaceWithNA <- function(df, values) {
    for (i in values) {
      df[df == i] <- NA
    }
    return(df)
  }

  # Replace unknowns with NAs
  unknownValues <- c("<unknown>", "<Unknown>", "<>")
  df_subscriberstatesGrouped <- replaceWithNA(df_subscriberstates, unknownValues) |>

    # Sort descending by citizen count
    arrange(desc(subscribers)) |>

    # Add an address for Google Maps API to look up lat/lon coordinates
    mutate(googleAddress = str_replace_all(paste(state, country), 'NA ', ''),

    )


  # Ensure ordering remains same across dataset
  # Note non-US States are in dataset, so just get rid of them
  mapShapes                 <- subset(mapShapes, mapShapes$name %in% df_subscriberstatesGrouped$state)
  df_subscriberstatesGrouped <- subset(df_subscriberstatesGrouped, df_subscriberstatesGrouped$state %in% mapShapes$name)

  # Match the order of each dataset
  mapShapes <- mapShapes[order(match(mapShapes$name, df_subscriberstatesGrouped$state) ), ]

  ## Pull in Coordinates to mapShapes ---------------------------------------------

  # Unique listing of mapShapes in data set
  uniqueAdresses <- unique(df_subscriberstatesGrouped$googleAddress)
  paste(length(uniqueAdresses), 'unique mapShapes')

  # Now convert the addresses to the lat/lon coordinates
  # coordinates <- geocode(uniqueAdresses, source = 'google')
  # write.csv(coordinates, paste0(dataDir, 'Map-Elements/', 'state-coordinates.csv'),
  # row.names = F)
  coordinates <- read_csv(paste0(dataDir, 'Map-Elements/state-coordinates.csv'))



  # Combine the addresses and lat/lon coordinates
  addressAndCoords <- cbind(uniqueAdresses, coordinates)

  # Join the lat/lon coordinates back to the main citizen/county dataset
  df_subscriberstatesGrouped <- df_subscriberstatesGrouped |>

    # join the coords
    left_join(addressAndCoords,
              by = c('googleAddress' = 'uniqueAdresses') )



  # Map ==========================================================================

  # Remove filtered states before creating color gradient. must happen here
  df_subscriberstatesGrouped <- df_subscriberstatesGrouped  |> filter(!(state %in% stateNamesOmit))


  ## Colors for fill -------------------------------------------------------------
  numBins = 4

  # Create a continuous palette
  dc_ContinuousPalette <- colorRampPalette(c(styles::scale_dc('gray', "white"), palette_continuous)
  )
  # Function to get elements of the palette
  getPalette <- colorNumeric(
    palette = dc_ContinuousPalette(numBins),
    domain  = df_subscriberstatesGrouped$subscribers
  )
  baseOpacityLevel = 0.8


  ## Tool tip --------------------------------------------------------------------

  # Function that wraps each line in html format for newlines
  boldText <- function(text) {
    return(paste0('<b>', text, '</b>'))
  }

  # Create the data label
  df_subscriberstatesGrouped <- df_subscriberstatesGrouped |>
    mutate(theLabel = paste0(
      paste0(state, br()),
      paste(boldText(comma(subscribers)), 'Subscribers')
    )
    )


  ## Map -------------------------------------------------------------------------

  # Remove filtered states before creating color gradient. must happen here
  mapShapes <- mapShapes |> filter(!(name %in% stateNamesOmit))

  # Create map
  map_output <- leaflet() |>

    # Customized background map
    addProviderTiles(providers$Esri.WorldGrayCanvas) |>

    # Zoom in to location of Oklahoma
    setView(lat  =  35.5,
            lng  = -98.5,
            zoom = 4.25 # 6.5
    ) |>
    # Now add the lines for Oklahoman countiess
    addPolygons(data = mapShapes$geometry,

                # Add some colorss
                color        = scale_dc('gray', 'gray7'), # Color of lines
                weight       = 1,      # Weight of line
                smoothFactor = 0.8,         # Smooths out the lines for faster loading. Default is 1. > 1 is faster but lose details


                # Heatmap element - uses the palette function and actual data
                fillColor   = getPalette(df_subscriberstatesGrouped$subscribers),
                fillOpacity = baseOpacityLevel,

                # Highlight the state borders on hover
                highlight = highlightOptions(
                  weight    = 1.5,
                  color     = scale_dc('color', 'gray'),
                  fillColor = scale_dc('fill',  'gray'),
                  bringToFront = FALSE

                ),

                # Tooltip when hovering over regions
                label = lapply(df_subscriberstatesGrouped$theLabel, HTML) # Creates using HTML mapping
    ) |>

    # Legend
    addLegend(title    = 'Number of<br>Subscribers',
              values   = df_subscriberstatesGrouped$subscribers,
              pal      = getPalette,
              opacity  = baseOpacityLevel - 0.1,
              position = 'bottomright',


    )

  return(map_output)
}


# subscriber_geography_analysis()
