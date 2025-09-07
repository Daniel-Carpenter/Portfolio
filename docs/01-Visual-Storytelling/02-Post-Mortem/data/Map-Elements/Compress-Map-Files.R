library(sf)
library(janitor) # Clean names of data frame
library(tidyverse)

# Instructions
# Download this data (for updated data)
# 1) (states/counties) https://www.census.gov/cgi-bin/geo/shapefiles/index.php
# Then, go copy and place ALL files into the below directory. States left as an example

currentWkDir = 'docs//01-Visual-Storytelling//02-Post-Mortem//data//Map-Elements//'
nativeAreas <- c('Choctaw', 'Cherokee', 'Creek', 'Seminole')

# Function to read, clean, and save shape files
readAndSaveShpFile <- function(oldShapeFilename,     # .shp file to read in (be sure to inlcude all other files)
                               newShapeFilename,     # New .rds to export
                               nameToInclude = NULL, # c('names', 'to', 'include'),
                               columnCase    = 'lower_camel' # Convert column names to a case, default camelCase
                               ) {
  # Read data
  shpData   <- st_read(oldShapeFilename)

  # Clean data. (need right format. 4326 is the EPSG code for WGS84)
  shpFileClean <- st_transform(shpData, crs = 4326) |>

    # Convert column names to case
    clean_names(columnCase)

  # Filter data based on names if included
  if (!is.null(nameToInclude)) {
    shpFileClean <- shpFileClean |>
      filter(name %in% nameToInclude) |>
      mutate(name = paste(name, 'Nation'))
  }

  # Save as compressed RDS as single data frame
  # saveRDS(shpFileClean |> as.data.frame(),   file = newShapeFilename)

  return(shpFileClean)
}

# Load and save states
states <- readAndSaveShpFile(oldShapeFilename = paste0(currentWkDir, 'states/cb_2022_us_state_500k.shp'),
                             newShapeFilename = paste0(currentWkDir, 'states.rds'))

# Load and save
nativeAmerican <- readAndSaveShpFile(oldShapeFilename = paste0(currentWkDir, 'native-american/tl_2022_us_aiannh.shp'),
                                     newShapeFilename = paste0(currentWkDir, 'native-american.rds'),
                                     nameToInclude = nativeAreas)

# Save combined shape file database
mapShapes <- bind_rows(nativeAmerican, states)
saveRDS(mapShapes, paste0(currentWkDir, 'map-shapes.rds'))
# write.csv(mapShapes, paste0(currentWkDir, 'map-shapes.csv'), row.names = FALSE) # to write as a csv

