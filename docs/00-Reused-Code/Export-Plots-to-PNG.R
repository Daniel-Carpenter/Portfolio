library(devtools)
library(cowplot)

# This is mainly to write ggplots in times new roman, or export a cowplot efficiently

# Function to write **single** ggplot to a certain file extension (PDF, PNG)
ggsaveToPNG <- function(ggplotObj,
                        outputFileName,
                        outWidth      = 1400,
                        outHeight     = 1000,
                        outResolution = 140,
                        SCALED_BY     = 4
                        ) {
  
  # Convert GGplot to cowplot
  outputPlot <- cowplot::plot_grid(ggplotObj)
  
  # Create blank file
  png(filename = paste0(outputFileName, '.png'),
      width    =  SCALED_BY       * outWidth,
      height   =  SCALED_BY       * outHeight,
      res      = (SCALED_BY + 1.15) * outResolution # Bigger = larger text
  )
  
  # Print the plots
  print(outputPlot)
  
  dev.off() # write the file
}


# Function to write **cowplot** object to a certain file extension (PDF, PNG)
cowplotSaveToPNG <- function(combinedPlots,
                               outputFileName,
                               outWidth      = 1500,
                               outHeight     = 950,
                               outResolution = 72,
                               SCALED_BY     = 2
                               ) {
  # Create blank file
  png(filename = paste0(outputFileName, '.png'),
      width    =  SCALED_BY       * outWidth,
      height   =  SCALED_BY       * outHeight,
      res      = (SCALED_BY + 1.15) * outResolution # Bigger = larger text
  )
  
  # Print the plots
  print(combinedPlots)
  
  dev.off() # write the file
}
