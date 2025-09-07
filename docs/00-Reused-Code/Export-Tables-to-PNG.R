# Required libraries
library(gt)
library(webshot2)  # webshot2 is preferred over webshot (install via remotes::install_github("rstudio/webshot2"))


# Save a gt table to PNG
gtSaveToPNG <- function(gt_table,
                        outputFileName,
                        outWidth      = NULL, # automatically infers size/scale from internal gt rendering
                        outHeight     = NULL,
                        outResolution = NULL,
                        SCALED_BY     = NULL,
                        includeExtension = TRUE
                        ) {

  output_filename = paste0(outputFileName, ifelse(includeExtension, ".png", ""))

  # direct save to PNG (under the hood uses webshot2)
  gtsave(
    data = gt_table,
    filename = output_filename
  )
}
