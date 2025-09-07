# GOAL:
#   1. Read Library, and
#   2. Read reusable functions
#
# Author: Daniel Carpenter


# INPUT - Location of this folder
reusedCodeDir <- 'docs//00-Reused-Code'

# Packages
if (!require('purrr'))     install.packages('purrr')
if (!require('grDevices')) install.packages('grDevices')

# Load files/packages in the reusable code directory
# Filter to include only .R files (case-insensitive)
filesToSource <- list.files(reusedCodeDir, pattern = '\\.[Rr]$', full.names = TRUE)

# Don't include this R script
indices_to_remove <- grep('Read-Library-and-Functions.R', filesToSource)
filesToSource <- filesToSource[-indices_to_remove]

# Source files
numFilesLoaded <- length(try(map(filesToSource, source)))
