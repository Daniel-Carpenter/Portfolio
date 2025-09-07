# Main Packages ----------------------------------------------------------------

# My Package list
myPackages <- c(
  'dplyr',
  'tibble',
  'ggplot2',
  'readr',
  'tidyr',
  'stringr',
  'forcats',
  'lubridate',
  'purrr',
  'readr',
  'readxl',
  'janitor',
  'skimr',
  'magrittr',
  'scales',
  # 'DescTools',
  'cowplot',
  # 'grDevices',
  # 'DT',
  'gt',
  'gtExtras',
  'ggtext',
  'ggrepel',
  'ggforce',
  'plotly',
  'htmlwidgets',
  'data.table',
  'colorspace',
  'patchwork',
  'ggtext',
  'DT'
)


# Install packages not on this device
packagesToInstall <- myPackages[!myPackages %in% rownames(installed.packages())]
# print(paste('Installing:', unlist(packagesToInstall)))


for (thePackage in packagesToInstall) {
  install.packages(thePackage)
}

# Load library
lapply(myPackages, function(thePackage) suppressMessages(require(thePackage,
                                                                 character.only = TRUE,
                                                                 quietly        = TRUE,
                                                                 warn.conflicts = FALSE)))
# Clean environment
rm(thePackage, packagesToInstall, myPackages)


# Palette and color Styles -----------------------------------------------------

# Install devtools if not installed (for GitHub Package)
if (!require("devtools")) install.packages("devtools")

# Install the styles repository
if (!require("styles")) install_github("Daniel-Carpenter/styles")
require("styles")

# Others -----------------------------------------------------------------------

# library(data.table)
# library(R.cache) # Used to store data in memory on code execution
# library(magrittr)
# library(ggthemes)
# library(scales)
# library(formattable)
# library(grid)


# SQL Connection ---------------------------------------------------------------

# library(dbplyr)
# library(odbc)
# library(DBI)
# library(glue)


# Data Wrangling
# library(tidyverse)
# library(skimr)
# library(lubridate) # dates

# Modeling
# library(MASS)
# library(caret) # Modeling variants like SVM
# library(earth) # Modeling with Mars
# library(pls)   # Modeling with PLS
# library(glmnet) # Modeling with LASSO

# Aesthetics
# library(knitr)
# library(cowplot)  # multiple ggplots on one plot with plot_grid()
# library(scales)
# library(kableExtra)
# library(ggplot2)
# library(inspectdf)

# Hold-out Validation
# library(caTools)

# Data Correlation
# library(GGally)
# library(regclass)

# RMSE Calculation
# library(Metrics)

# p-value for OLS model
# library(broom)

# ncvTest
# library(car)
