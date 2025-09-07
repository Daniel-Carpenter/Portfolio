# Load packages and functions
source('docs//00-Reused-Code//Read-Library-and-Functions.R')
library(lmtest)


set.seed(1024) # All data is randomized! Seed set for reproducibility



# Main objective: Generate randomized panel data for U.S. ZIP codes across multiple months and save as CSV
# Input Parameters =============================================================
selected_states = c('OK', 'AR')
min_date = as.Date('2024-07-01')   # Start date for MONTH_END
max_date = as.Date('2025-08-01')   # End date for MONTH_END
filename_zip_codes_raw   = 'docs//01-Visual-Storytelling//03-Marketing-Effectiveness//functions//02-Map/ZIP_Locale_Detail.xls'
filename_map_data_output = 'docs//01-Visual-Storytelling//03-Marketing-Effectiveness//functions//02-Map/Patron-Map-Data.csv'



# Load ZIP Code Data ===========================================================
df_zip_codes <- read_xls(filename_zip_codes_raw) |>   # Import Excel file
  filter(`PHYSICAL STATE` %in% selected_states) |>  # Keep only selected states
  select(
    RESIDENTIAL_ZIP_CODE = `DELIVERY ZIPCODE`        # Keep ZIP code column
  ) |>
  mutate(RESIDENTIAL_ZIP_CODE = as.character(RESIDENTIAL_ZIP_CODE)) |>  # Ensure ZIP stored as string
  distinct()                                                           # Drop duplicates

num_zip_codes <- nrow(df_zip_codes)  # Count ZIP codes for randomization sizing



# Function: Create Enumerated Panel ============================================
create_panel <- function(
    df_zip_codes,
    num_source_prop_group_ids = 2,  # Number of source property group IDs
    num_metric_ids            = 4,  # Number of metric IDs
    num_calculation_ids       = 4   # Number of calculation IDs
) {
  df_zip_codes |>

    # cartesian product: ZIP x all ID combos
    crossing(
      SOURCE_PROP_GROUP_ID = 1:num_source_prop_group_ids,
      METRIC_ID            = 1:num_metric_ids,
      CALCULATION_ID       = 1:num_calculation_ids
    ) |>
    mutate(
      # one simulated value per ZIP–ID combo
      value = runif(n(), min = -250000, max = 500000)
    ) |>
    group_by(
      RESIDENTIAL_ZIP_CODE,
      SOURCE_PROP_GROUP_ID,
      METRIC_ID,
      CALCULATION_ID
    )
}



# Create Monthly Sequence ======================================================
MONTH_END_seq = seq(min_date, max_date, by = 'month')  # Full sequence of months



# Generate Full Panel Data =====================================================
df_map_data <- map_dfr(
  MONTH_END_seq,
  ~ create_panel(df_zip_codes) |>
    mutate(MONTH_END = .x),      # attach the current month to each randomized draw
  .id = NULL
) |>
  relocate(MONTH_END, .before = everything())  # keep date column first for readability

# Save Output ----
fwrite(df_map_data, filename_map_data_output, row.names = F)  # Export panel data to CSV
