source('docs//00-Reused-Code/Read-Library-and-Functions.R')


set.seed(1024) # All data is randomized! Seed set for reproducibility)



# Random data
get_data_is <- function(

  # Define constants
  date_min                = as.Date("2024-01-01"),
  date_max                = as.Date("2025-08-01"),
  comparison_date         = as.Date('2025-05-01'),
  comparison_drop         = 0.5,
  store_levels            = c("Flagship Store", "Other Stores"),
  tier_levels             = c("Tier 1", "Tier 2"),
  supplier_levels           = c("Supplier 1", "Supplier 2", "Supplier 3", "Supplier 4"),
  section_of_store_levels = c("Store Section 1", "Store Section 2", "Store Section 3", "Store Section 4", "Store Section 5", "Store Section 6", "Store Section 7")
) {


  # Number of days in range
  n_dates <- as.integer(date_max - date_min) + 1

  # Generate base combinations
  date_factors <- tibble(date_of_trx = seq(date_min, date_max, by = "day"))
  factor_combos <- crossing(
    store_id            = store_levels,
    tier_id             = tier_levels,
    supplier_id           = supplier_levels,
    section_of_store_id = section_of_store_levels
  )

  # Cross join dates with factor combinations
  base_df <- crossing(date_factors, factor_combos)

  # Total rows
  n <- nrow(base_df)

  # Generate dataset with random values
  output_data <- base_df |>
    mutate(
      distinct_num_stores     = runif(n,   min =    0,     max =   1000),
      num_days_store_was_open = runif(n,   min =   28,     max =     31),
      revenue                 = runif(n,   min =  1e5,     max =    5e5),
      expense                 = runif(n,   min =  9e4,     max =    1.8e5),
      discounts               = runif(n,   min =  5e3,     max =    1e4),
      operating_income        = runif(n,   min =  5e3,     max =    1e4),
      transactions            = runif(n,   min =  1e5,     max =    2e5),
      avg_transactions_amt    = runif(n,   min =    5,     max =     10)
    ) |>

    # If after comparison date, then set numeric column to 50% of historical distribution
    mutate(

      # flag rows after comparison_date
      after_comp_date = date_of_trx > comparison_date,

      # scale numeric cols by 50% after comparison_date
      across(c(operating_income, revenue, discounts), ~ if_else(after_comp_date, .x * comparison_drop, .x))
    ) |>

    select(-after_comp_date)

  return(output_data)
}
