# Load packages and functions
source('docs//00-Reused-Code/Read-Library-and-Functions.R')


set.seed(1024) # All data is randomized! Seed set for reproducibility

# Helper: Generate random bimodal distribution
rbimodal <- function(n, mean_1, sd_1, mean_2, sd_2) {

  n_is_odd = n %% 2 != 0

  n_1 = n / 2
  n_2 = n / 2

  if (n == 0)
    return(rnorm(n, 0, 0))

  else if (n_is_odd) {
    n_1 = (n - 1) / 2
    n_2 = (n + 1) / 2
  }

  bimodal_dist <- c(
    rnorm(n_1, mean_1, sd_1),
    rnorm(n_2, mean_2, sd_2)
  )

  return(bimodal_dist)
}

# Generate daily signups that strictly increase to the comparison date, then drop below the pre-pivot median and continue strictly decreasing.
generate_subscriber_signups <- function(
    date_seq,
    comparison_date,
    peak_value = 200
) {
  n <- length(date_seq)
  pivot_idx <- max(which(date_seq <= comparison_date), 1L)
  pivot_idx <- min(pivot_idx, n)

  # strictly increasing up to the pivot (peak at comparison_date)
  inc <- if (pivot_idx > 0) floor(seq(1, peak_value, length.out = pivot_idx)) else integer(0)

  dec_len <- n - pivot_idx
  if (dec_len > 0) {
    pre_median <- stats::median(inc)                         # historical median (pre-pivot)
    first_after <- max(1L, min(peak_value - 1L, floor(pre_median) - 1L))  # force sharp drop below median

    k <- dec_len - 1L
    if (k > 0L) {
      x <- seq_len(k) / k                                    # normalized distance along the tail
      v_raw <- 1 + (first_after - 1) * (1 - x^3)             # convex taper to 1
      v <- floor(v_raw)

      # enforce strict monotonic decrease despite flooring
      v <- pmin(v, (first_after - 1L) - seq_along(v))        # each step at least 1 less than previous
      v <- pmax(v, 1L)                                       # floor at 1

      dec <- c(first_after, v)
    } else {
      dec <- first_after
    }
  } else {
    dec <- integer(0)
  }

  tibble(
    date        = date_seq,
    num_subscribers = c(inc, dec)
  )
}





# Random subscriber data
get_data_subscriber <- function(
    date_min        = as.Date("2024-01-01"),
    comparison_date = as.Date("2025-05-01"),
    date_max        = as.Date("2025-08-01"),
    store_id     = c("Flagship Store"),
    gender_levels   = c("Male", "Female", "Neutral"),
    age_bin_levels  = 1:6,
    drivetime_bins  = 1:7
) {

  n_origin = 100

  max_store_id = 1
  gender_ID          = 3
  age_bin_id         = 6
  drivetime_bin_id   = 7


  date_seq <- seq.Date(from = date_min, to = date_max, by = "day")
  n_dates  <- length(date_seq)

  subscriber_signups <- generate_subscriber_signups(
    date_seq = date_seq,
    comparison_date = comparison_date
  )


  create_subscriber_panel <- function(subscriber_num_start, subscriber_num_end, date_of_signup, n_subscribers, min_val_id = 1) {

    mean_adj = if_else(subscriber_num_start > 50000, 60, 180)

    data.frame(
      store_id           = round(runif(n = n_subscribers, min = min_val_id, max = max_store_id)),
      subscriber_id          = subscriber_num_start:subscriber_num_end,
      date_of_trx        = date_of_signup,
      gender             = round(runif(n = n_subscribers, min = min_val_id, max = gender_ID)),
      age_bin_id         = round(runif(n = n_subscribers, min = min_val_id, max = age_bin_id)),
      drivetime_bin_id   = round(runif(n = n_subscribers, min = min_val_id, max = drivetime_bin_id)),
      drivetime_minutes  = rbimodal(n = n_subscribers, mean_1 = 180, sd_1 = 10, mean_2 = 60, sd_2 = 10)
    )
  }

  # Precompute starts/ends and map
  n_subscribers_vec          <- subscriber_signups$num_subscribers
  subscriber_num_start_vec   <- 1 + cumsum(lag(n_subscribers_vec, default = 0))
  subscriber_num_end_vec     <- subscriber_num_start_vec + n_subscribers_vec - 1

  output_data <- tibble(
    subscriber_num_start = subscriber_num_start_vec,
    subscriber_num_end   = subscriber_num_end_vec,
    date_of_signup   = subscriber_signups$date,
    n_subscribers        = n_subscribers_vec
  ) |>
    pmap_dfr(~ create_subscriber_panel(..1, ..2, ..3, n_subscribers = ..4))
}
