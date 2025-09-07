
# Function that creates an output file path based on the provided filename and whether it is a CSV or PNG file.
# Example: create_output_filepath("my_data", is_csv = TRUE) will return "data//my_data.csv"

create_output_filepath <- function(output_filename, is_csv = TRUE) {

  output_path_csv = 'data'
  output_path_png = 'output'

  # Add file extension based on is_csv flag
  if (is_csv) {
    # If CSV, use the CSV output path
    output_path = output_path_csv
    output_filepath = paste0(output_path, '//', output_filename, '.csv')
  } else {

    # If not CSV, assume PNG
    output_path = output_path_png
    output_filepath = paste0(output_path, '//', output_filename, '.png')
  }

  # Create the output directory if it doesn't exist
  dir.create(output_path, showWarnings = FALSE, recursive = TRUE)

  return(output_filepath)
}
