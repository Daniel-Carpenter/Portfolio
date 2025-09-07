library(stringr)

# Function to convert column of snake case to Proper with spaces 
cleanTitles <- function(title, nCharWrap) {
  str_wrap(str_to_title(str_replace_all(title, '_', ' ')), nCharWrap)
}
