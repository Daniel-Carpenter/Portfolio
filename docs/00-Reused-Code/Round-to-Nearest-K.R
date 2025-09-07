library(purrr)

# Function to produce rounding functions
createRoundingFunction <- function(roundingFunctionName) {

  # Create the dynamic rounding function
  roundingFunction <- function(num, k) {
    
    roundedNum <- map(num/k, roundingFunctionName)[[1]] * k
    return(roundedNum)
  }
  
}

# Create rounding functions
roundUpToK   <- createRoundingFunction(ceiling) # Round up to nearest k
roundDownToK <- createRoundingFunction(floor)   # Round down to nearest k

rm(createRoundingFunction)

# Examples
# roundUpToK(11, 5)
# roundDownToK(11, 5)
