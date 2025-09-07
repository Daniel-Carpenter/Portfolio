# T Test function

autoTTest <- function(x, y, alpha = 0.05, paired = FALSE) {
  
  # Check if x and y are numeric vectors
  if (!is.numeric(x) || !is.numeric(y)) {
    stop("x and y must be numeric vectors.")
  }
  
  
  # Check the variances of x and y and determine which type of t-test to perform
  variancesAreNotEqual <- function(x, y, alpha) {
    
    # Check the variances of x and y using an F-test
    fTest <- var.test(x, y)
    
    # Return TRUE if the p-value is less than alpha, indicating unequal variances
    return(fTest$p.value < alpha)
  }
  
  # Perform a two-sample t-test on x and y
  performTTest <- function(x, y, alpha, paired) {
    
    # 0. Check if variances equal or not
    variancesNotEqual <- variancesAreNotEqual(x, y, alpha)
    
    # 1. Discover which t test to use
    
    ## 1.1 If paired in paraeters, then paired
    if (paired) {
      testType     <- 'Paired'
      tTestResults <- t.test(x, y, paired = TRUE, conf.level = 1 - alpha)
      
      ## 1.2 Else If variances are not equal then use Welch's T-Test
    } else if (variancesNotEqual) {
      testType     <- 'Welch'
      tTestResults <- t.test(x, y, var.equal = FALSE, conf.level = 1 - alpha)
      
      ## 1.3 Else variances are equal and not using a paired ttest, so use standard
    } else {
      testType     <- 'T-test'
      tTestResults <- t.test(x, y, var.equal = TRUE, conf.level = 1 - alpha)
    }
    
    # Determine if the null hypothesis is rejected based on the p-value
    rejectNull <- tTestResults$p.value < alpha
    
    # Confidence interval and p value
    ci     = tTestResults[['conf.int']]
    pValue = tTestResults[['p.value']]
    
    # Return the test type (name), t-test output, and if we rejected the null
    return(list(ci           = ci,
                pValue       = pValue,
                testType     = testType,
                tTestResults = tTestResults,
                rejectNull   = rejectNull
    )
    )
  }
  
  # Call the performTTest function and store the results in testResult
  testResult <- performTTest(x, y, alpha, paired)
  
  # Create a results list of the test results and input data
  result <- list(
    # data         = data.frame(x = x, y = y),
    # alpha        = alpha,
    ci           = testResult$ci,
    pValue       = testResult$pValue,
    testType     = testResult$testType,
    summaryStats = testResult$tTestResults,
    rejectNull   = testResult$rejectNull
  )
  
  # Return the result object invisibly
  return(result)
}