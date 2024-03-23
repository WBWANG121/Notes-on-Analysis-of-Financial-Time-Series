library(quantmod)
library(moments)

price_analysis <- function(symbol, start_date, end_date) {
  print(paste("Results for stock ticker", symbol, "."))
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  # Load data using getSymbols from quantmod
  data <- quantmod::getSymbols(symbol, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
  
  # Extract Close prices into a separate vector
  close_prices <- as.numeric(data[, paste0(symbol, ".Close")])
  
  # Calculate shifted Close prices and daily returns
  shift_by_1 <- c(NA, close_prices[-length(close_prices)])
  returns <- (close_prices - shift_by_1) / shift_by_1
  
  # Calculate log returns
  logReturns <- log(close_prices / shift_by_1)
  
  # Create a data frame to store results
  results <- data.frame(Date = index(data), Close = close_prices, Shifted_Close = shift_by_1, Returns = returns, LogReturns = logReturns)
  
  # Print data type of each column in the results data frame
  column_types <- sapply(results, class)
  print(column_types)
  
  # Calculate and print statistics for returns excluding the first element
  print("Simple Returns:")
  print(paste("Mean:", mean(returns[-1], na.rm = TRUE), 
              "Standard Deviation:", sd(returns[-1], na.rm = TRUE), 
              "Minimum:", min(returns[-1], na.rm = TRUE), 
              "Maximum:", max(returns[-1], na.rm = TRUE), 
              "Skewness:", skewness(returns[-1], na.rm = TRUE), 
              "Excess Kurtosis:", kurtosis(returns[-1], na.rm = TRUE) - 3))

  print("Log Returns:")
  print(paste("Mean:", mean(logReturns[-1], na.rm = TRUE), 
              "Standard Deviation:", sd(logReturns[-1], na.rm = TRUE), 
              "Minimum:", min(logReturns[-1], na.rm = TRUE), 
              "Maximum:", max(logReturns[-1], na.rm = TRUE), 
              "Skewness:", skewness(logReturns[-1], na.rm = TRUE), 
              "Excess Kurtosis:", kurtosis(logReturns[-1], na.rm = TRUE) - 3))
  print("Hypothesis testing:")
  print("H0: mu = 0 for log returns")
  print("Ha: mu != 0")
  t_test <- t.test(logReturns, mu = 0)
  print(t_test)
  
  test_statistic <- t_test$statistic
  degrees_freedom <- t_test$parameter
  alpha <- 0.05
  critical_value <- qt(1 - alpha/2, df = degrees_freedom)
  if (abs(test_statistic) > critical_value) {
    print("Reject the null hypothesis")
  } else {
    print("Fail to reject the null hypothesis")
  }
}

symbol <- "AXP"
start_date <- "2010-01-01"
end_date <- "2024-03-23"
price_analysis(symbol, start_date, end_date)

symbol <- "CAT"
price_analysis(symbol, start_date, end_date)

symbol <- "SBUX"
price_analysis(symbol, start_date, end_date)
