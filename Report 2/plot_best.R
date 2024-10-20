library(scales)
library(parallel)

# Set seed for overall script
set.seed(2024)

# Read CSV data from URL
url <- "https://raw.githubusercontent.com/benz3927/Probability-Seminar/refs/heads/main/Report%202/msft.csv"
msft_data <- read.csv(url)

# Ensure there are no NA values in the data
msft_data <- na.omit(msft_data)

# Convert 'date' to a numeric sequence representing time in days (1 to 2516)
nRows <- nrow(msft_data)
msft_data$time_numeric <- seq(1, nRows)

# Calculate log returns
log_returns <- diff(log(msft_data$value))  

# Annualized volatility in 252 trading days
sigma <- sd(log_returns) * sqrt(252)  

# --- Setup for Grid Search ---
mu_candidates <- seq(0.001, 1, by = 0.001)  # Candidate values for mu
median_last_20_prices <- median(tail(msft_data$value, n = 20))  # Median of last 20 prices

# --- Simulation Parameters ---
nSims <- 100  # Number of simulations
nSteps <- 2516  # Total number of steps for 2516 days
startPrice <- msft_data$value[1]  # Starting value (first price in dataset)

# Function for running GBM simulations
run_simulation <- function(mu) {
  set.seed(2024)  # Set the seed inside the function
  wieners <- matrix(0, nSims, nSteps + 1)  # Initialize to zeros
  wieners[, 1] <- startPrice  # Set the starting price
  
  for (i in 1:nSteps) {
    wieners[, i + 1] <- wieners[, i] * (1 + mu / 252 + sigma * rnorm(nSims, 0, 1 / sqrt(252)))
  }
  
  final_gbm_price <- median(wieners[, nSteps + 1])
  diff <- abs(median_last_20_prices - final_gbm_price)
  
  return(c(mu, diff))  # Return mu and difference
}

# Set up parallel computing
cl <- makeCluster(detectCores() - 1)  # Use all but one core
clusterExport(cl, varlist = c("nSims", "nSteps", "startPrice", "sigma", "median_last_20_prices"))

# Run simulations in parallel
results <- parSapply(cl, mu_candidates, run_simulation)

# Stop the cluster
stopCluster(cl)

# Find the best mu and minimum difference
best_mu_index <- which.min(results[2, ])
best_mu <- results[1, best_mu_index]
min_diff <- results[2, best_mu_index]

# --- Output the best mu and corresponding minimum difference ---
cat("Best mu:", best_mu, "\n")

# Initialize matrix for Brownian motions
wieners <- matrix(0, nSims, nSteps + 1)  # +1 for initial price
wieners[, 1] <- startPrice  # Set starting price

# Generate Geometric Brownian motions with the best mu
for (i in 1:nSteps) {
  wieners[, i + 1] <- wieners[, i] * (1 + best_mu / 252 + sigma * rnorm(nSims, 0, 1 / sqrt(252)))
}

# Plot all of the Geometric Brownian Motions with light blue color
colorArray <- "lightblue"  # Set color to light blue

# Use seq(1, nSteps + 1) to match the number of simulation steps
matplot(seq(1, nSteps + 1), t(wieners), type = "l", lty = 1, 
        col = alpha(colorArray, 0.2), xlab = "Time (Days)", 
        ylab = "Price ($)", main = "GBM vs MSFT Actual Price (2014-2024)")

# Plot actual MSFT data on top with brighter color and thicker line
lines(msft_data$time_numeric, msft_data$value, col = "blue", lwd = 3)

# Calculate and plot the median price at the final time step
median_price_at_t_final <- median(wieners[, nSteps + 1])
abline(h = median_price_at_t_final, col = "darkgreen", lwd = 2, lty = 1)

# Add a legend
legend("topright", legend = c("Actual MSFT Data", "GBM Median Price"),
       col = c("blue", "darkgreen"), lwd = c(3, 2), lty = c(1, 1),
       bty = "n")

# Print the median price and compare with the last 20 actual prices
cat("Median of Last 20 MSFT Prices:", median_last_20_prices, "\n")
cat("Median Price at t = Final from GBM:", median_price_at_t_final, "\n")

# Print the Best Mu and Sigma
cat("Best Mu:", best_mu, "\n")
cat("Sigma (Annualized Volatility):", sigma, "\n")
