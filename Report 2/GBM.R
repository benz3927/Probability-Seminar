library(scales)

# Read CSV data from URL
url <- "https://raw.githubusercontent.com/benz3927/Probability-Seminar/refs/heads/main/Report%202/msft.csv"
msft_data <- read.csv(url)

# Convert 'date' to a numeric sequence representing time in years (0 to 10 years)
msft_data$time_numeric <- seq(0, 10, length.out = nrow(msft_data))

# Set seed
set.seed(2024)

# --- Setup for Grid Search ---
mu_candidates <- seq(0.01, 0.5, by = 0.001)  # Candidate values for mu
best_mu <- NA  # Variable to store the best mu
min_diff <- Inf  # Variable to store the minimum difference

# Calculate log returns
log_returns <- diff(log(msft_data$value))  

# Annualized volatility in 252 trading days
sigma <- sd(log_returns) * sqrt(252)  

# --- Adjust time_numeric for log returns (n - 1) ---
time_numeric_log <- msft_data$time_numeric[-1]

# --- Create a marplot of log returns ---
plot(time_numeric_log, log_returns, type = "h", col = "lightblue", 
     xlab = "Time (Years)", ylab = "Log Returns", main = "MSFT Log Returns",
     lwd = 1)

# Add a horizontal line for reference
abline(h = 0, col = "black", lty = 2)



# --- Simulation Parameters ---
nSims <- 100  # Number of simulations
stepSize <- 0.01  # Simulation time step, daily 1 
totalTime <- 10  # Length of time to simulate, daily 2516
nSteps <- ceiling(totalTime / stepSize)
startPrice <- 45.53  # Starting value

# --- Perform Grid Search ---
for (mu in mu_candidates) {
  # Initialize matrix for Brownian motions
  wieners <- matrix(startPrice, nSims, nSteps + 1)
  
  # Generate Geometric Brownian motions for each candidate mu
  for (i in 1:nSteps) {
    wieners[,i+1] <- wieners[,i] * (1 + mu * stepSize + sigma * rnorm(nSims, 0, sqrt(stepSize)))
  }
  
  # Find the median GBM price at the final time step
  final_gbm_price <- median(wieners[, nSteps + 1])
  
  # Compute the absolute difference between the last MSFT price and final GBM price
  diff <- abs(last_price - final_gbm_price)
  
  # Update best_mu if this mu gives a smaller difference
  if (diff < min_diff) {
    min_diff <- diff
    best_mu <- mu
  }
}

# --- Output the best mu and corresponding minimum difference ---
cat("Best mu:", best_mu, "\n")
cat("Minimum difference between MSFT and GBM final price:", min_diff, "\n")

# Parameters
nSims <- 100 # Number of Geometric Brownian motions
stepSize <- 0.01 # Simulation time step
totalTime <- 10 # Length of time to simulate
upperLimit <- 450 # Upper hitting bound
lowerLimit <- 20 # Lower hitting bound
volatility <- 0.2 # Volatility sigma, found from online and guess and check
drift <- best_mu # Drift mu
startPrice <- 45.53 # Starting value
nSteps <- ceiling(totalTime / stepSize)

# Initialize matrix for Brownian motions and vectors for hit times/values
wieners <- matrix(startPrice, nSims, nSteps + 1)
hitStep <- rep(0, nSims)
hitValue <- rep(0, nSims)

# Generate Geometric Brownian motions
for (i in 1:nSteps) {
  wieners[,i+1] <- wieners[,i] * (1 + drift * stepSize +
                                    volatility * rnorm(nSims, 0, sqrt(stepSize)))
  hitValue <- hitValue +
    (hitValue == 0) * (wieners[,i+1] > upperLimit) * upperLimit +
    (hitValue == 0) * (wieners[,i+1] < lowerLimit) * lowerLimit
  hitStep <- hitStep +
    (hitStep == 0) * (hitValue != 0) * i
}

# Plot all of the Geometric Brownian Motions
colorArray <- rep("black", nSims)
colorArray[hitValue == upperLimit] <- "blue"
colorArray[hitValue == lowerLimit] <- "red"
matplot(seq(0, totalTime, stepSize), t(wieners), type = "l", lty = 1, 
        col = alpha(colorArray, 0.2), xlab = "Time", 
        ylab = "Price", main = "Geometric Brownian Motion vs Actual Data")
abline(upperLimit, 0, col = "black", lty = 2)
abline(lowerLimit, 0, col = "black", lty = 2)

# Plot actual MSFT data on top
lines(msft_data$time_numeric, msft_data$value, col = "blue", lwd = 2)

# --- Print the Best Mu and Sigma ---
cat("Best Mu:", best_mu, "\n")
cat("Sigma (Annualized Volatility):", sigma, "\n")

# Calculate and plot the median price at time t = 10
median_price_at_t_10 <- median(wieners[, nSteps + 1])
abline(h = median_price_at_t_10, col = "darkgreen", lwd = 2, lty = 1)

# Print the median price
print(median_price_at_t_10)
