library(scales)

# Read CSV data from URL (MSFT stock prices from 2014-2024)
url <- "https://raw.githubusercontent.com/benz3927/Probability-Seminar/refs/heads/main/Report%202/msft.csv"
msft_data <- read.csv(url)

# Convert 'date' to a numeric sequence representing time in days
nRows <- nrow(msft_data)  # Define nRows before using it
msft_data$time_numeric <- seq(1, nRows)  # Days from 1 to number of rows

# Set seed for reproducibility
set.seed(2024)

# --- Plot MSFT Data (2014-2024) ---
plot(msft_data$time_numeric, msft_data$value, type = "l", col = rgb(0, 0, 0.5, 0.8), lwd = 1.5,
     xlab = "Time (Days)", ylab = "Price ($)", main = "MSFT Price GBM Projections (2014-2026)",
     xlim = c(1, nRows + 2 * 252), ylim = c(0, max(msft_data$value) * 1.5))

# Add vertical line at the end of actual data
abline(v = nRows, col = "black", lty = 2)

# Parameters for Simulation
S_0 <- tail(msft_data$value, 1)   # Starting price at end of actual data
mu <- 0.258                       # Drift
sigma <- 0.272                    # Estimated volatility
nSims <- 100                      # Number of GBM simulations
stepSize <- 1 / 252               # Daily time step (252 trading days in a year)
totalTime <- 2                    # Projecting 2 years into the future
nSteps <- ceiling(totalTime * 252)  # Total steps for simulation (2 years in days)

# Initialize matrix for Brownian motions
wieners <- matrix(S_0, nSims, nSteps + 1)

# Generate GBM paths for each simulation
for (i in 1:nSteps) {
  wieners[, i + 1] <- wieners[, i] * exp((mu - 0.5 * sigma^2) * stepSize + sigma * sqrt(stepSize) * rnorm(nSims))
}

# --- Calculate Paths ---
# Bullish and Bearish Paths (75% and 25% quantiles) and Median Path
bullish_path <- apply(wieners, 2, quantile, probs = 0.75)
bearish_path <- apply(wieners, 2, quantile, probs = 0.25)
median_path <- apply(wieners, 2, median)

# Time scale for simulated data (2 years ahead in days)
sim_time <- seq(nRows, nRows + nSteps, length.out = nSteps + 1)

# --- Shaded Region for Projections (Confidence Interval) ---
polygon(c(sim_time, rev(sim_time)), 
        c(bullish_path, rev(bearish_path)), 
        col = alpha("lightblue", 0.4), border = NA)

# Plot bullish, bearish, and median paths (add lines to the existing plot)
lines(sim_time, bullish_path, col = rgb(0, 0.5, 0, 0.7), lwd = 1)   # Dark green
lines(sim_time, bearish_path, col = rgb(0.9, 0, 0, 0.7), lwd = 1)  # Dark red
lines(sim_time, median_path, col = rgb(0, 0, 0.8, 0.7), lwd = 1)   # Muted blue

# Add legend
legend("topleft", legend = c("MSFT Actual", "Bullish (75%)", "Bearish (25%)", "Median Path"),
       col = c(rgb(0, 0, 0.5, 0.8), rgb(0, 0.5, 0, 0.7), rgb(0.9, 0, 0, 0.7), rgb(0, 0, 0.8, 0.7)), 
       lty = c(1, 1, 1, 1), lwd = 1)