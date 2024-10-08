# Load the necessary libraries
library(scales)

# Read CSV from the URL
url <- "https://raw.githubusercontent.com/benz3927/Probability-Seminar/refs/heads/main/Report%202/msft.csv"
msft_data <- read.csv(url)

# Ensure the column names are correct
str(msft_data)

# Calculate log returns for estimating mu and sigma
log_returns <- rep(NA, length(msft_data$value))  # Initialize with NA values
log_returns[-1] <- diff(log(msft_data$value))  # Skip the first NA value

# Add log returns to the data frame
msft_data$log_returns <- log_returns

# Remove NA values for estimation
valid_log_returns <- na.omit(msft_data$log_returns)

# Estimate drift (mu) and volatility (sigma)
mu <- mean(valid_log_returns)
sigma <- sd(valid_log_returns)

cat("Estimated Drift (mu):", mu, "\n")
cat("Estimated Volatility (sigma):", sigma, "\n")

# Starting price
S0 <- 45.53

# Bounds
L <- 20
U <- 416.06

# Probability of hitting L before U
prob_hitting_L_before_U <- (log(S0/L) - mu) / (log(U/L))
cat("Probability of hitting $20 before $416.06:", prob_hitting_L_before_U, "\n")

# Current price
S_curr <- 416.06
S_lower <- 400

# Solve for x
x <- S_curr * exp(2 * mu / sigma^2 * log(S_curr / S_lower))
cat("Equal chance for MSFT to drop to $400 or rise to:", x, "\n")

# Time horizon
T <- 1  # One year

# Expected value
S_T_exp <- S_curr * exp(mu * T)
cat("Expected value of MSFT in one year:", S_T_exp, "\n")

# Simulate GBM over one year
set.seed(123)  # For reproducibility
nSims <- 1000  # Number of simulations
stepSize <- 1/252  # Trading days in a year
nSteps <- ceiling(T / stepSize)

# Simulate Geometric Brownian Motion
wieners <- matrix(S_curr, nSims, nSteps + 1)
for (i in 1:nSteps) {
  wieners[,i+1] <- wieners[,i] * exp((mu - 0.5 * sigma^2) * stepSize + sigma * sqrt(stepSize) * rnorm(nSims))
}

# Final values at one year
final_prices <- wieners[,nSteps + 1]

# Plot distribution of final prices
hist(final_prices, breaks = 50, col = "skyblue", main = "Distribution of MSFT Price in One Year", xlab = "Price")
