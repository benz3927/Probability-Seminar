# --- Setup for Libraries ---
library(scales)
set.seed(2024)

# --- Reading CSV Data ---
url <- "https://raw.githubusercontent.com/benz3927/Probability-Seminar/refs/heads/main/Report%202/msft.csv"
msft_data <- read.csv(url)

# Check the data loaded correctly
print(head(msft_data))  # Print the first few rows of the data

# Get the initial price and last price from the dataset
first_price <- head(msft_data[, 2], n = 1)
last_price <- tail(msft_data[, 2], n = 1)

# --- Question 1: Probability of hitting $20 before $416.06 ---
# Parameters
S_0 <- 45.53    # Initial price (starting price MSFT)
S_lower <- 20   # Lower boundary ($20)
S_upper <- 416.06  # Upper boundary ($416.06)
mu <- 0.253      # Drift
sigma <- 0.272   # Volatility

# Compute a and b for the problem (relative distances)
a <- log(S_upper / S_0)  
b <- log(S_lower / S_0)

# Compute the probability of hitting S_upper before S_lower
prob_hit_upper_before_lower  <- (1 - exp(- (2 * mu * b) / sigma^2)) / 
  (exp(- (2 * mu * a) / sigma^2) - exp(- (2 * mu * b) / sigma^2))

# Therefore, probability of hitting lower = 1 - p:
prob_hit_lower_before_upper <- 1 - prob_hit_upper_before_lower
# Print the result for Question 1
cat("Probability of hitting $20 before $416.06:", prob_hit_lower_before_upper, "\n")

# --- Question 2: Equal chance of dropping to $400 or rising to x ---
# Parameters
S_current <- 416.06  # Current price
S_lower <- 400       # Lower boundary ($400)

# Compute the relative log distance for the lower boundary
b <- log(S_lower / S_current)  # Log distance for the lower boundary

# Solve for a using the correct derived formula
a <- (-sigma^2 / (2 * mu)) * log(2 - exp(-(2 * mu * b) / sigma^2))

# Solve for x using the value of a
x_upper <- S_current * exp(a)

# Print the result for the upper bound
cat("Value of x where there's an equal chance of dropping to $400 or rising to x:", x_upper, "\n")

# --- Question 3: Distribution of Prices in One Year ---

# Given values for Question 3
S_0 <- 416.06  # Current price of MSFT
mu <- 0.261     # Drift
sigma <- 0.272   # Volatility
t <- 1         # Time in years

# Calculate expected value
expected_value <- S_0 * exp(mu * t)

# Print the expected value result
cat(sprintf("For S_0 = %.2f, mu = %.2f, sigma = %.2f, t = %.2f:\n", S_0, mu, sigma, t))
cat(sprintf("Expected Value E[X_t] = %.2f\n", expected_value))

# Calculate variance
variance <- (S_0^2) * exp(2 * mu * t) * (exp(t * sigma^2) - 1)

# Calculate standard deviation
std_dev <- sqrt(variance)

# Print the variance and standard deviation results
cat(sprintf("Variance Var[S_t] = %.2f\n", variance))
cat(sprintf("Standard Deviation SD[S_t] = %.2f\n", std_dev))

