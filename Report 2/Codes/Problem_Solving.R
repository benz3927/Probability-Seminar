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
mu_gbm <- 0.25856      # GBM Drift
sigma <- 0.272   # Volatility
mu_bm <- mu_gbm - sigma^2/2 # Mu of BM with Drift converted from GBM Drift
# Using Ito's Lemma

# Compute a and b for the problem (relative distances)
# So that we can preserve a>0, b<0, S(0) -> 0
a <- log(S_upper) - log(S_0)  
b <- log(S_lower) - log(S_0)

# Compute the probability of hitting S_upper before S_lower
prob_hit_upper_before_lower  <- (1 - exp(- (2 * mu_bm * b) / sigma^2)) / 
  (exp(- (2 * mu_bm * a) / sigma^2) - exp(- (2 * mu_bm * b) / sigma^2))

# Therefore, probability of hitting lower = 1 - p:
prob_hit_lower_before_upper <- 1 - prob_hit_upper_before_lower
# Print the result for Question 1
cat("Probability of hitting $20 before $416.06:", prob_hit_lower_before_upper, "\n")

# --- Question 2: Equal chance of dropping to $400 or rising to x ---
# Parameters
S_current <- 416.06  # Current price
S_lower <- 400       # Lower boundary ($400)

# Compute the relative log distance for the lower boundary
b <- log(S_lower) - log(S_current)  # Log distance for the lower boundary

# Solve for a using the correct derived formula
a <- (-sigma^2 / (2 * mu_bm)) * log(2 - exp(-(2 * mu_bm * b) / sigma^2))

# Solve for x using the value of a
x_upper <- S_current * exp(a)

# Print the result for the upper bound
cat("Value of x where there's an equal chance of dropping to $400 or rising to x:", x_upper, "\n")

# --- Question 3: Distribution of Prices in One Year ---

# Given values for Question 3
S_0 <- 416.06  # Current price of MSFT
mu_gbm <- mu_gbm
sigma <- 0.272   # Volatility
t <- 1         # Time in years

# Calculate expected value
expected_value <- S_0 * exp(mu_gbm * t)

# Print the expected value result
cat(sprintf("For S_0 = %.3f, mu = %.3f, sigma = %.3f, t = %.3f:\n", S_0, mu_gbm, sigma, t))
cat(sprintf("Expected Value E[X_t] = %.2f\n", expected_value))
