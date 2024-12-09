# Load necessary libraries
library(ggplot2)

# Define parameters
mu <- 0.2586        # Drift
sigma <- 0.272      # Volatility
S_0 <- 416.06       # Initial value

# Create a sequence of x values for the CDF
x <- seq(0.01, 2000, length.out = 1000)  # Start from 0.01 to avoid log(0)

# Calculate the CDF of GBM for t = 1 year
t <- 1  # Time in years
cdf_gbm_t1 <- pnorm(log(x / S_0), mean = (mu - 0.5 * sigma^2) * t, sd = sigma * sqrt(t))

# Create a data frame for ggplot for t = 1
cdf_data_t1 <- data.frame(x = x, cdf = cdf_gbm_t1)

# Plot the CDF for t = 1 using ggplot2
ggplot(cdf_data_t1, aes(x = x, y = cdf)) +
  geom_line(color = "lightblue", size = 1.2) +
  labs(x = "Stock Price ($)",
       y = expression(F[S](s))) +
  theme_minimal() +
  xlim(0, max(x) * 1.1) +  # Extend x-axis limit by 10%
  ylim(0, 1) +  # CDF should be between 0 and 1
  theme(plot.title = element_text(hjust = 0.5))

