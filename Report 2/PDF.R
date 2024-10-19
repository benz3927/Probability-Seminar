# Load necessary libraries
library(ggplot2)

# Define parameters
mu <- 0.24          # Drift
sigma <- 0.27       # Volatility
S_0 <- 416.06       # Initial value
t <- 1              # Time in years

# Create a sequence of x values for the PDF, starting from a small positive value
x <- seq(0.00001, 1500, length.out = 1000)  # Start from 0.01 to avoid log(0)

# Calculate the PDF of GBM
pdf_gbm <- (1 / (x * sigma * sqrt(2 * pi * t))) * 
  exp(-((log(x / S_0) - (mu - 0.5 * sigma^2) * t)^2) / (2 * sigma^2 * t))

# Create a data frame for ggplot
pdf_data <- data.frame(x = x, pdf = pdf_gbm)

# Filter out any rows with NaN or negative PDF values
pdf_data <- pdf_data[!is.na(pdf_data$pdf) & pdf_data$pdf >= 0, ]  # Remove missing or negative values

# Plot the PDF using ggplot2
ggplot(pdf_data, aes(x = x, y = pdf)) +
  geom_line(color = "lightblue", size = 1.2) +
  labs(title = expression(paste("Probability Density Function of ", X[1])),
       x = "Value",
       y = "Density") +
  theme_minimal() +
  xlim(0, max(x) * 1.1) +  # Extend x-axis limit by 10%
  ylim(0, max(pdf_data$pdf, na.rm = TRUE) * 1.1) +  # Extend y-axis limit by 10%
  theme(plot.title = element_text(hjust = 0.5))

