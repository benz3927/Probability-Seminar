# Load ggplot
library(ggplot2)

# Define the function c(x) = -x^2 + 3x
c_x <- function(x) {
  return(-x^2 + 3 * x)
}

# Generate x values
x_values <- seq(0, 3, length.out = 400)

# Generate c(x) values
y_values <- c_x(x_values)

# Create a data frame for plotting
data <- data.frame(x = x_values, c_x = y_values)

# Plot the function
ggplot(data, aes(x = x, y = c_x)) +
  geom_line(color = 'blue') +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  geom_vline(xintercept = 0, color = "black", linewidth = 0.5) +
  ggtitle(expression("Graph of " * c(x) == -x^2 + 3 * x)) +
  xlab("x") +
  ylab(expression(c(x))) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5)) +
  ylim(-.5, 2.5)
