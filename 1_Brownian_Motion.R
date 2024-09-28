library(ggplot2)

T <- 100  
n_steps <- 200  
n_paths <- 1  
dt <- T / n_steps  

generate_brownian <- function(T, n_steps, n_paths) {
  paths <- matrix(0, nrow = n_steps + 1, ncol = n_paths)
  
  for (i in 2:(n_steps + 1)) {
    # Generate increments and create the Brownian path
    paths[i, ] <- paths[i - 1, ] + rnorm(n_paths, mean = 0, sd = sqrt(dt))
  }
  
  # Create a data frame for plotting
  t <- seq(0, T, length.out = n_steps + 1)
  paths_df <- data.frame(t = rep(t, times = n_paths),
                         value = as.vector(paths),
                         path = factor(rep(1:n_paths, each = n_steps + 1)))
  
  return(paths_df)
}

# Generate Brownian motion paths
paths_df <- generate_brownian(T = T, n_steps = n_steps, n_paths = n_paths)

# Define the standard deviation sqrt(t) for the plot
t <- seq(0, T, length.out = n_steps + 1)
upper_bound <- sqrt(t)
lower_bound <- -sqrt(t)

bounds_df <- data.frame(t = t, upper_bound = upper_bound, lower_bound = lower_bound)

# Set y-axis limits for better visualization
y_min <- min(c(lower_bound, min(paths_df$value))) - 5
y_max <- max(c(upper_bound, max(paths_df$value))) + 5

# Plot the Brownian motion path and sqrt(t)
ggplot() + 
  geom_line(data = paths_df, aes(x = t, y = value), size = 1, color = "darkblue") +
  geom_line(data = bounds_df, aes(x = t, y = upper_bound), color = "orange", linetype = "dashed") +
  geom_line(data = bounds_df, aes(x = t, y = lower_bound), color = "orange", linetype = "dashed") +
  labs(title = "A Continuous Random Walk!", x = "Time", y = "W(t)") +
  ylim(y_min, y_max) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "none") +
  annotate("text", x = 80, y = y_max - 5, label = "sqrt(t)", color = "black", size = 5) +
  annotate("text", x = 80, y = y_min + 5, label = "-sqrt(t)", color = "black", size = 5)
