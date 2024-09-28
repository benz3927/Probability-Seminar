library(ggplot2)
library(dplyr)

simulate_multiple_brownian_motions <- function(T, n, num_paths) {
  all_paths <- data.frame()  # Initialize an empty data frame to store all paths
  
  for (i in 1:num_paths) {
    increments <- rnorm(n, mean = 0, sd = sqrt(T / n))  # Generate random increments
    W <- cumsum(c(0, increments))  # Create the Brownian motion path by cumulative sum
    time <- seq(0, T, length.out = n + 1)  # Create a time vector
    path_data <- data.frame(time = time, W = W, path = factor(i))  # Store time, W, and path ID
    all_paths <- rbind(all_paths, path_data)  # Combine paths into a single data frame
  }
  
  return(all_paths)  # Return the complete data frame of paths
}

T <- 1.0  
n <- 100  
num_paths <- 5  

# Simulate multiple Brownian motion paths
brownian_data <- simulate_multiple_brownian_motions(T, n, num_paths)

# Assign colors based on the final value of each path
colors <- sapply(1:num_paths, function(i) {
  final_value <- brownian_data %>%
    filter(path == i) %>%
    summarize(final_W = last(W)) %>%
    pull(final_W)  # Get the last value of the path
  
  # Choose color based on whether the final value is positive or negative
  if (final_value > 0) {
    return(rgb(0.6, 1, 0.6, alpha = 0.9))  # Light green for paths ending above 0
  } else {
    return(rgb(1, 0.6, 0.6, alpha = 0.9))  # Light red for paths ending below 0
  }
})

# Plot the Brownian motion paths with conditional colors
ggplot(brownian_data, aes(x = time, y = W, color = path)) +
  geom_line(size = 1) +  # Draw lines for the paths
  geom_point(size = 0.5) +  # Add points to the paths
  scale_color_manual(values = colors) +  # Set colors for the paths
  theme_minimal() +
  labs(title = "Brownian Motion Simulation Paths", x = "Time", y = "W(t)") +
  theme(text = element_text(size = 14),
        plot.title = element_text(hjust = 0.5),
        panel.grid.major = element_line(color = "lightgrey"),
        panel.grid.minor = element_blank(),
        legend.position = "none") +  # Remove the legend
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5)  # Add horizontal line at W=0
