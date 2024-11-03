# Load necessary library for plotting
library(ggplot2)

# Parameters
nIterations <- 10000

# Generate random points
x <- runif(nIterations, -0.5, 0.5)
y <- runif(nIterations, -0.5, 0.5)

# Determine which points are inside the circle
inCircle <- (x^2 + y^2) < 0.25

# Create a data frame to store the points and their classification
points_data <- data.frame(x = x, y = y, inCircle = inCircle)

# Plot the points
ggplot(points_data, aes(x = x, y = y, color = inCircle)) +
  geom_point(alpha = 0.5, size = 0.5, show.legend = FALSE) +  # Plot all points, no legend
  scale_color_manual(values = c("red", "lightgreen")) +  # Red for outside, green for inside
  coord_fixed() +  # Ensure the aspect ratio is 1:1
  theme_minimal() +
  labs(title = expression("Monte Carlo Approximation of " * pi),
       x = "x",
       y = "y") +
  theme(plot.title = element_text(hjust = 0.5)) +
  # Add the circle outline
  annotate("path",
           x = 0.5 * cos(seq(0, 2 * pi, length.out = 100)),
           y = 0.5 * sin(seq(0, 2 * pi, length.out = 100)),
           color = "lightblue", size = 1) +
  # Add the square outline
  geom_rect(aes(xmin = -0.5, xmax = 0.5, ymin = -0.5, ymax = 0.5),
            fill = NA, color = "darkgrey", size = 1)
