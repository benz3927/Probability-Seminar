library(scales) # Needed only for alpha in plot

# Read CSV data from URL
url <- "https://raw.githubusercontent.com/benz3927/Probability-Seminar/refs/heads/main/Report%202/msft.csv"
msft_data <- read.csv(url)

# Convert 'date' to a numeric sequence representing time in years (0 to 10 years)
msft_data$time_numeric <- seq(0, 10, length.out = nrow(msft_data))

# Set seed for reproducibility
set.seed(5)

# Parameters
nSims <- 100 # Number of Geometric Brownian motions
stepSize <- 0.01 # Simulation time step
totalTime <- 10 # Length of time to simulate
upperLimit <- 200 # Upper hitting bound
lowerLimit <- 50 # Lower hitting bound
volatility <- 0.24 # Volatility sigma
drift <- 0.24 # Drift mu
startPrice <- 45.53 # Starting value
nSteps <- ceiling(totalTime / stepSize)

# Initialize matrix for Brownian motions and vectors for hit times/values
wieners <- matrix(startPrice, nSims, nSteps + 1)
hitStep <- rep(0, nSims)
hitValue <- rep(0, nSims)

# Generate Geometric Brownian motions
for (i in 1:nSteps) {
  wieners[,i+1] <- wieners[,i] * (1 + drift * stepSize +
                                    volatility * rnorm(nSims, 0, sqrt(stepSize)))
  hitValue <- hitValue +
    (hitValue == 0) * (wieners[,i+1] > upperLimit) * upperLimit +
    (hitValue == 0) * (wieners[,i+1] < lowerLimit) * lowerLimit
  hitStep <- hitStep +
    (hitStep == 0) * (hitValue != 0) * i
}

# Plot all of the Geometric Brownian Motions
colorArray <- rep("black", nSims)
colorArray[hitValue == upperLimit] <- "blue"
colorArray[hitValue == lowerLimit] <- "red"
matplot(seq(0, totalTime, stepSize), t(wieners), type = "l", lty = 1, 
        col = alpha(colorArray, 0.2), xlab = "Time", 
        ylab = "Price", main = "Geometric Brownian Motion vs Actual Data")
abline(upperLimit, 0, col = "black", lty = 2)
abline(lowerLimit, 0, col = "black", lty = 2)

# Plot actual MSFT data on top
lines(msft_data$time_numeric, msft_data$value, col = "blue", lwd = 2)

# Calculate and plot the mean price at time t = 10
mean_price_at_t_10 <- mean(wieners[, nSteps + 1])
abline(h = mean_price_at_t_10, col = "darkgreen", lwd = 2, lty = 1)

# Print the mean price
print(mean_price_at_t_10)
