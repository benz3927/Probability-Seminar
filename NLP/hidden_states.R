# Matrix of transition probabilities for hidden states
p <- matrix(c(0.3, 0.5, 0.6, 0.4, 0.1, 0.1, 0.3, 0.4, 0.3), 3, 3)
hlabel <- c("N", "V", "A")
# Matrix of observation/emission probabilities (in columns)
q <- matrix(c(0.6, 0.2, 0.2, 0.1, 0.7, 0.2, 0.1, 0.4, 0.5), 3, 3)
olabel <- c("x", "y", "z")
# Initialize empty hidden (h) and observed (o) values
#  --> using numbers 1, 2, 3 instead of labels
n <- 100
h <- rep(0, n)
o <- rep(0, n)
h[1] <- 1
o[1] <- 1
# Left to right...
for (i in 2:n)
{
  # Randomly generate hidden state according to previous state
  h[i] <- which.max(cumsum(p[h[i-1],]) >= runif(1))
  
  # Randomly generate observed state according to current hidden state
  o[i] <- which.max(cumsum(q[,h[i]]) >= runif(1))
}
# To view our observed words...
olabel[o]