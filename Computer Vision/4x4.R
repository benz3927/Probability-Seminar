# Logarithmic cooling schedule
coolingschedule <- function(i,n)
{
  return(0.02/log(1+(i+1)/n))
}

# Gibbs Sampler
gibbs_sample <- function(x,p)
{
  curguess <- x
  hasisolatedpixel <- 0
  for (k in 2:3)
  {
    for (l in 2:3)
    {
      if (curguess[k,l] == 0 && sum(curguess[(k-1):(k+1),(l-1):(l+1)]) == 8)
      {
        hasisolatedpixel <- 1
      }
    }
  }
  niterations <- 1000
  pnoswitch <- (1-p)^(16) * onedist[sum(curguess)+1] * isolateddist[hasisolatedpixel + 1]
  for (i in 1:niterations)
  {
    randomcoords <- t(floor(runif(2,1,5)))
    proposal <- curguess
    proposal[randomcoords] <- (curguess[randomcoords] + 1) %% 2
    hasisolatedpixel <- 0
    for (k in 2:3)
    {
      for (l in 2:3)
      {
        if (proposal[k,l] == 0 && sum(proposal[(k-1):(k+1),(l-1):(l+1)]) == 8)
        {
          hasisolatedpixel <- 1
        }
      }
    }
    pswitch <- p^sum(proposal != messyimg) * (1-p)^sum(proposal == messyimg) *
      onedist[sum(proposal)+1]  * isolateddist[hasisolatedpixel + 1]
    if (runif(1) < 1 / (1 + (pnoswitch/pswitch) ^ (1/coolingschedule(i, niterations))))
    {
      curguess <- proposal
      pnoswitch <- pswitch
    }
  }
  return(curguess)
}

# Load THE CAT
library(png)
x <- readPNG("/Users/CS/Documents/GitHub/Probability-Seminar/Computer_Vision/cat.png")
dispcat <- x[,,1]

# Count for how often certain numbers of ones appear in 4x4 grids
onedist <- rep(0.01, 17)
isolateddist <- rep(0.01, 2)
for (i in 1:12)
{
  for (j in 1:12)
  {
    subimg <- x[(4*i-2):(4*i+1), (4*j-2):(4*j+1), 1]
    if ((i+j) %% 2 == 0)
    {
      dispcat[(4*i-2):(4*i+1), (4*j-2):(4*j+1)] <- dispcat[(4*i-2):(4*i+1), (4*j-2):(4*j+1)] + 2
    }
    onedist[sum(subimg)+1] <- onedist[sum(subimg)+1] + 1
    hasisolatedpixel <- 0
    for (k in 2:3)
    {
      for (l in 2:3)
      {
        if (subimg[k,l] == 0 && sum(subimg[(k-1):(k+1),(l-1):(l+1)]) == 8)
        {
          hasisolatedpixel <- 1
        }
      }
    }
    isolateddist[hasisolatedpixel + 1] <- isolateddist[hasisolatedpixel + 1] + 1
  }
}
# (normalize distribution)
onedist <- onedist / sum(onedist)
isolateddist <- isolateddist / sum(isolateddist)

# Visualizing 4 x 4 grids for THE CAT
image(dispcat)

# Specify your actual image here...
cleanimg <- matrix(1, 4, 4)

# Corrupt your image (alternatively, you can manually specify a messy image)
p <- 0.05
messyimg <- (cleanimg + matrix(rbinom(16,1,p),4,4)) %% 2

# Try to fix it!
fixedimg <- gibbs_sample(messyimg, p)


# Gibbs Sampler
image_generator <- function(x)
{
  curguess <- x
  hasisolatedpixel <- 0
  for (k in 2:3)
  {
    for (l in 2:3)
    {
      if (curguess[k,l] == 0 && sum(curguess[(k-1):(k+1),(l-1):(l+1)]) == 8)
      {
        hasisolatedpixel <- 1
      }
    }
  }
  niterations <- 1000
  pnoswitch <-  onedist[sum(curguess)+1] * isolateddist[hasisolatedpixel + 1]
  for (i in 1:niterations)
  {
    randomcoords <- t(floor(runif(2,1,5)))
    proposal <- curguess
    proposal[randomcoords] <- (curguess[randomcoords] + 1) %% 2
    hasisolatedpixel <- 0
    for (k in 2:3)
    {
      for (l in 2:3)
      {
        if (proposal[k,l] == 0 && sum(proposal[(k-1):(k+1),(l-1):(l+1)]) == 8)
        {
          hasisolatedpixel <- 1
        }
      }
    }
    pswitch <- onedist[sum(proposal)+1]  * isolateddist[hasisolatedpixel + 1]
    if (runif(1) < 1 / (1 + (pnoswitch/pswitch) ))
    {
      curguess <- proposal
      pnoswitch <- pswitch
    }
  }
  return(curguess)
}

random_starting_point <- matrix(rbinom(16,1,0.5),4,4)