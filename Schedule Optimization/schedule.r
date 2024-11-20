##----------------------------------------------------------------------
## EVERYTHING YOU NEED TO CHANGE IS IN THIS SECTION OR THE FINAL SECTION

# Number of doctors available
ndoctors <- 10

# Number of nurses
nnurses <- 15

# Number of days in the schedule
ndays <- 7

# Number of staff present for each shift (must have same # of shifts)
# e.g., c(2, 3, 3) has three shifts with 2, 3, 3 people respectively
doctorstaffing <- c(2, 2, 2)
nursestaffing <- c(3, 3, 3)

# Penalties for violating each of six constraints
# Penalty 1: Each instance of three consecutive shifts (including overnight)
# Penalty 2: Each person who does not have two days off AT ALL
# Penalty 3: Each person who does not have two days off IN A ROW
# Penalty 4: Each instance of first shift following last shift on prev day
# Penalty 5: Each instance of working both weekend days (days 1, 2)
# Penalty 6: Weights for the stdev of nurses'/doctors' shifts in the schedule
#            (trying to make everyone work similar amounts)
penalties <- c(100, 100, 0.5, 5, 1, 2)

# Number of iterations (if too small, might not be able to explore enough)
niter <- 50000

# The cooling schedule: what should the temperature be at step i out of n
coolingschedule <- function(i,n)
{
  # If you want greedy optimization, just fix some low temperature
  #return(1e-3)
  
  # Linear cooling schedule from 1 down to 0
  return(1 - (i/(n+1)))
  
  # Logarithmic cooling schedule (goes down really slowly,
  # choose constant to make sure the temp gets low enough)
  #return(0.02/log(1+(i+1)/n))
}

## END OF PARAMETER SECTION
##----------------------------------------------------------------------

# (Some things which are calculated automatically)
doctorindices <- c(1,cumsum(doctorstaffing)+1)
nurseindices <- c(1,cumsum(nursestaffing)+1)
nshifts <- length(doctorstaffing)

# Function to calculate the "energy" (total cost of violations)
get_energy <- function(doctorsunwrapped, nursesunwrapped, penalties)
{
  return(get_energy_sub(doctorsunwrapped,penalties)+
           get_energy_sub(nursesunwrapped,penalties))
}

# ...calculates the energy contribution from doctors & nurses separately
get_energy_sub <- function(subsched, penalties)
{
  energy <- 0
  
  # Array to keep track of total number of shifts worked by each worker
  totshift <- rep(0, dim(subsched)[1])
  
  # Go through the schedule worker by worker...
  for (i in 1:dim(subsched)[1])
  {
    # Count number of shifts for that worker
    totshift[i] <- sum(subsched[i,,])
    
    # Look at whether worker works the last shift on the last day
    lastshift <- subsched[i,ndays,nshifts]
    
    # Look at whether worker has the last day off
    prevoff <- all(subsched[i,ndays,] == FALSE)
    
    daysoff <- 0
    twoinarow <- 1
    
    # If worked the very last shift, did you work the shift before as well?
    #  (looking to capture three shifts in a row wrapping around week to week)
    if (lastshift == 1)
    {
      lastshift <- lastshift + subsched[i,ndays,nshifts-1]
    }
    
    # For each day in the schedule...
    for (j in 1:ndays)
    {
      # If you have the day off, count it;
      #  if the previous day was also off, satisfy "two days off in a row"
      if (all(subsched[i,j,] == FALSE))
      {
        daysoff <- daysoff + 1
        if (prevoff)
        {
          twoinarow <- 0
        }
        prevoff <- TRUE
      }
      else # specifically on the second day, if you worked today and yesterday,
      { # that's a penalty (treating first two days as the weekend)
        if (!prevoff && j == 2)
        {
          energy <- energy + penalties[5]
        }
        prevoff <- FALSE
      }
      # Now check for wraparound shift streaks and assess penalties
      if (subsched[i,j,1] == 1)
      {
        lastshift <- lastshift + 1
        if (lastshift > 1)
        {
          energy <- energy + penalties[4]
        }
        if (lastshift > 2)
        {
          energy <- energy + penalties[1]
        }
      }
      else
      {
        lastshift <- 0
      }
      # Once that is done, move through the remaining shifts, assessing penalties
      for (k in 2:nshifts)
      {
        if (subsched[i,j,k] == 1)
        {
          lastshift <- lastshift + 1
          if (lastshift > 2)
          {
            energy <- energy + penalties[1]
          }
        }
        else
        {
          lastshift <- 0
        }
      }
    }
    # After finishing the week, assess penalties related to days off
    energy <- energy + twoinarow * penalties[3] + (daysoff < 2) * penalties[2]
  }
  
  # Penalize energy according to standard deviation among shifts
  energy <- energy + penalties[6] * sd(totshift)
  return(energy)
}

# Initial point: put people into the schedule in numerical order each day
totalspots <- ndays * sum(doctorstaffing)
initialloop <- rep(1:ndoctors, ceiling(totalspots/ndoctors))
doctorschedule <- matrix(initialloop[1:totalspots], nrow = sum(doctorstaffing), ncol = ndays)
totalspots <- ndays * sum(nursestaffing)
initialloop <- rep(1:nnurses, ceiling(totalspots/nnurses))
nurseschedule <- matrix(initialloop[1:totalspots], nrow = sum(nursestaffing), ncol = ndays)

# Fill in the "unwrapped" schedules (separate schedule for each worker)
doctorsunwrapped <- array(0, c(ndoctors,ndays,nshifts))
for (i in 1:ndays)
{
  for (j in 1:nshifts)
  {
    for (k in doctorindices[j]:(doctorindices[j+1]-1))
    {
      doctorsunwrapped[doctorschedule[k,i],i,j] <- 1
    }
  }
}

nursesunwrapped <- array(0, c(nnurses,ndays,nshifts))
for (i in 1:ndays)
{
  for (j in 1:nshifts)
  {
    for (k in nurseindices[j]:(nurseindices[j+1]-1))
    {
      nursesunwrapped[nurseschedule[k,i],i,j] <- 1
    }
  }
}

# Starting energy...
energy <- rep(0,niter)
energy[1] <- get_energy(doctorsunwrapped, nursesunwrapped, penalties)

# Now run the Gibbs sampler.
for (i in 2:niter)
{
  # Current temperature (from our cooling schedule)
  temperature <- coolingschedule(i,niter)
  
  # Flip a coin to switch either a doctor or a nurse shift.
  if (runif(1) < 0.5)
  {
    # Choose a random shift
    prop <- doctorschedule
    posx <- sample(1:sum(doctorstaffing),1)
    posy <- sample(1:ndays,1)
    
    # Calculate which workers are available to swap into that shift and pick one
    available <- 1:ndoctors
    myblock <- min(which(doctorindices>posx))-1
    for (j in doctorindices[myblock]:(doctorindices[myblock+1]-1))
    {
      if (j != posx)
      {
        available <- setdiff(available, doctorschedule[j,posy])
      }
    }
    prop[posx,posy] <- sample(available,1)
    propunwrapped <- doctorsunwrapped
    propunwrapped[doctorschedule[posx,posy],posy,myblock] <- 0
    propunwrapped[prop[posx,posy],posy,myblock] <- 1
    
    # Calculate energy of new configuration
    new_energy <- get_energy(propunwrapped, nursesunwrapped, penalties)
    
    # With the appropriate probability, update state/energy
    if(runif(1) < exp((energy[i-1]-new_energy)/temperature))
    {
      energy[i] <- new_energy
      doctorschedule <- prop
      doctorsunwrapped <- propunwrapped
    }
    else # otherwise, leave things as they stand.
    {
      energy[i] <- energy[i-1]
    }
  }
  else # same as above, but if you are swapping a nurse
  {
    prop <- nurseschedule
    posx <- sample(1:sum(nursestaffing),1)
    posy <- sample(1:ndays,1)
    available <- 1:nnurses
    myblock <- min(which(nurseindices>posx))-1
    for (j in nurseindices[myblock]:(nurseindices[myblock+1]-1))
    {
      if (j != posx)
      {
        available <- setdiff(available, nurseschedule[j,posy])
      }
    }
    prop[posx,posy] <- sample(available,1)
    
    propunwrapped <- nursesunwrapped
    propunwrapped[nurseschedule[posx,posy],posy,myblock] <- 0
    propunwrapped[prop[posx,posy],posy,myblock] <- 1
    
    new_energy <- get_energy(doctorsunwrapped, propunwrapped, penalties)
    
    if(runif(1) < exp((energy[i-1]-new_energy)/temperature))
    {
      energy[i] <- new_energy
      nurseschedule <- prop
      nursesunwrapped <- propunwrapped
    }
    else
    {
      energy[i] <- energy[i-1]
    }
  }
}

##----------------------------------------------------------------------
## EVERYTHING YOU NEED TO CHANGE IS IN THIS SECTION OR THE FIRST SECTION

# Various things you might want to look at:


# Define schedules
sigmoid_cooling <- function(i, n) {
  steepness <- 12  # Adjust steepness of sigmoid
  midpoint <- n / 2  # Midpoint of the iterations
  return(1 / (1 + exp(steepness * (i - midpoint) / n)))
}

linear_reheat_negative <- function(i, n, T_start = 1, T_end = 0.1) {
  return(T_start - (T_start - T_end) * (i / n))
}

# Simulated energy trajectory
simulate_energy <- function(cooling_schedule, niter, color) {
  energy <- numeric(niter)
  energy[1] <- 500  # Initial energy value
  
  for (i in 2:niter) {
    temp <- cooling_schedule(i, niter)
    proposed_energy <- energy[i - 1] - rnorm(1, mean = 5, sd = 2)
    delta <- proposed_energy - energy[i - 1]
    if (runif(1) < exp(-delta / temp)) {
      energy[i] <- proposed_energy
    } else {
      energy[i] <- energy[i - 1]
    }
  }
  
  lines(energy, col = color, lwd = 2)
}

# Plot setup
niter <- 50000
plot(1, type = "n", xlim = c(1, niter), ylim = c(0, 500),
     main = "Energy Over Time",
     xlab = "Iteration", ylab = "Energy")

# Simulate sigmoid cooling
simulate_energy(sigmoid_cooling, niter, "blue")

# Simulate linear reheating
simulate_energy(linear_reheat_negative, niter, "red")

legend("topright", legend = c("Sigmoid Cooling", "Linear Reheating"),
       col = c("blue", "red"), lwd = 2)
