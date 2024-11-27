##----------------------------------------------------------------------
## EVERYTHING YOU NEED TO CHANGE IS IN THIS SECTION OR THE FINAL SECTION

# Shortage of doctors, close to real numbers ratio
# Number of doctors available
# in realty before scaled down
ndoctors <- 100

# Number of nurses
nnurses <- 250

# Number of days in the schedule
ndays <- 7

# In US, on average each 12 hour shift,

# Number of staff present for each shift (must have same # of shifts)
# e.g., c(2, 3, 3) has three shifts with 2, 3, 3 people respectively
# Ideally, especially for urgent care, a higher doctor to nurse ratio is better
# 1:2 nurse to patient ratio
# 1 doctor : 2 nurses or fewer at all times
# 14-19: 16 patients per 12 hour shift
# Ideal
doctorstaffing <- c(6, 4)
nursestaffing <- c(12, 8)

# Penalties for violating each of six constraints
# Penalty 1: Each instance of two consecutive shifts (including overnight)
# Penalty 2: Each person who does not have two days off AT ALL
# Penalty 3: Each person who does not have two days off IN A ROW
# Penalty 4: Each instance of first shift following last shift on prev day
# Penalty 5: Each instance of working both weekend days (days 1, 2)
# Penalty 6: Weights for the stdev of nurses'/doctors' shifts in the schedule
#            (trying to make everyone work similar amounts)
#Penalty 7: Each instance of exceeding three night shifts/week
penalties <- c(100, 100, 0.5, 5, 1, 2, 50)

# Number of iterations (if too small, might not be able to explore enough)
niter <- 30000

# The cooling schedule: what should the temperature be at step i out of n
coolingschedule <- function(i,n)
{
  # If you want greedy optimization, just fix some low temperature
  #return(1e-3)
  
  # Linear cooling schedule from 1 down to 0
  return(1 - (i/(n+1)))
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
get_energy_sub <- function(subsched, penalties) {
  max_night_shifts_allowed = 3
  energy <- 0
  
  # Array to keep track of total number of shifts worked by each worker
  totshift <- rep(0, dim(subsched)[1])
  
  # Array to keep track of the number of night shifts worked by each worker
  night_shifts <- rep(0, dim(subsched)[1])
  
  # Loop through each worker
  for (i in 1:dim(subsched)[1]) {
    # Count total shifts and night shifts for the current worker
    totshift[i] <- sum(subsched[i,,])
    night_shifts[i] <- sum(subsched[i,,] == 1)
    
    # Penalize if worker exceeds the max number of night shifts allowed (penalize each night shift over the limit)
    if (night_shifts[i] > max_night_shifts_allowed) {
      energy <- energy + penalties[6] * (night_shifts[i] - max_night_shifts_allowed)
    }
    
    # Check the last shift and whether worker has the last day off
    lastshift <- subsched[i, ndays, nshifts]
    prevoff <- all(subsched[i, ndays,] == FALSE)
    
    daysoff <- 0
    twoinarow <- 1  # Variable for tracking consecutive days off
    
    # Check for consecutive shifts across weeks and days off in a row
    if (lastshift == 1) {
      lastshift <- lastshift + subsched[i, ndays, nshifts - 1]  # Check if last shift is consecutive to the previous week
    }
    
    # Loop through each day in the schedule
    for (j in 1:ndays) {
      # If the worker has the day off, check if it satisfies "two days off in a row" condition
      if (all(subsched[i, j,] == FALSE)) {
        daysoff <- daysoff + 1
        if (prevoff) {
          twoinarow <- 0  # No consecutive off days if previous day was also off
        }
        prevoff <- TRUE
      } else {
        # If worker worked on the second day of a weekend-like schedule, penalize
        if (!prevoff && j == 2) {
          energy <- energy + penalties[5]
        }
        prevoff <- FALSE
      }
      
      # Check for wraparound shift streaks and assess penalties
      if (subsched[i, j, 1] == 1) {
        lastshift <- lastshift + 1
        if (lastshift > 0) {
          energy <- energy + penalties[4]
        }
        if (lastshift > 1) {
          energy <- energy + penalties[1]
        }
      } else {
        lastshift <- 0  # Reset if no shift worked
      }
      
      # Loop through remaining shifts in the day
      for (k in 2:nshifts) {
        if (subsched[i, j, k] == 1) {
          lastshift <- lastshift + 1
          if (lastshift > 1) {
            energy <- energy + penalties[1]
          }
        } else {
          lastshift <- 0  # Reset for non-working shifts
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

# Number of staff present for each shift (must have same # of shifts)
# e.g., c(2, 3, 3) has three shifts with 2, 3, 3 people respectively
# Ideally, especially for urgent care, a higher doctor to nurse ratio is better

# Various things you might want to look at:

# Plotting the energy over time
plot(energy, type="l", col="#4682B4", lwd=0.9, xlab="Iteration", ylab="Energy")

# What was the lowest energy? (Is this necessarily the energy of the final state?)
min(energy)

# Check out our final schedule.
#  The columns are days. If there are three shifts of 2 workers,
#  the first 2 rows are shift 1, the next 2 rows are shift 2, etc.
#  Numbers in table correspond to workers. 1, 2, 3, ...
print(doctorschedule)
print(nurseschedule)

# If you want to look at an individual person's schedule,
#  check the "unwrapped" matrices. For example,
#  the shift schedule for doctor 3 is below; rows are days,
#  columns are shifts, 1 = working, 0 not working
print(doctorsunwrapped[3,,])

## END OF FINAL SECTION
##----------------------------------------------------------------------

# Greedy
##----------------------------------------------------------------------
## EVERYTHING YOU NEED TO CHANGE IS IN THIS SECTION OR THE FINAL SECTION

# Number of staff present for each shift (must have same # of shifts)
# e.g., c(2, 3, 3) has three shifts with 2, 3, 3 people respectively
# Ideally, especially for urgent care, a higher doctor to nurse ratio is better

# The cooling schedule: what should the temperature be at step i out of n
coolingschedule <- function(i,n)
{
  # If you want greedy optimization, just fix some low temperature
  return(1e-3)
}

## END OF PARAMETER SECTION
##----------------------------------------------------------------------

# (Some things which are calculated automatically)
doctorindices <- c(1,cumsum(doctorstaffing)+1)
nurseindices <- c(1,cumsum(nursestaffing)+1)
nshifts <- length(doctorstaffing)

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
# Plotting the energy over time
lines(energy, type = "l", col = "#FFA500", lwd = 0.9, xlab = "Iteration", ylab = "Energy")

# What was the lowest energy? (Is this necessarily the energy of the final state?)
min(energy)

# Check out our final schedule.
#  The columns are days. If there are three shifts of 2 workers,
#  the first 2 rows are shift 1, the next 2 rows are shift 2, etc.
#  Numbers in table correspond to workers. 1, 2, 3, ...
print(doctorschedule)
print(nurseschedule)

# If you want to look at an individual person's schedule,
#  check the "unwrapped" matrices. For example,
#  the shift schedule for doctor 3 is below; rows are days,
#  columns are shifts, 1 = working, 0 not working
print(doctorsunwrapped[3,,])

## END OF FINAL SECTION
##----------------------------------------------------------------------


#Log
##----------------------------------------------------------------------
## EVERYTHING YOU NEED TO CHANGE IS IN THIS SECTION OR THE FINAL SECTION

# Number of staff present for each shift (must have same # of shifts)
# e.g., c(2, 3, 3) has three shifts with 2, 3, 3 people respectively
# Ideally, especially for urgent care, a higher doctor to nurse ratio is better

# The cooling schedule: what should the temperature be at step i out of n
coolingschedule <- function(i,n)
{
  # Log cooling schedule
  return(0.02/log(1+(i+1)/n))
}

## END OF PARAMETER SECTION
##----------------------------------------------------------------------

# (Some things which are calculated automatically)
doctorindices <- c(1,cumsum(doctorstaffing)+1)
nurseindices <- c(1,cumsum(nursestaffing)+1)
nshifts <- length(doctorstaffing)

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

# Plotting the energy over time
# Add the second line with a red dashed line
lines(energy, type="l", col="#006400", lwd=0.9, lty=1)  #

# What was the lowest energy? (Is this necessarily the energy of the final state?)
min(energy)

# Check out our final schedule.
#  The columns are days. If there are three shifts of 2 workers,
#  the first 2 rows are shift 1, the next 2 rows are shift 2, etc.
#  Numbers in table correspond to workers. 1, 2, 3, ...
print(doctorschedule)
print(nurseschedule)

# If you want to look at an individual person's schedule,
#  check the "unwrapped" matrices. For example,
#  the shift schedule for doctor 3 is below; rows are days,
#  columns are shifts, 1 = working, 0 not working
print(doctorsunwrapped[3,,])

## END OF FINAL SECTION
##----------------------------------------------------------------------


# Exponential
##----------------------------------------------------------------------
## EVERYTHING YOU NEED TO CHANGE IS IN THIS SECTION OR THE FINAL SECTION

# Number of staff present for each shift (must have same # of shifts)
# e.g., c(2, 3, 3) has three shifts with 2, 3, 3 people respectively
# Ideally, especially for urgent care, a higher doctor to nurse ratio is better

# Exponential cooling schedule
coolingschedule <- function(i, n) {
  T0 <- 1 # Initial temperature
  alpha <- 5 / n # Decay rate (adjust this as needed for your application)
  return(T0 * exp(-alpha * i))
}

## END OF PARAMETER SECTION
##----------------------------------------------------------------------

# Calculate indices for doctors and nurses
doctorindices <- c(1, cumsum(doctorstaffing) + 1)
nurseindices <- c(1, cumsum(nursestaffing) + 1)
nshifts <- length(doctorstaffing)

# Initial schedules
totalspots <- ndays * sum(doctorstaffing)
initialloop <- rep(1:ndoctors, ceiling(totalspots / ndoctors))
doctorschedule <- matrix(initialloop[1:totalspots], nrow = sum(doctorstaffing), ncol = ndays)
totalspots <- ndays * sum(nursestaffing)
initialloop <- rep(1:nnurses, ceiling(totalspots / nnurses))
nurseschedule <- matrix(initialloop[1:totalspots], nrow = sum(nursestaffing), ncol = ndays)

# Unwrapped schedules for each worker
doctorsunwrapped <- array(0, c(ndoctors, ndays, nshifts))
for (i in 1:ndays) {
  for (j in 1:nshifts) {
    for (k in doctorindices[j]:(doctorindices[j + 1] - 1)) {
      doctorsunwrapped[doctorschedule[k, i], i, j] <- 1
    }
  }
}

nursesunwrapped <- array(0, c(nnurses, ndays, nshifts))
for (i in 1:ndays) {
  for (j in 1:nshifts) {
    for (k in nurseindices[j]:(nurseindices[j + 1] - 1)) {
      nursesunwrapped[nurseschedule[k, i], i, j] <- 1
    }
  }
}

# Starting energy
energy <- rep(0, niter)
energy[1] <- get_energy(doctorsunwrapped, nursesunwrapped, penalties)

# Gibbs sampler with exponential cooling schedule
for (i in 2:niter) {
  temperature <- coolingschedule(i, niter)
  
  if (runif(1) < 0.5) {
    prop <- doctorschedule
    posx <- sample(1:sum(doctorstaffing), 1)
    posy <- sample(1:ndays, 1)
    available <- 1:ndoctors
    myblock <- min(which(doctorindices > posx)) - 1
    for (j in doctorindices[myblock]:(doctorindices[myblock + 1] - 1)) {
      if (j != posx) {
        available <- setdiff(available, doctorschedule[j, posy])
      }
    }
    prop[posx, posy] <- sample(available, 1)
    propunwrapped <- doctorsunwrapped
    propunwrapped[doctorschedule[posx, posy], posy, myblock] <- 0
    propunwrapped[prop[posx, posy], posy, myblock] <- 1
    new_energy <- get_energy(propunwrapped, nursesunwrapped, penalties)
    if (runif(1) < exp((energy[i - 1] - new_energy) / temperature)) {
      energy[i] <- new_energy
      doctorschedule <- prop
      doctorsunwrapped <- propunwrapped
    } else {
      energy[i] <- energy[i - 1]
    }
  } else {
    prop <- nurseschedule
    posx <- sample(1:sum(nursestaffing), 1)
    posy <- sample(1:ndays, 1)
    available <- 1:nnurses
    myblock <- min(which(nurseindices > posx)) - 1
    for (j in nurseindices[myblock]:(nurseindices[myblock + 1] - 1)) {
      if (j != posx) {
        available <- setdiff(available, nurseschedule[j, posy])
      }
    }
    prop[posx, posy] <- sample(available, 1)
    propunwrapped <- nursesunwrapped
    propunwrapped[nurseschedule[posx, posy], posy, myblock] <- 0
    propunwrapped[prop[posx, posy], posy, myblock] <- 1
    new_energy <- get_energy(doctorsunwrapped, propunwrapped, penalties)
    if (runif(1) < exp((energy[i - 1] - new_energy) / temperature)) {
      energy[i] <- new_energy
      nurseschedule <- prop
      nursesunwrapped <- propunwrapped
    } else {
      energy[i] <- energy[i - 1]
    }
  }
}

##----------------------------------------------------------------------
## EVERYTHING YOU NEED TO CHANGE IS IN THIS SECTION OR THE FIRST SECTION

# Lowest energy achieved
cat("Lowest Energy:", min(energy), "\n")

# Final schedules for doctors and nurses
print("Final Doctors Schedule:")
print(doctorschedule)
print("Final Nurses Schedule:")
print(nurseschedule)

# Example: Doctor 3's unwrapped schedule
print("Doctor 3's Schedule:")
print(doctorsunwrapped[3, , ])

## END OF FINAL SECTION
##----------------------------------------------------------------------

# Plotting the energy over time
lines(energy, type = "l", col = "#B22222", lwd = 0.75)

# Lowest energy achieved
min_energy <- min(energy)
cat("Lowest energy:", min_energy, "\n")

# Add custom legend
legend("topright", 
       legend = c("Linear", "Greedy", "Log", "Exponential"),  # New legend text
       col = c("#4682B4", "#FFA500", "#006400","#B22222"),                # Colors of the lines
       lty = c(1, 1),                          # Line types for each
       lwd = c(2, 2),                          # Line widths for each
       box.lwd = 0.5)                          # Border line width around the legend

title(main = "Energy vs. Time")
