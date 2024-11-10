##----------------------------------------------------------------------
## EVERYTHING YOU NEED TO CHANGE IS IN THIS SECTION OR THE FINAL SECTION

# Number of doctors available
ndoctors <- 7

# Number of nurses
nnurses <- 10

# Number of days in the schedule
ndays <- 7

# Number of staff present for each shift (must have same # of shifts)
# e.g., c(2, 3, 3) has three shifts with 2, 3, 3 people respectively
doctorstaffing <- c(2, 2, 2)
nursestaffing <- c(3, 3, 3)

# Penalties for violating each of six constraints
penalties <- c(100, 100, 0.5, 5, 1, 2)

# Number of iterations
niter <- 50000

# Negative linear reheating schedule
linear_reheat_negative <- function(i, n, T_start = 1, T_end = 0) {
  return(T_start - (T_start - T_end) * (i / n))
}

## END OF PARAMETER SECTION
##----------------------------------------------------------------------

# Calculate indices for doctors and nurses
doctorindices <- c(1, cumsum(doctorstaffing) + 1)
nurseindices <- c(1, cumsum(nursestaffing) + 1)
nshifts <- length(doctorstaffing)

# Function to calculate the "energy" (total cost of violations)
get_energy <- function(doctorsunwrapped, nursesunwrapped, penalties) {
  return(get_energy_sub(doctorsunwrapped, penalties) +
           get_energy_sub(nursesunwrapped, penalties))
}

# Energy calculation for doctors and nurses
get_energy_sub <- function(subsched, penalties) {
  energy <- 0
  totshift <- rep(0, dim(subsched)[1])
  for (i in 1:dim(subsched)[1]) {
    totshift[i] <- sum(subsched[i, , ])
    lastshift <- subsched[i, ndays, nshifts]
    prevoff <- all(subsched[i, ndays, ] == FALSE)
    daysoff <- 0
    twoinarow <- 1
    if (lastshift == 1) {
      lastshift <- lastshift + subsched[i, ndays, nshifts - 1]
    }
    for (j in 1:ndays) {
      if (all(subsched[i, j, ] == FALSE)) {
        daysoff <- daysoff + 1
        if (prevoff) {
          twoinarow <- 0
        }
        prevoff <- TRUE
      } else {
        if (!prevoff && j == 2) {
          energy <- energy + penalties[5]
        }
        prevoff <- FALSE
      }
      if (subsched[i, j, 1] == 1) {
        lastshift <- lastshift + 1
        if (lastshift > 1) {
          energy <- energy + penalties[4]
        }
        if (lastshift > 2) {
          energy <- energy + penalties[1]
        }
      } else {
        lastshift <- 0
      }
      for (k in 2:nshifts) {
        if (subsched[i, j, k] == 1) {
          lastshift <- lastshift + 1
          if (lastshift > 2) {
            energy <- energy + penalties[1]
          }
        } else {
          lastshift <- 0
        }
      }
    }
    energy <- energy + twoinarow * penalties[3] + (daysoff < 2) * penalties[2]
  }
  energy <- energy + penalties[6] * sd(totshift)
  return(energy)
}

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

# Gibbs sampler with negative linear reheating schedule
for (i in 2:niter) {
  temperature <- linear_reheat_negative(i, niter)
  
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

# Plotting the energy over time
plot(energy, type = "l", main = "Energy Over Time", xlab = "Iteration", ylab = "Energy")

# Lowest energy achieved
min_energy <- min(energy)
cat("Lowest energy:", min_energy, "\n")

## END OF PROGRAM
