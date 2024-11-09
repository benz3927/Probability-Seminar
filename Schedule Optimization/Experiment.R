# Number of doctors and nurses
ndoctors <- 10
nnurses <- 15

# Number of days in the schedule
ndays <- 7

# Shifts staffing requirements
doctorstaffing <- c(2, 2, 2)
nursestaffing <- c(3, 3, 3)
nshifts <- length(doctorstaffing)

# Automatically generated parameters
doctorindices <- c(1, cumsum(doctorstaffing) + 1)
nurseindices <- c(1, cumsum(nursestaffing) + 1)

# Initialize the unwrapped schedules as arrays of zeros
doctorsunwrapped <- array(0, c(ndoctors, ndays, nshifts))
nursesunwrapped <- array(0, c(nnurses, ndays, nshifts))

# Initial schedules, where workers are assigned in order to each shift
totalspots <- ndays * sum(doctorstaffing)
initialloop <- rep(1:ndoctors, length.out = totalspots)
doctorschedule <- matrix(initialloop, nrow = sum(doctorstaffing), ncol = ndays)

totalspots <- ndays * sum(nursestaffing)
initialloop <- rep(1:nnurses, length.out = totalspots)
nurseschedule <- matrix(initialloop, nrow = sum(nursestaffing), ncol = ndays)

# Populate the unwrapped matrices with 1s for assigned shifts
for (i in 1:ndays) {
  for (j in 1:nshifts) {
    # Populate doctors' shifts
    for (k in doctorindices[j]:(doctorindices[j+1] - 1)) {
      doctorsunwrapped[doctorschedule[k, i], i, j] <- 1
    }
    # Populate nurses' shifts
    for (k in nurseindices[j]:(nurseindices[j+1] - 1)) {
      nursesunwrapped[nurseschedule[k, i], i, j] <- 1
    }
  }
}

# Print the unwrapped matrices to check the initial 1s and 0s
print(doctorsunwrapped)
print(nursesunwrapped)
