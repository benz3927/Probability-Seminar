##----------------------------------------------------------------------
## PARAMETERS YOU MAY WANT TO CHANGE FOR EACH EXPERIMENT

# Number of experiments to compare
n_experiments <- 3

# Define different configurations for each experiment:
# - Penalty configurations for different experiments
# - Cooling schedules (linear and logarithmic)
experiment_params <- list(
  list(penalties = c(100, 100, 0.5, 5, 1, 2), coolingschedule = function(i, n) 1 - (i / (n + 1))),
  list(penalties = c(80, 120, 0.4, 6, 1, 1.5), coolingschedule = function(i, n) 0.02 / log(1 + (i + 1) / n)),
  list(penalties = c(120, 80, 0.6, 4, 2, 3), coolingschedule = function(i, n) 1 - (i / (n + 1))^2)
)

## END OF PARAMETER SECTION
##----------------------------------------------------------------------

# Initialize lists to store results of each experiment
all_energy <- list()
all_doctorschedules <- list()
all_nurseschedules <- list()

# Run each experiment
for (exp in 1:n_experiments) {
  # Set up parameters for this experiment
  penalties <- experiment_params[[exp]]$penalties
  coolingschedule <- experiment_params[[exp]]$coolingschedule
  
  # (Re-)initialize schedule matrices
  doctorschedule <- matrix(initialloop[1:(ndays * sum(doctorstaffing))], nrow = sum(doctorstaffing), ncol = ndays)
  nurseschedule <- matrix(initialloop[1:(ndays * sum(nursestaffing))], nrow = sum(nursestaffing), ncol = ndays)
  
  # Prepare the unwrapped schedules
  doctorsunwrapped <- array(0, c(ndoctors, ndays, nshifts))
  nursesunwrapped <- array(0, c(nnurses, ndays, nshifts))
  
  # Fill unwrapped schedules as before
  # (Loop similar to original for initializing doctorsunwrapped and nursesunwrapped)
  
  # Initialize energy tracking
  energy <- rep(0, niter)
  energy[1] <- get_energy(doctorsunwrapped, nursesunwrapped, penalties)
  
  # Gibbs Sampler
  for (i in 2:niter) {
    temperature <- coolingschedule(i, niter)
    # Same update code for Gibbs sampler with doctor or nurse shift swapping
  }
  
  # Store results for this experiment
  all_energy[[exp]] <- energy
  all_doctorschedules[[exp]] <- doctorschedule
  all_nurseschedules[[exp]] <- nurseschedule
}

##----------------------------------------------------------------------
## PLOTTING RESULTS SIDE-BY-SIDE FOR COMPARISON

# Plot energy trends
par(mfrow = c(2, n_experiments), mar = c(4, 4, 2, 1))

for (exp in 1:n_experiments) {
  plot(all_energy[[exp]], type = "l", col = "blue",
       main = paste("Energy Decay (Exp", exp, ")"), xlab = "Iteration", ylab = "Energy")
}

# Plot final doctor schedules for each experiment
for (exp in 1:n_experiments) {
  image(1:ndays, 1:sum(doctorstaffing), t(all_doctorschedules[[exp]]),
        main = paste("Doctor Schedule (Exp", exp, ")"), xlab = "Days", ylab = "Shifts")
}

# Plot final nurse schedules for each experiment
for (exp in 1:n_experiments) {
  image(1:ndays, 1:sum(nursestaffing), t(all_nurseschedules[[exp]]),
        main = paste("Nurse Schedule (Exp", exp, ")"), xlab = "Days", ylab = "Shifts")
}

## END OF FINAL SECTION
##----------------------------------------------------------------------

# Additional Code for Viewing an Individual Person’s Schedule
print(doctorsunwrapped[3,,])  # e.g., view doctor 3's schedule for each experiment as needed
