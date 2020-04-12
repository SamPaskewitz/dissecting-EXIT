use_receptors <- function(old_exp, n_points = 8, decay = 5, metric = 1) {
  # We assume that, in the original representation, there are only two or three dimensions.
  # We can use the "add_configural" function to add configural features.
  new_exp <- old_exp
  n_input_dims <- ncol(old_exp$stim_defs)
  if(n_input_dims == 2) {
    n_cues <- 2*n_points
    n_stimuli <- length(new_exp$stim_names)
    new_exp$cue_names <- c(paste0("X-", 1:n_points), paste0("Y-", 1:n_points))

    x_min <- min(old_exp$stim_defs[,1])
    x_max <- max(old_exp$stim_defs[,1])
    x_centers <- seq(from = x_min, to = x_max, length.out = n_points)
    y_min <- min(old_exp$stim_defs[,2])
    y_max <- max(old_exp$stim_defs[,2])
    y_centers <- seq(from = y_min, to = y_max, length.out = n_points)
    # Convert stim_defs
    new_exp$stim_defs <- matrix(0, nrow = n_stimuli, ncol = n_cues, dimnames = list(new_exp$stim_names, new_exp$cue_names))
    for(i in 1:n_stimuli) {
      x <- old_exp$stim_defs[i, 1]
      new_exp$stim_defs[i, 1:n_points] <- exp(-decay*abs(x - x_centers)^metric)
      y <- old_exp$stim_defs[i, 2]
      new_exp$stim_defs[i, (n_points + 1):(2*n_points)] <- exp(-decay*abs(y - y_centers)^metric)
    }
  }

  if(n_input_dims == 3) {
    n_cues <- 3*n_points
    n_stimuli <- length(new_exp$stim_names)
    new_exp$cue_names <- c(paste0("X-", 1:n_points), paste0("Y-", 1:n_points), paste0("Z-", 1:n_points))

    # Convert stim_defs
    new_exp$stim_defs <- matrix(0, nrow = n_stimuli, ncol = n_cues)

    x_min <- min(old_exp$stim_defs[,1])
    x_max <- max(old_exp$stim_defs[,1])
    x_centers <- seq(from = x_min, to = x_max, length.out = n_points)
    y_min <- min(old_exp$stim_defs[,2])
    y_max <- max(old_exp$stim_defs[,2])
    y_centers <- seq(from = y_min, to = y_max, length.out = n_points)
    z_min <- min(old_exp$stim_defs[,3])
    z_max <- max(old_exp$stim_defs[,3])
    z_centers <- seq(from = z_min, to = z_max, length.out = n_points)
    for(i in 1:n_stimuli) {
      x <- old_exp$stim_defs[i, 1]
      new_exp$stim_defs[i, 1:n_points] <- exp(-decay*abs(x - x_centers)^metric)
      y <- old_exp$stim_defs[i, 2]
      new_exp$stim_defs[i, (n_points + 1):(2*n_points)] <- exp(-decay*abs(y - y_centers)^metric)
      z <- old_exp$stim_defs[i, 3]
      new_exp$stim_defs[i, (2*n_points + 1):(3*n_points)] <- exp(-decay*abs(z - z_centers)^metric)

      x_act <- new_exp$stim_defs[, 1:n_points]
      y_act <- new_exp$stim_defs[, (n_points + 1):(2*n_points)]
      z_act <- new_exp$stim_defs[, (2*n_points + 1):(3*n_points)]
      index <- 3*n_points
    }
    new_exp$cue_names <- new_exp$cue_names[1:n_cues] # This is incredibly crude, but works.
    rownames(new_exp$stim_defs) <- new_exp$stim_names
    colnames(new_exp$stim_defs) <- new_exp$cue_names
  }

  new_exp
}
