add_unique <- function(experiment) {
  # This adds a unique cue for each stimulus.
  # This has the effect of adding an n-ary configural cue to stimuli with n elements,
  # so long as each stimulus contains the same number of elements and none is duplicated.

  n_stimuli <- nrow(experiment$stim_defs)
  new_experiment <- experiment
  new_experiment$stim_defs <- cbind(experiment$stim_defs, diag(n_stimuli))
  new_experiment$cue_names <- c(experiment$cue_names, experiment$stim_names)
  colnames(new_experiment$stim_defs) <- new_experiment$cue_names

  return(new_experiment)
}
