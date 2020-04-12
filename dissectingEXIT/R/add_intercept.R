add_intercept <- function(experiment) {
  new_cue_names <- c(experiment$cue_names, "intercept")
  new_stim_defs <- cbind(experiment$stim_defs, rep(1, nrow(experiment$stim_defs)))
  colnames(new_stim_defs) <- new_cue_names

  new_experiment <- experiment
  new_experiment$cue_names <- new_cue_names
  new_experiment$stim_defs <- new_stim_defs

  new_experiment
}
