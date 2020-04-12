add_configural <- function(experiment, multiplicative = T, config_strength = 1) {
  # This adds configural cues to an experiment.
  # We only include pairs (not triplets, quadruplets etc.).
  # We only include configural cues for features that are actually paired during training or testing.

  stim_defs <- experiment$stim_defs
  n_stimuli <- nrow(stim_defs)
  n_elem <- ncol(stim_defs)
  elem_cues <- experiment$cue_names
  config_cues <- vector()

  for(i in 1:(n_elem-1)) {
    for(j in (i+1):n_elem) {
      # Check whether these two cues co-occur in any stimulus.
      found <- F
      for(k in 1:n_stimuli) {
        stim <- stim_defs[k,]
        if(stim[i] * stim[j] > 0) {
          if(found == F) {
            # New cue pair is found for the first time.
            config_cues <- c(config_cues, paste0(elem_cues[i], "-", elem_cues[j]))
            stim_defs <- cbind(stim_defs, rep(0, n_stimuli))
            colnames(stim_defs) <- c(elem_cues, config_cues)
            found <- T
          }
          cue_strength <- ifelse(multiplicative == T, stim[i]*stim[j], config_strength)
          # Encode the configural cue in the current stimulus vector.
          stim_defs[k, config_cues[length(config_cues)]] <- cue_strength
        }
      }
    }
  }

  new_experiment <- experiment
  new_experiment$cue_names <- c(elem_cues, config_cues)
  new_experiment$stim_defs <- stim_defs

  new_experiment
}
