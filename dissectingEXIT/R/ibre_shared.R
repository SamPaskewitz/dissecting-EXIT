ibre_shared <- function(n_blocks = c(8, 20), n_test_reps = 1) {
  # Kruschke 2001, Experiment 1 (importance of shared cues for the inverse base rate effect)
  # The default number of blocks is the number used in the original experiment.
  # I'm using slightly different names for the cues.
  X <- list(); class(X) <- "learnExp"
  X$n_test_reps <- n_test_reps # This determines the number of times the set of test trials is given (it's only relevant for simulating responses).
  X$action_names <- c("DC", "DR", "C", "R")
  training_names <- c("DC1 + DC2", "DR1 + DR2", "I + PC", "I + PR")
  test_stim_names <- c("PC", "PR", "I", "PC + PR", "DC1 + DR1", "DC2 + DR2") # There were some other test trial types.
  X$stim_names <- c(training_names, test_stim_names)
  X$cue_names <- c("DC1", "DC2", "DR1", "DR2", "I", "PC", "PR")
  X$stim_types <- c(rep("training", 4), rep("test", 6))

  n_actions <- length(X$action_names)
  n_stimuli <- length(X$stim_names)
  n_cues <- length(X$cue_names)

  X$elg_function <- function(stage) {
    if(stage == "test") {
      elg <- rep(0, 4)
    }
    else {
      elg <- rep(1, 4)
    }
    elg
  }

  X$avail_function <- function(stage) {
    avail <- rep(1, 4)
    avail
  }

  fb_matrix <- matrix(0, nrow = n_stimuli, ncol = n_actions, dimnames = list(X$stim_names, X$action_names))
  fb_matrix["DC1 + DC2", "DC"] <- 1
  fb_matrix["DR1 + DR2", "DR"] <- 1
  fb_matrix["I + PC", "C"] <- 1
  fb_matrix["I + PR", "R"] <- 1

  X$fb_function <- function(stimVector, stim_seq, trial, stage) {
    fb <- fb_matrix[stim_seq[trial],]
    fb
  }

  X$stim_defs <- matrix(0, nrow = n_stimuli, ncol = n_cues, dimnames = list(X$stim_names, X$cue_names))
  # training
  X$stim_defs["DC1 + DC2", c("DC1", "DC2")] <- 1
  X$stim_defs["DR1 + DR2", c("DR1", "DR2")] <- 1
  X$stim_defs["I + PC", c("I", "PC")] <- 1
  X$stim_defs["I + PR", c("I", "PR")] <- 1
  # test
  X$stim_defs["PC", "PC"] <- 1
  X$stim_defs["PR", "PR"] <- 1
  X$stim_defs["I", "I"] <- 1
  X$stim_defs["PC + PR", c("PC", "PR")] <- 1
  X$stim_defs["DC1 + DR1", c("DC1", "DR1")] <- 1
  X$stim_defs["DC2 + DR2", c("DC2", "DR2")] <- 1

  X$stim_seq <- vector()
  for (i in 1:n_blocks[1]) {
    X$stim_seq <- c(X$stim_seq, sample(c("DC1 + DC2", "I + PC")))
  }
  for(i in 1:n_blocks[2]) {
    X$stim_seq <- c(X$stim_seq, sample(c(rep("DC1 + DC2", 3), "DR1 + DR2", rep("I + PC", 3), "I + PR")))
  }
  for(i in 1:n_test_reps) {
    X$stim_seq <- c(X$stim_seq, test_stim_names)
  }
  X$stage <- c(rep("initial", n_blocks[1]*2),
               rep("training", n_blocks[2]*8),
               rep("test", n_test_reps*6))

  X
}
