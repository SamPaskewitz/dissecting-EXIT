ibre <- function(n_blocks = 15, n_test_reps = 1) {
  # Kruschke 1996, Experiment 1 (Inverse Base Rate Effect)
  # The default number of blocks is the number used in the original experiment.
  X <- list(); class(X) <- "learnExp"
  X$n_test_reps <- n_test_reps # This determines the number of times the set of test trials is given (it's only relevant for simulating responses).
  X$action_names <- c("C1", "R1", "C2", "R2")
  training_names <- c("I1 + PC1", "I1 + PR1", "I2 + PC2", "I2 + PR2")
  test_stim_names <- c("I1", "PC1", "PR1", "PC1 + PR1", "I1 + PC1 + PR1", "I1 + PC2", "I1 + PR2", "PC1 + PR2", "I1 + PC1 + PR2",
                       "I2", "PC2", "PR2", "PC2 + PR2", "I2 + PC2 + PR2", "I2 + PC1", "I2 + PR1", "PC2 + PR1", "I2 + PC2 + PR1")
  X$stim_names <- c(training_names, test_stim_names)
  X$cue_names <- c("I1", "PC1", "PR1", "I2", "PC2", "PR2")
  X$stim_types <- c(rep("training", 4), rep("test", 18))

  n_actions <- length(X$action_names)
  n_stimuli <- length(X$stim_names)
  n_cues <- length(X$cue_names)

  X$elg_function <- function(stage) {
    if(stage == "training") {
      elg <- rep(1, 4)
    }
    if(stage == "test") {
      elg <- rep(0, 4)
    }
    elg
  }

  X$avail_function <- function(stage) {
    avail <- rep(1, 4)
    avail
  }

  fb_matrix <- matrix(0, nrow = n_stimuli, ncol = n_actions, dimnames = list(X$stim_names, X$action_names))
  fb_matrix["I1 + PC1", "C1"] <- 1
  fb_matrix["I1 + PR1", "R1"] <- 1
  fb_matrix["I2 + PC2", "C2"] <- 1
  fb_matrix["I2 + PR2", "R2"] <- 1

  X$fb_function <- function(stimVector, stim_seq, trial, stage) {
    fb <- fb_matrix[stim_seq[trial],]
    fb
  }

  X$stim_defs <- matrix(0, nrow = n_stimuli, ncol = n_cues, dimnames = list(X$stim_names, X$cue_names))
  # training
  X$stim_defs["I1 + PC1", c("I1", "PC1")] <- 1
  X$stim_defs["I1 + PR1", c("I1", "PR1")] <- 1
  X$stim_defs["I2 + PC2", c("I2", "PC2")] <- 1
  X$stim_defs["I2 + PR2", c("I2", "PR2")] <- 1
  # test (set 1)
  X$stim_defs["I1", "I1"] <- 1
  X$stim_defs["PC1", "PC1"] <- 1
  X$stim_defs["PR1", "PR1"] <- 1
  X$stim_defs["PC1 + PR1", c("PC1", "PR1")] <- 1
  X$stim_defs["I1 + PC1 + PR1", c("I1", "PC1", "PR1")] <- 1
  X$stim_defs["I1 + PC2", c("I1", "PC2")] <- 1
  X$stim_defs["I1 + PR2", c("I1", "PR2")] <- 1
  X$stim_defs["PC1 + PR2", c("PC1", "PR2")] <- 1
  X$stim_defs["I1 + PC1 + PR2", c("I1", "PC1", "PR2")] <- 1
  # test (set 2)
  X$stim_defs["I2", "I2"] <- 1
  X$stim_defs["PC2", "PC2"] <- 1
  X$stim_defs["PR2", "PR2"] <- 1
  X$stim_defs["PC2 + PR2", c("PC2", "PR2")] <- 1
  X$stim_defs["I2 + PC2 + PR2", c("I2", "PC2", "PR2")] <- 1
  X$stim_defs["I2 + PC1", c("I2", "PC1")] <- 1
  X$stim_defs["I2 + PR1", c("I2", "PR1")] <- 1
  X$stim_defs["PC2 + PR1", c("PC2", "PR1")] <- 1
  X$stim_defs["I2 + PC2 + PR1", c("I2", "PC2", "PR1")] <- 1

  X$stim_seq <- vector()
  for(i in 1:n_blocks) {
    X$stim_seq <- c(X$stim_seq, sample(c(rep("I1 + PC1", 3), "I1 + PR1", rep("I2 + PC2", 3), "I2 + PR2")))
  }
  for(i in 1:n_test_reps) {
    X$stim_seq <- c(X$stim_seq, test_stim_names)
  }
  X$stage <- c(rep("training", n_blocks*8),
               rep("test", n_test_reps*18))

  X
}
