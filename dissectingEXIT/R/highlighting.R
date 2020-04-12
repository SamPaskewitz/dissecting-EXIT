highlighting <- function(n_blocks = c(10, 5), n_test_reps = 1) {
  # Kruschke 2009, the "canonical" highlighting design
  # The default numbers of blocks are those used in the original experiment.
  X <- list(); class(X) <- "learnExp"
  X$n_test_reps <- n_test_reps # This determines the number of times the set of test trials is given (it's only relevant for simulating responses).
  X$action_names <- c("E1", "L1", "E2", "L2")
  training_names <- c("I1 + PE1", "I1 + PL1", "I2 + PE2", "I2 + PL2")
  test_stim_names <- c("I1",
                       "PE1",
                       "PL1",
                       "PE1 + PL1",
                       "I1 + PE1 + PL1",
                       "I1 + PE2",
                       "I1 + PL2",
                       "PE1 + PL2",
                       "I1 + PE1 + PL2",
                       "I2",
                       "PE2",
                       "PL2",
                       "PE2 + PL2",
                       "I2 + PE2 + PL2",
                       "I2 + PE1",
                       "I2 + PL1",
                       "PE2 + PL1",
                       "I2 + PE2 + PL1")
  # Later, revise the test stimuli.  These are slightly different from the 1996 "early late" design.
  X$stim_names <- c(training_names, test_stim_names)
  X$cue_names <- c("I1", "PE1", "PL1", "I2", "PE2", "PL2")
  X$stimTypes <- c(rep("training", 4), rep("test", 18))

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
  fb_matrix["I1 + PE1", "E1"] <- 1
  fb_matrix["I1 + PL1", "L1"] <- 1
  fb_matrix["I2 + PE2", "E2"] <- 1
  fb_matrix["I2 + PL2", "L2"] <- 1
  X$fb_function <- function(stim_vector, stim_seq, trial, stage) {
    fb_matrix[stim_seq[trial],]
  }

  X$stim_defs <- matrix(0, nrow = n_stimuli, ncol = n_cues, dimnames = list(X$stim_names, X$cue_names))
  # training
  X$stim_defs["I1 + PE1", c("I1", "PE1")] <- 1
  X$stim_defs["I1 + PL1", c("I1", "PL1")] <- 1
  X$stim_defs["I2 + PE2", c("I2", "PE2")] <- 1
  X$stim_defs["I2 + PL2", c("I2", "PL2")] <- 1
  # test (set 1)
  X$stim_defs["I1", "I1"] <- 1
  X$stim_defs["PE1", "PE1"] <- 1
  X$stim_defs["PL1", "PL1"] <- 1
  X$stim_defs["PE1 + PL1", c("PE1", "PL1")] <- 1
  X$stim_defs["I1 + PE1 + PL1", c("I1", "PE1", "PL1")] <- 1
  X$stim_defs["I1 + PE2", c("I1", "PE2")] <- 1
  X$stim_defs["I1 + PL2", c("I1", "PL2")] <- 1
  X$stim_defs["PE1 + PL2", c("PE1", "PL2")] <- 1
  X$stim_defs["I1 + PE1 + PL2", c("I1", "PE1", "PL2")] <- 1
  # test (set 2)
  X$stim_defs["I2", "I2"] <- 1
  X$stim_defs["PE2", "PE2"] <- 1
  X$stim_defs["PL2", "PL2"] <- 1
  X$stim_defs["PE2 + PL2", c("PE2", "PL2")] <- 1
  X$stim_defs["I2 + PE2 + PL2", c("I2", "PE2", "PL2")] <- 1
  X$stim_defs["I2 + PE1", c("I2", "PE1")] <- 1
  X$stim_defs["I2 + PL1", c("I2", "PL1")] <- 1
  X$stim_defs["PE2 + PL1", c("PE2", "PL1")] <- 1
  X$stim_defs["I2 + PE2 + PL1", c("I2", "PE2", "PL1")] <- 1

  X$stim_seq <- vector()
  for(i in 1:n_blocks[1]) {
    X$stim_seq <- c(X$stim_seq, sample(c(rep("I1 + PE1", 2), rep("I2 + PE2", 2))))
  }
  for(i in 1:n_blocks[2]) {
    X$stim_seq <- c(X$stim_seq, sample(c(rep("I1 + PE1", 3), rep("I2 + PE2", 3), "I1 + PL1", "I2 + PL2")) )
  }
  for(i in 1:(n_blocks[1] + n_blocks[2])) {
    X$stim_seq <- c(X$stim_seq, sample(c("I1 + PE1", "I2 + PE2", rep("I1 + PL1", 3), rep("I2 + PL2", 3))) )
  }
  for(i in 1:n_test_reps) {
    X$stim_seq <- c(X$stim_seq, test_stim_names)
  }
  X$stage <- c(rep("stage 1", n_blocks[1]*4),
               rep("stage 2", n_blocks[2]*8),
               rep("stage 3", (n_blocks[1] + n_blocks[2])*8),
               rep("test", n_test_reps*18))

  X
}
