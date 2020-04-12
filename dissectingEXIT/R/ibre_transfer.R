ibre_transfer <- function(n_blocks = c(15, 10), n_test_reps = 1) {
  # This tests whether attention learning transfers from an IBRE to subsequent learning.
  X <- list(); class(X) <- "learnExp"
  X$n_test_reps <- n_test_reps # This determines the number of times the set of test trials is given (it's only relevant for simulating responses).
  X$action_names <- c("C1", "R1", "C2", "R2", "T1", "T2")
  ibre_names <- c("I1 + PC1", "I1 + PR1", "I2 + PC2", "I2 + PR2")
  transfer_names <- c("PC1 + PR2", "PC2 + PR1", "N1 + N2", "N3 + N4")
  test_stim_names <- c("PC1 + PR1", "PC2 + PR2")
  X$stim_names <- c(ibre_names, transfer_names, test_stim_names)
  X$cue_names <- c("I1", "PC1", "PR1", "I2", "PC2", "PR2", "N1", "N2", "N3", "N4")
  X$stim_types <- c(rep("ibre", 4), rep("transfer", 4), rep("test", 2))

  n_actions <- length(X$action_names)
  n_stimuli <- length(X$stim_names)
  n_cues <- length(X$cue_names)

  X$elg_function <- function(stage) {
    if(stage == "ibre") {
      elg <- c(1, 1, 1, 1, 0, 0)
    }
    else if(stage == "transfer") {
      elg <- c(0, 0, 0, 0, 1, 1)
    }
    else if(stage == "test") {
      elg <- rep(0, 6)
    }

    return(elg)
  }

  X$avail_function <- function(stage) {
    if(stage == "ibre") {
      avail <- c(1, 1, 1, 1, 0, 0)
    }
    else if(stage == "transfer") {
      avail <- c(0, 0, 0, 0, 1, 1)
    }
    else if(stage == "test") {
      avail <- c(0, 0, 0, 0, 1, 1)
    }

    return(avail)
  }

  fb_matrix <- matrix(0, nrow = n_stimuli, ncol = n_actions, dimnames = list(X$stim_names, X$action_names))
  fb_matrix["I1 + PC1", "C1"] <- 1
  fb_matrix["I1 + PR1", "R1"] <- 1
  fb_matrix["I2 + PC2", "C2"] <- 1
  fb_matrix["I2 + PR2", "R2"] <- 1
  fb_matrix["PC1 + PR2", "T1"] <- 1
  fb_matrix["PC2 + PR1", "T2"] <- 1
  fb_matrix["N1 + N2", "T1"] <- 1
  fb_matrix["N3 + N4", "T2"] <- 1

  X$fb_function <- function(stimVector, stim_seq, trial, stage) {
    fb <- fb_matrix[stim_seq[trial],]

    return(fb)
  }

  X$stim_defs <- matrix(0, nrow = n_stimuli, ncol = n_cues, dimnames = list(X$stim_names, X$cue_names))
  # ibre
  X$stim_defs["I1 + PC1", c("I1", "PC1")] <- 1
  X$stim_defs["I1 + PR1", c("I1", "PR1")] <- 1
  X$stim_defs["I2 + PC2", c("I2", "PC2")] <- 1
  X$stim_defs["I2 + PR2", c("I2", "PR2")] <- 1
  # transfer
  X$stim_defs["PC1 + PR2", c("PC1", "PR2")] <- 1
  X$stim_defs["PC2 + PR1", c("PC2", "PR1")] <- 1
  X$stim_defs["N1 + N2", c("N1", "N2")] <- 1
  X$stim_defs["N3 + N4", c("N3", "N4")] <- 1
  # test
  X$stim_defs["PC1 + PR1", c("PC1", "PR1")] <- 1
  X$stim_defs["PC2 + PR2", c("PC2", "PR2")] <- 1

  X$stim_seq <- vector()
  for(i in 1:n_blocks[1]) {
    X$stim_seq <- c(X$stim_seq, sample(c(rep("I1 + PC1", 3), "I1 + PR1", rep("I2 + PC2", 3), "I2 + PR2")))
  }
  for(i in 1:n_blocks[2]) {
    X$stim_seq <- c(X$stim_seq, sample(transfer_names))
  }
  for(i in 1:n_test_reps) {
    X$stim_seq <- c(X$stim_seq, test_stim_names)
  }
  X$stage <- c(rep("ibre", n_blocks[1]*8),
               rep("transfer", n_blocks[2]*4),
               rep("test", n_test_reps*2))

  X
}
