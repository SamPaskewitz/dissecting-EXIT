blocked_cues <- function(n_blocks = c(10, 8, 6), n_test_reps = 1) {
  # This is based on Beesley and Le Pelley, 2011.
  # The default block numbers are from the paper.
  X <- list(); class(X) <- "learnExp"
  X$n_test_reps <- n_test_reps # This determines the number of times the set of test trials is given (it's only relevant for simulating responses).
  X$action_names <- c("cat 1", "cat 2", "cat 3", "cat 4")
  stage1_names <- c("A", "C", "E", "G", "I", "L", "O", "R")
  stage2_names <- c("A + B", "C + D", "E + F", "G + H", "J + K", "M + N", "P + Q", "S + T")
  stage3_names <- c("B + K", "D + N", "F + Q", "H + T")
  test_stim_names <- c("B + T", "D + Q", "F + N", "H + K", "B + N", "D + K", "F + T", "H + Q")
  X$stim_names <- c(stage1_names, stage2_names, stage3_names, test_stim_names)
  X$cue_names <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T")
  X$stim_types <- c(rep("training", 20), rep("test", 8))

  n_actions <- length(X$action_names)
  n_stimuli <- length(X$stim_names)
  n_cues <- length(X$cue_names)

  X$elg_function <- function(stage) {
    if(stage %in% c("stage 1", "stage 2")) {
      elg <- c(1, 1, 0, 0)
    }
    if(stage == "stage 3") {
      elg <- c(0, 0, 1, 1)
    }
    if(stage == "test") {
      elg <- rep(0, 4)
    }

    elg
  }

  X$avail_function <- function(stage) {
    if(stage %in% c("stage 1", "stage 2")) {
      avail <- c(1, 1, 0, 0)
    }
    if(stage %in% c("stage 3", "test")) {
      avail <- c(0, 0, 1, 1)
    }

    avail
  }

  fb_matrix <- matrix(0, nrow = n_stimuli, ncol = n_actions, dimnames = list(X$stim_names, X$action_names))
  fb_matrix[c("A", "C", "I", "L"), "cat 1"] <- 1
  fb_matrix[c("E", "G", "O", "R"), "cat 2"] <- 1
  fb_matrix[c("A + B", "C + D", "J + K", "M + N"), "cat 1"] <- 1
  fb_matrix[c("E + F", "G + H", "P + Q", "S + T"), "cat 2"] <- 1
  fb_matrix[c("B + K", "F + Q"), "cat 3"] <- 1
  fb_matrix[c("D + N", "H + T"), "cat 4"] <- 1

  X$fb_function <- function(stim_vector, stim_seq, trial, stage) {
    fb_matrix[stim_seq[trial],]
  }

  X$stim_defs <- matrix(0, nrow = n_stimuli, ncol = n_cues, dimnames = list(X$stim_names, X$cue_names))
  # stage 1
  X$stim_defs["A", "A"] <- 1
  X$stim_defs["C", "C"] <- 1
  X$stim_defs["E", "E"] <- 1
  X$stim_defs["G", "G"] <- 1
  X$stim_defs["I", "I"] <- 1
  X$stim_defs["L", "L"] <- 1
  X$stim_defs["O", "O"] <- 1
  X$stim_defs["R", "R"] <- 1
  # stage 2
  X$stim_defs["A + B", c("A", "B")] <- 1
  X$stim_defs["C + D", c("C", "D")] <- 1
  X$stim_defs["E + F", c("E", "F")] <- 1
  X$stim_defs["G + H", c("G", "H")] <- 1
  X$stim_defs["J + K", c("J", "K")] <- 1
  X$stim_defs["M + N", c("M", "N")] <- 1
  X$stim_defs["P + Q", c("P", "Q")] <- 1
  X$stim_defs["S + T", c("S", "T")] <- 1
  # stage 3
  X$stim_defs["B + K", c("B", "K")] <- 1
  X$stim_defs["D + N", c("D", "N")] <- 1
  X$stim_defs["F + Q", c("F", "Q")] <- 1
  X$stim_defs["H + T", c("H", "T")] <- 1
  # test stage
  X$stim_defs["B + T", c("B", "T")] <- 1
  X$stim_defs["D + Q", c("D", "Q")] <- 1
  X$stim_defs["F + N", c("F", "N")] <- 1
  X$stim_defs["H + K", c("H", "K")] <- 1
  X$stim_defs["B + N", c("B", "N")] <- 1
  X$stim_defs["D + K", c("D", "K")] <- 1
  X$stim_defs["F + T", c("F", "T")] <- 1
  X$stim_defs["H + Q", c("H", "Q")] <- 1

  X$stim_seq <- vector()
  for(i in 1:n_blocks[1]) {
    X$stim_seq <- c(X$stim_seq, sample(stage1_names))
  }
  for(i in 1:n_blocks[2]) {
    X$stim_seq <- c(X$stim_seq, sample(stage2_names))
  }
  for(i in 1:n_blocks[3]) {
    X$stim_seq <- c(X$stim_seq, sample(stage3_names))
  }
  for(i in 1:n_test_reps) {
    X$stim_seq <- c(X$stim_seq, test_stim_names)
  }

  X$stage <- c(rep("stage 1", n_blocks[1]*8),
               rep("stage 2", n_blocks[2]*8),
               rep("stage 3", n_blocks[3]*4),
               rep("test", n_test_reps*8))

  X
}
