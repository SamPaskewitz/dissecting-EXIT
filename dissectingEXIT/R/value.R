value <- function(n_blocks = c(15, 15), n_test_reps = 1, ab_rwd = c(1, .01), cd_rwd = c(.02, .01), stage2_rwd = c(1, 0)) {
  # Adapted from Le Pelley Mitchell and Johnson, 2013.
  # Represents our MTurk experiments.
  X <- list(); class(X) <- "learnExp"
  X$n_test_reps <- n_test_reps # This determines the number of times the set of test trials is given (it's only relevant for simulating responses).
  X$action_names <- c("cat 1", "cat 2", "cat 3", "cat 4")
  stage1_names <- c("A + X", "A + Y", "B + X", "B + Y", "C + X", "C + Y", "D + X", "D + Y")
  stage2_names <- c("A + D", "B + C", "E + F", "G + H")
  test_stim_names <- c("A + C", "B + D")
  X$stim_names <- c(stage1_names, stage2_names, test_stim_names)
  X$cue_names <- c("A", "B", "C", "D", "E", "F", "G", "H", "X", "Y")
  X$stim_types <- c(rep("training", 12), rep("test", 2))

  n_actions <- length(X$action_names)
  n_stimuli <- length(X$stim_names)
  n_cues <- length(X$cue_names)

  X$elg_function <- function(stage) {
    if(stage == "stage 1") {
      elg <- c(1, 1, 0, 0)
    }
    if(stage == "stage 2") {
      elg <- c(0, 0, 1, 1)
    }
    if(stage == "test") {
      elg <- rep(0, 4)
    }

    elg
  }

  X$avail_function <- function(stage) {
    if(stage == "stage 1") {
      avail <- c(1, 1, 0, 0)
    }
    if(stage %in% c("stage 2", "test")) {
      avail <- c(0, 0, 1, 1)
    }

    avail
  }

  fb_matrix <- matrix(0, nrow = n_stimuli, ncol = n_actions, dimnames = list(X$stim_names, X$action_names))
  # stage 1
  fb_matrix[c("A + X", "A + Y"), "cat 1"] <- ab_rwd[1]
  fb_matrix[c("A + X", "A + Y"), "cat 2"] <- ab_rwd[2]
  fb_matrix[c("B + X", "B + Y"), "cat 1"] <- ab_rwd[2]
  fb_matrix[c("B + X", "B + Y"), "cat 2"] <- ab_rwd[1]
  fb_matrix[c("C + X", "C + Y"), "cat 1"] <- cd_rwd[1]
  fb_matrix[c("C + X", "C + Y"), "cat 2"] <- cd_rwd[2]
  fb_matrix[c("D + X", "D + Y"), "cat 1"] <- cd_rwd[2]
  fb_matrix[c("D + X", "D + Y"), "cat 2"] <- cd_rwd[1]
  # stage 2
  fb_matrix[c("A + D", "E + F"), "cat 3"] <- stage2_rwd[1]
  fb_matrix[c("A + D", "E + F"), "cat 4"] <- stage2_rwd[2]
  fb_matrix[c("B + C", "G + H"), "cat 3"] <- stage2_rwd[2]
  fb_matrix[c("B + C", "G + H"), "cat 4"] <- stage2_rwd[1]

  X$fb_function <- function(stim_vector, stim_seq, trial, stage) {
    fb_matrix[stim_seq[trial],]
  }

  X$stim_defs <- matrix(0, nrow = n_stimuli, ncol = n_cues, dimnames = list(X$stim_names, X$cue_names))
  # stage 1
  X$stim_defs["A + X", c("A", "X")] <- 1
  X$stim_defs["A + Y", c("A", "Y")] <- 1
  X$stim_defs["B + X", c("B", "X")] <- 1
  X$stim_defs["B + Y", c("B", "Y")] <- 1
  X$stim_defs["C + X", c("C", "X")] <- 1
  X$stim_defs["C + Y", c("C", "Y")] <- 1
  X$stim_defs["D + X", c("D", "X")] <- 1
  X$stim_defs["D + Y", c("D", "Y")] <- 1
  # stage 2
  X$stim_defs["A + D", c("A", "D")] <- 1
  X$stim_defs["B + C", c("B", "C")] <- 1
  X$stim_defs["E + F", c("E", "F")] <- 1
  X$stim_defs["G + H", c("G", "H")] <- 1
  # test stage
  X$stim_defs["A + C", c("A", "C")] <- 1
  X$stim_defs["B + D", c("B", "D")] <- 1

  X$stim_seq <- vector()
  for(i in 1:n_blocks[1]) {
    X$stim_seq <- c(X$stim_seq, sample(stage1_names))
  }
  for(i in 1:n_blocks[2]) {
    X$stim_seq <- c(X$stim_seq, sample(stage2_names))
  }
  for(i in 1:n_test_reps) {
    X$stim_seq <- c(X$stim_seq, test_stim_names)
  }

  X$stage <- c(rep("stage 1", n_blocks[1]*8),
               rep("stage 2", n_blocks[2]*4),
               rep("test", n_test_reps*2))

  X
}
