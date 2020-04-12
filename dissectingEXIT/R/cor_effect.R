cor_effect <- function(n_blocks = c(14, 4), n_test_reps = 1) {
  # Based on Le Pelley and McLaren, 2003.  Tests whether predictiveness in stage 1 affects associability in stage 2.
  X <- list(); class(X) <- "learnExp"
  X$n_test_reps <- n_test_reps # This determines the number of times the set of test trials is given (it's only relevant for simulating responses).
  X$action_names <- c("cat 1", "cat 2", "cat 3", "cat 4")
  stage1_names <- c("A + V", "B + V", "A + W", "B + W", "C + X", "D + X", "C + Y", "D + Y")
  stage2_names <- c("A + X", "B + Y", "C + V", "D + W", "E + F", "G + H", "I + J", "K + L")
  test_stim_names <- c("A + C", "B + D", "V + X", "W + Y", "E + H", "F + G", "I + J", "K + L")
  X$stim_names <- c(stage1_names, stage2_names, test_stim_names)
  X$cue_names <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "V", "W", "X", "Y")
  X$stim_types <- c(rep("training", 16), rep("test", 8))

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
  fb_matrix["A + V", "cat 1"] <- 1
  fb_matrix["B + V", "cat 2"] <- 1
  fb_matrix["A + W", "cat 1"] <- 1
  fb_matrix["B + W", "cat 2"] <- 1
  fb_matrix["C + X", "cat 2"] <- 1
  fb_matrix["D + X", "cat 1"] <- 1
  fb_matrix["C + Y", "cat 2"] <- 1
  fb_matrix["D + Y", "cat 1"] <- 1
  # stage 2
  fb_matrix["A + X", "cat 3"] <- 1
  fb_matrix["B + Y", "cat 4"] <- 1
  fb_matrix["C + V", "cat 3"] <- 1
  fb_matrix["D + W", "cat 4"] <- 1
  fb_matrix["E + F", "cat 3"] <- 1
  fb_matrix["G + H", "cat 4"] <- 1
  fb_matrix["I + J", "cat 3"] <- 1
  fb_matrix["K + L", "cat 4"] <- 1

  X$fb_function <- function(stim_vector, stim_seq, trial, stage) {
    fb_matrix[stim_seq[trial],]
  }

  X$stim_defs <- matrix(0, nrow = n_stimuli, ncol = n_cues, dimnames = list(X$stim_names, X$cue_names))
  # stage 1
  X$stim_defs["A + V", c("A", "V")] <- 1
  X$stim_defs["B + V", c("B", "V")] <- 1
  X$stim_defs["A + W", c("A", "W")] <- 1
  X$stim_defs["B + W", c("B", "W")] <- 1
  X$stim_defs["C + X", c("C", "X")] <- 1
  X$stim_defs["D + X", c("D", "X")] <- 1
  X$stim_defs["C + Y", c("C", "Y")] <- 1
  X$stim_defs["D + Y", c("D", "Y")] <- 1
  # stage 2
  X$stim_defs["A + X", c("A", "X")] <- 1
  X$stim_defs["B + Y", c("B", "Y")] <- 1
  X$stim_defs["C + V", c("C", "V")] <- 1
  X$stim_defs["D + W", c("D", "W")] <- 1
  X$stim_defs["E + F", c("E", "F")] <- 1
  X$stim_defs["G + H", c("G", "H")] <- 1
  X$stim_defs["I + J", c("I", "J")] <- 1
  X$stim_defs["K + L", c("K", "L")] <- 1
  # test stage
  X$stim_defs["A + C", c("A", "C")] <- 1
  X$stim_defs["B + D", c("B", "D")] <- 1
  X$stim_defs["V + X", c("V", "X")] <- 1
  X$stim_defs["W + Y", c("W", "Y")] <- 1
  X$stim_defs["E + H", c("E", "H")] <- 1
  X$stim_defs["F + G", c("F", "G")] <- 1
  X$stim_defs["I + J", c("I", "J")] <- 1
  X$stim_defs["K + L", c("K", "L")] <- 1

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
               rep("stage 2", n_blocks[2]*8),
               rep("test", n_test_reps*8))

  X
}
