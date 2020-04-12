cor_effect_oldnew <- function(n_blocks = c(14, 4), pairs = "old", n_test_reps = 1) {
  X <- list(); class(X) <- "learnExp"
  X$n_test_reps <- n_test_reps # This determines the number of times the set of test trials is given (it's only relevant for simulating responses).
  X$action_names <- c("cat 1", "cat 2", "cat 3", "cat 4")
  X$stim_names <- c("A + V", "B + V", "A + W", "B + W", "C + X", "D + X", "C + Y", "D + Y")
  if(pairs == "old") {
    X$stim_names <- c(X$stim_names, c("E + F", "G + H", "I + J", "K + L"))
  }
  else if(pairs == "new") {
    X$stim_names <- c(X$stim_names, c("A + X", "B + Y", "C + V", "D + W", "E + F", "G + H", "I + J", "K + L"))
  }
  X$stim_names <- c(X$stim_names, c("A + C", "B + D", "V + X", "W + Y", "E + H", "F + G", "I + J", "K + L"))
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

  X$fb_function <- function(stim_vector, stim_seq, trial, stage) {
    fb <- rep(0, length(X$action_names))
    names(fb) <- X$action_names
    if(stage == "stage 1") {
      if(stim_vector["A"] == 1 | stim_vector["D"] == 1) {
        fb["cat 1"] <- 1
      }
      else if(stim_vector["B"] == 1 | stim_vector["C"] == 1) {
        fb["cat 2"] <- 1
      }
    }
    else if(stage == "stage 2") {
      if(stim_vector["A"] == 1 | stim_vector["C"] == 1 | stim_vector["E"] == 1 | stim_vector["I"] == 1) {
        fb["cat 3"] <- 1
      }
      else if(stim_vector["B"] == 1 | stim_vector["D"] == 1 | stim_vector["G"] == 1 | stim_vector["K"] == 1) {
        fb["cat 4"] <- 1
      }
    }

    return(fb)
  }

  X$stim_defs <- matrix(0, nrow = n_stimuli, ncol = n_cues, dimnames = list(X$stim_names, X$cue_names))

  X$stim_defs["A + V", c("A", "V")] <- 1
  X$stim_defs["B + V", c("B", "V")] <- 1
  X$stim_defs["A + W", c("A", "W")] <- 1
  X$stim_defs["B + W", c("B", "W")] <- 1
  X$stim_defs["C + X", c("C", "X")] <- 1
  X$stim_defs["D + X", c("D", "X")] <- 1
  X$stim_defs["C + Y", c("C", "Y")] <- 1
  X$stim_defs["D + Y", c("D", "Y")] <- 1

  if(pairs == "new") {
    X$stim_defs["A + X", c("A", "X")] <- 1
    X$stim_defs["B + Y", c("B", "Y")] <- 1
    X$stim_defs["C + V", c("C", "V")] <- 1
    X$stim_defs["D + W", c("D", "W")] <- 1
  }

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
  stage1_names <- X$stim_names[1:8]
  if(pairs == "old") {
    stage2_names <- c("A + V", "B + W", "C + X", "D + Y", "E + F", "G + H", "I + J", "K + L")
  }
  else if(pairs == "new") {
    stage2_names <- c("A + X", "B + Y", "C + V", "D + W", "E + F", "G + H", "I + J", "K + L")
  }
  test_stim_names <- c("A + C", "B + D", "V + X", "W + Y", "E + H", "F + G", "I + J", "K + L")

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
