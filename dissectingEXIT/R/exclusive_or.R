exclusive_or <- function(n_blocks = 10, n_test_reps = 1) {
  # This is a Shepard, Hovland and Jenkins type II category structure.
  X <- list(); class(X) <- "learnExp"
  X$n_test_reps <- n_test_reps # This determines the number of times the set of test trials is given (it's only relevant for simulating responses).
  X$action_names <- c("cat 1", "cat 2")
  X$stim_names <- c("A + X + I", "A + Y + I", "B + X + I", "B + Y + I", "A + X + II", "A + Y + II", "B + X + II", "B + Y + II")
  X$cue_names <- c("A", "B", "X", "Y", "I", "II")
  X$stim_types <- rep("training", 8)

  n_actions <- length(X$action_names)
  n_stimuli <- length(X$stim_names)
  n_cues <- length(X$cue_names)

  X$elg_function <- function(stage) {
    if(stage == "training") {
      elg <- c(1, 1)
    }
    else if(stage == "test") {
      elg <- c(0, 0)
    }

    elg
  }

  X$avail_function <- function(stage) {
    avail <- c(1, 1)

    avail
  }

  fb_matrix <- matrix(0, nrow = 8, ncol = n_actions, dimnames = list(X$stim_names, X$action_names))
  # category 1 (A + X and B + Y)
  fb_matrix["A + X + I", "cat 1"] <- 1
  fb_matrix["B + Y + I", "cat 1"] <- 1
  fb_matrix["A + X + II", "cat 1"] <- 1
  fb_matrix["B + Y + II", "cat 1"] <- 1
  # category 2 (A + Y and B + X)
  fb_matrix["A + Y + I", "cat 2"] <- 1
  fb_matrix["B + X + I", "cat 2"] <- 1
  fb_matrix["A + Y + II", "cat 2"] <- 1
  fb_matrix["B + X + II", "cat 2"] <- 1

  X$fb_function <- function(stim_vector, stim_seq, trial, stage) {
    if(stage == "training") {
      fb <- fb_matrix[stim_seq[trial],]
    }
    else if(stage == "test") {
      fb <- rep(0, 2)
    }
    fb
  }

  X$stim_defs <- matrix(0, nrow = n_stimuli, ncol = n_cues, dimnames = list(X$stim_names, X$cue_names))
  X$stim_defs["A + X + I", c("A", "X", "I")] <- 1
  X$stim_defs["A + Y + I", c("A", "Y", "I")] <- 1
  X$stim_defs["B + X + I", c("B", "X", "I")] <- 1
  X$stim_defs["B + Y + I", c("B", "Y", "I")] <- 1
  X$stim_defs["A + X + II", c("A", "X", "II")] <- 1
  X$stim_defs["A + Y + II", c("A", "Y", "II")] <- 1
  X$stim_defs["B + X + II", c("B", "X", "II")] <- 1
  X$stim_defs["B + Y + II", c("B", "Y", "II")] <- 1

  X$stim_seq <- vector()
  for(i in 1:n_blocks) {
    X$stim_seq <- c(X$stim_seq, sample(X$stim_names))
  }
  for(i in 1:n_test_reps) {
    X$stim_seq <- c(X$stim_seq, X$stim_names)
  }

  X$stage <- c(rep("training", n_blocks*8),
               rep("test", n_test_reps*8))

  X
}
