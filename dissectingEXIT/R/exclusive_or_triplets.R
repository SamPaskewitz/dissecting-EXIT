exclusive_or_triplets <- function(n_blocks = 10, n_test_reps = 1) {
  # This is a Shepard, Hovland and Jenkins type II category structure in which cues are represented as triplets.
  X <- list(); class(X) <- "learnExp"
  X$n_test_reps <- n_test_reps # This determines the number of times the set of test trials is given (it's only relevant for simulating responses).
  X$action_names <- c("cat 1", "cat 2")
  X$stim_names <- c("A + X + I", "A + Y + I", "B + X + I", "B + Y + I", "A + X + II", "A + Y + II", "B + X + II", "B + Y + II")
  X$cue_names <- c("dim 1", "dim 2", "dim 3")
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
  #X$stim_defs["A + X + I", 1:3] <- c(-1, -1, -1)
  #X$stim_defs["A + Y + I", 1:3] <- c(-1, 1, -1)
  #X$stim_defs["B + X + I", 1:3] <- c(1, -1, -1)
  #X$stim_defs["B + Y + I", 1:3] <- c(1, 1, -1)
  #X$stim_defs["A + X + II", 1:3] <- c(-1, -1, 1)
  #X$stim_defs["A + Y + II", 1:3] <- c(-1, 1, 1)
  #X$stim_defs["B + X + II", 1:3] <- c(1, -1, 1)
  #X$stim_defs["B + Y + II", 1:3] <- c(1, 1, 1)

  X$stim_defs["A + X + I", 1:3] <- c(0, 0, 0)
  X$stim_defs["A + Y + I", 1:3] <- c(0, 1, 0)
  X$stim_defs["B + X + I", 1:3] <- c(1, 0, 0)
  X$stim_defs["B + Y + I", 1:3] <- c(1, 1, 0)
  X$stim_defs["A + X + II", 1:3] <- c(0, 0, 1)
  X$stim_defs["A + Y + II", 1:3] <- c(0, 1, 1)
  X$stim_defs["B + X + II", 1:3] <- c(1, 0, 1)
  X$stim_defs["B + Y + II", 1:3] <- c(1, 1, 1)

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
