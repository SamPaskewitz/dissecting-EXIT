dif_cue_use <- function(n_blocks = 10, n_test_reps = 1) {
  # This is only Experiment 1 (Experiment 2 had a similar design).
  # Originally, this ran until participants completed 4 error-free blocks.
  X <- list(); class(X) <- "learnExp"
  X$n_test_reps <- n_test_reps # This determines the number of times the set of test trials is given (it's only relevant for simulating responses).
  X$action_names <- c("cat A", "cat B")
  training_names <- c(
    "6-2",
    "6-3",
    "6-4",
    "7-3",
    "7-4",
    "7-5",
    "2-6",
    "3-6",
    "4-6",
    "3-7",
    "4-7",
    "5-7"
  )
  # For now, we only have the "critical" test stimuli (labeled W, X, Y and Z in the paper) plus the four corners.
  test_stim_names <- c("2-7 (W)", "5-6 (X)", "6-5 (Y)", "7-2 (Z)", "1-1 (top left)", "8-1 (top right)", "1-8 (bottom left)", "8-8 (bottom right)")
  X$stim_names <- c(training_names, test_stim_names)
  X$cue_names <- c("position", "size")
  X$stim_types <- c(rep("training", 12), rep("test", 8))

  n_actions <- length(X$action_names)
  n_stimuli <- length(X$stim_names)
  n_cues <- length(X$cue_names)

  X$elg_function <- function(stage) {
    if(stage == "training") {
      elg <- rep(1, 2)
    }
    if(stage == "test") {
      elg <- rep(0, 2)
    }
    elg
  }

  X$avail_function <- function(stage) {
    avail <- rep(1, 2)
    avail
  }

  fb_matrix <- matrix(0, nrow = n_stimuli, ncol = n_actions, dimnames = list(X$stim_names, X$action_names))
  fb_matrix[c("2-6", "3-6", "4-6", "7-3", "7-4", "7-5"), "cat A"] <- 1
  fb_matrix[c("3-7", "4-7", "5-7", "6-2", "6-3", "6-4"), "cat B"] <- 1
  X$fb_function <- function(stim_vector, stim_seq, trial, stage) {
    fb_matrix[stim_seq[trial],]
  }

  X$stim_defs <- matrix(0, nrow = n_stimuli, ncol = n_cues, dimnames = list(X$stim_names, X$cue_names))
  # training
  X$stim_defs["6-2", 1:2] <- c(0.6, 0.2) # stim 6-2
  X$stim_defs["6-3", 1:2] <- c(0.6, 0.3) # stim 6-3
  X$stim_defs["6-4", 1:2] <- c(0.6, 0.4) # stim 6-4
  X$stim_defs["7-3", 1:2] <- c(0.7, 0.3) # stim 7-3
  X$stim_defs["7-4", 1:2] <- c(0.7, 0.4) # stim 7-4
  X$stim_defs["7-5", 1:2] <- c(0.7, 0.5) # stim 7-5
  X$stim_defs["2-6", 1:2] <- c(0.2, 0.6) # stim 2-6
  X$stim_defs["3-6", 1:2] <- c(0.3, 0.6) # stim 3-6
  X$stim_defs["4-6", 1:2] <- c(0.4, 0.6) # stim 4-6
  X$stim_defs["3-7", 1:2] <- c(0.3, 0.7) # stim 3-7
  X$stim_defs["4-7", 1:2] <- c(0.4, 0.7) # stim 4-7
  X$stim_defs["5-7", 1:2] <- c(0.5, 0.7) # stim 5-7
  # test
  X$stim_defs["2-7 (W)", 1:2] <- c(0.2, 0.7) # 2-7 (W) empirical result: 10% Cat A
  X$stim_defs["5-6 (X)", 1:2] <- c(0.5, 0.6) # 5-6 (X) empirical result: 90% Cat A
  X$stim_defs["6-5 (Y)", 1:2] <- c(0.6, 0.5) # 6-5 (Y) empirical result: 30% Cat A
  X$stim_defs["7-2 (Z)", 1:2] <- c(0.7, 0.2) # 7-2 (Z) empirical result: 80% Cat A
  X$stim_defs["1-1 (top left)", 1:2] <- c(0.1, 0.1) # 1-1 (top left) empirical result: 50% Cat A
  X$stim_defs["8-1 (top right)", 1:2] <- c(0.8, 0.1) # 8-1 (top right) empirical result: 30% Cat A
  X$stim_defs["1-8 (bottom left)", 1:2] <- c(0.1, 0.8) # 1-8 (bottom left) empirical result: 55% Cat A
  X$stim_defs["8-8 (bottom right)", 1:2] <- c(0.8, 0.8) # 8-8 (bottom right) empirical result: 60% Cat A

  X$stim_seq <- vector()
  for(i in 1:n_blocks) {
    X$stim_seq <- c(X$stim_seq, sample(training_names))
  }
  for(i in 1:n_test_reps) {
    X$stim_seq <- c(X$stim_seq, test_stim_names)
  }
  X$stage <- c(rep("training", n_blocks*12),
               rep("test", n_test_reps*8))

  X
}
