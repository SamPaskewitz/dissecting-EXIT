cond_cor_effect <- function(n_blocks = c(15, 10), n_test_reps = 1, group = "c1") {
  # Based on Uengoer et al 2013, Experiment 2.
  # The default number of trials in stage 2 is the same as in the original experiment (they use "block" to mean two repetitions of the stimulus set instead of just one).
  # Instead of the preliminary stages, we just add 5 blocks to stage 1 (which had 10 blocks in their experiment).
  # Currently, this ignores the dimensional structure of the task (each of the pairs A and B, X and Y, I and II shared the same dimension).
  # The first stage essentially uses a type III category structure.
  # We use "I" to denote Context 1 and II to denote Context 2.
  X <- list(); class(X) <- "learnExp"
  X$n_test_reps <- n_test_reps # This determines the number of times the set of test trials is given (it's only relevant for simulating responses).
  X$action_names <- c("cat 1", "cat 2", "cat 3", "cat 4")
  X$stim_names <- c("A + X + I", "A + Y + I", "B + X + I", "B + Y + I", "A + X + II", "A + Y + II", "B + X + II", "B + Y + II")
  X$cue_names <- c("A", "B", "X", "Y", "I", "II")
  X$stim_types <- rep("training", 8)

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

  fb_matrix1 <- matrix(0, nrow = 8, ncol = n_actions, dimnames = list(X$stim_names, X$action_names))
  # stage 1, context I (A and B are relevant)
  fb_matrix1["A + X + I", "cat 1"] <- 1
  fb_matrix1["A + Y + I", "cat 1"] <- 1
  fb_matrix1["B + X + I", "cat 2"] <- 1
  fb_matrix1["B + Y + I", "cat 2"] <- 1
  # stage 1, context II (X and Y are relevant)
  fb_matrix1["A + X + II", "cat 1"] <- 1
  fb_matrix1["A + Y + II", "cat 2"] <- 1
  fb_matrix1["B + X + II", "cat 1"] <- 1
  fb_matrix1["B + Y + II", "cat 2"] <- 1

  if(group == "c1") {
    # stage 2, context I (group c1, intradimensional shift)
    fb_matrix2 <- matrix(0, nrow = 4, ncol = n_actions, dimnames = list(X$stim_names[1:4], X$action_names))
    fb_matrix2["A + X + I", "cat 3"] <- 1
    fb_matrix2["A + Y + I", "cat 3"] <- 1
    fb_matrix2["B + X + I", "cat 4"] <- 1
    fb_matrix2["B + Y + I", "cat 4"] <- 1
  }
  if(group == "c2") {
    # stage 2, context II (group c2, extradimensional shift)
    fb_matrix2 <- matrix(0, nrow = 4, ncol = n_actions, dimnames = list(X$stim_names[5:8], X$action_names))
    fb_matrix2["A + X + II", "cat 3"] <- 1
    fb_matrix2["A + Y + II", "cat 3"] <- 1
    fb_matrix2["B + X + II", "cat 4"] <- 1
    fb_matrix2["B + Y + II", "cat 4"] <- 1
  }

  X$fb_function <- function(stim_vector, stim_seq, trial, stage) {
    if(stage == "stage 1") {
      fb <- fb_matrix1[stim_seq[trial],]
    }
    if(stage == "stage 2") {
      fb <- fb_matrix2[stim_seq[trial],]
    }
    if(stage == "test") {
      fb <- rep(0, 4)
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
  for(i in 1:n_blocks[1]) {
    X$stim_seq <- c(X$stim_seq, sample(X$stim_names))
  }
  if(group == "c1") {
    for(i in 1:n_blocks[2]) {
      X$stim_seq <- c(X$stim_seq, sample(X$stim_names[1:4]))
    }
    for(i in 1:n_test_reps) {
      X$stim_seq <- c(X$stim_seq, X$stim_names[1:4])
    }
  }
  if(group == "c2") {
    for(i in 1:n_blocks[2]) {
      X$stim_seq <- c(X$stim_seq, sample(X$stim_names[5:8]))
    }
    for(i in 1:n_test_reps) {
      X$stim_seq <- c(X$stim_seq, X$stim_names[5:8])
    }
  }

  X$stage <- c(rep("stage 1", n_blocks[1]*8),
               rep("stage 2", n_blocks[2]*4),
               rep("test",n_test_reps*4))

  X
}
