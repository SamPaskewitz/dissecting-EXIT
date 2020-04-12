model2 <- function(experiment, pars = c(0.1, 0.1, 5, 0.5), w0 = 0, a_max = 1) {
  start_time <- Sys.time()

  # Model parameters
  lambda <- pars[1]
  mu <- pars[2] # learning rate for attention (determines amount of shift retained between trials)
  phi <- pars[3]
  a0 <- pars[4]

  # Information from the experiment list
  n_actions <- length(experiment$action_names)
  n_stimuli <- length(experiment$stim_names)
  n_test_types <- nrow(experiment$test_stimuli)
  n_test <- n_test_types * experiment$n_testReps
  n_cues <- length(experiment$cue_names)
  n_trials <- length(experiment$stim_seq)

  stim_seq <- experiment$stim_seq
  stage <- experiment$stage
  cue_names <- experiment$cue_names
  action_names <- experiment$action_names
  trial_names <- paste("trial", 1:n_trials)

  # Setup arrays and matrices etc. to keep track of simulation information.
  s <- matrix(ncol = n_trials, nrow = n_cues, dimnames = list(cue_names, trial_names))
  choice_probs <- matrix(nrow = n_actions, ncol = n_trials, dimnames = list(action_names, trial_names)) # response probabilities
  avail <- choice_probs # action availability
  elg <- choice_probs # eligibility for learning
  r <- choice_probs # feedback
  z <- choice_probs # predictions
  delta <- choice_probs # prediction error

  a <- matrix(0, ncol = n_trials + 1, nrow = n_cues, dimnames = list(cue_names, c(trial_names, "final"))) # attention
  a[,1] <- a0 # initial values of a (defaults to 0.5)
  w <- array(0, c(n_trials + 1, n_actions, n_cues), dimnames = list(c(trial_names, "final") , experiment$action_names, cue_names)) # association weights
  w[1,,] <- w0 # initial values of w (defaults to 0)

  for(h in 1:n_trials) {
    # Prediction
    s[,h] <- experiment$stim_defs[stim_seq[h],]
    avail[,h] <- experiment$avail_function(stage[h]) # available actions
    z[,h] <- avail[,h] * w[h,,] %*% (a[,h]*s[,h]) # prediction
    # Response and feedback
    choice_probs[,h] <- (avail[,h]*exp(z[,h]*phi))/sum(avail[,h]*exp(z[,h]*phi)) # calculate choice probabilities
    r[,h] <- experiment$fb_function(s[,h], stim_seq, h, stage[h]) # feedback
    elg[,h] <- experiment$elg_function(stage[h]) # eligibility (which actions can be learned about)
    delta[,h] <- elg[,h]*(r[,h] - z[,h]) # compute prediction error
    # Association learning
    change_w <- lambda * delta[,h] %*% t(a[,h]*s[,h]) # compute update to w
    w[h+1,,] <- w[h,,] + change_w # update w
    # Attention learning
    z_alone <- avail[,h] * w[h,,] %*% diag(s[,h]) # predictions based on single cues
    change_a <- mu * t(z_alone) %*% delta[,h]
    a[,h+1] <- a[,h] + change_a
    a[a[,h+1] < 0, h+1] <- 0
    a[a[,h+1] > a_max, h+1] <- a_max
  }

  # Package response probabilities for test stimuli in a convenient way
  test_probs <- choice_probs[,stage == "test"]
  colnames(test_probs) <- stim_seq[stage == "test"]

  # Assemble data for output
  obsv_data <- list(stage = stage, stim_seq = stim_seq, r = r, avail = avail, elg = elg) # observable data
  class(obsv_data) <- "obsv_data"
  sim_data <- list(s = s, a = a[,1:n_trials], w = w[1:n_trials,,], z = z, delta = delta, choice_probs = choice_probs, test_probs = test_probs) # simulation data
  class(sim_data) <- "sim_data"
  end_time <- Sys.time()
  output <- list(obsv_data = obsv_data, sim_data = sim_data, sim_time = end_time - start_time)

  output
}
