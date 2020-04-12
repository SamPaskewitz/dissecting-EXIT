EXIT <- function(experiment, pars = c(0.1, 0.1, 2, 1, 0.5, 5), w0 = 0, v0 = 0) {
  start_time <- Sys.time()

  # Model parameters
  lambda <- pars[1]
  mu <- pars[2] # learning rate for attention (determines amount of shift retained between trials)
  rho <- pars[3] # shift rate for attention (determines rapidity of shifts within a trial)
  p <- pars[4]
  C <- pars[5]
  phi <- pars[6]

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
  z_new <- choice_probs # predictions after rapid shift
  delta <- choice_probs # prediction error
  delta_new <- choice_probs # prediction error after rapid shift

  psi <- experiment$stim_defs # exemplar locations
  calcMatrix <- matrix(1, nrow = n_stimuli, ncol = 1) # for subtracting the current stimulus from the exemplars
  v <- array(0, c(n_trials + 1, n_cues, n_stimuli), dimnames = list(paste("trial", 1:(n_trials + 1)), cue_names, experiment$stim_names)) # exemplar -> cue gain weights
  v[1,,] <- v0
  eta <- matrix(0, ncol = n_trials + 1, nrow = n_cues, dimnames = list(cue_names, c(trial_names, "final"))) # attention
  a <- eta
  a_new <- eta
  w <- array(0, c(n_trials + 1, n_actions, n_cues), dimnames = list(c(trial_names, "final") , experiment$action_names, cue_names)) # association weights
  w[1,,] <- w0 # initial values of w (defaults to 0)

  for(h in 1:n_trials) {
    # Prediction
    s[,h] <- experiment$stim_defs[stim_seq[h],]
    avail[,h] <- experiment$avail_function(stage[h]) # available actions
    distance <- rowSums(abs(psi - calcMatrix%*%s[,h])) # distance from stimulus to exemplars
    u <- exp(-C*distance) # stimulus similarity to exemplars
    eta[,h] <- exp(v[h,,]%*%u) # salience computed from similarity
    eta[,h][eta[,h] > 1000] <- 1000 # this cap is necessary to guard against overflow errors when C is small (eta blows up in a big way)
    eta[,h][eta[,h] < 0.01] <- 0.01
    g <- eta[,h]*s[,h] # attention gain
    g[g < 0.01] <- 0.01
    g_norm <- sum(g^p)^(1/p) # norm of attention gain vector
    a[,h] <- g/g_norm # attention weights
    z[,h] <- avail[,h] * w[h,,] %*% (a[,h]*s[,h]) # prediction
    # Response and feedback
    choice_probs[,h] <- (avail[,h]*exp(z[,h]*phi))/sum(avail[,h]*exp(z[,h]*phi)) # calculate choice probabilities
    r[,h] <- experiment$fb_function(s[,h], stim_seq, h, stage[h]) # feedback
    elg[,h] <- experiment$elg_function(stage[h]) # eligibility (which actions can be learned about)
    delta[,h] <- elg[,h]*(r[,h] - z[,h]) # compute prediction error
    # Rapid attention shift
    g_prime <- g
    z_alone <- avail[,h] * w[h,,] %*% diag(s[,h]) # predictions based on single cues
    for(j in 1:10) {
      g_prime_norm <- sum(g_prime^p)^(1/p) # norm of attention gain vector
      a_prime <- g_prime/g_prime_norm # attention weights
      z_prime <- avail[,h] * w[h,,] %*% (a_prime*s[,h]) # new prediction
      delta_prime <- elg[,h]*(r[,h] - z_prime) # new prediction error
      compet_factor <- t(a_prime^(p-1))
      z_prime_dif <- z_alone - z_prime %*% compet_factor # comparison of single cue and multiple cue predictions
      shift_g <- rho * (1 / g_prime_norm) * t(z_prime_dif) %*% delta_prime
      g_prime <- g_prime + shift_g
      g_prime[g_prime < 0.01] <- 0.01 # If only one cue is present and that cue has g = 0, there's a problem.
    }
    g_new <- g_prime # attention gain
    g_new_norm <- sum(g_new^p)^(1/p) # norm of attention gain vector
    a_new[,h] <- g_new/g_new_norm # attention weights
    z_new[,h] <- avail[,h] * w[h,,] %*% (a_new[,h]*s[,h]) # prediction with shifted attention
    delta_new[,h] <- elg[,h]*(r[,h] - z_new[,h]) # prediction error with shifted attention
    # Association learning
    change_w <- lambda * delta_new[,h] %*% t(a_new[,h]*s[,h]) # compute update to w
    w[h+1,,] <- w[h,,] + change_w # update w
    # Attention learning
    change_v <- mu * ((g_new - g)*g) %*% u
    v[h+1,,] <- v[h,,] + change_v
  }

  # Package response probabilities for test stimuli in a convenient way
  test_probs <- choice_probs[,stage == "test"]
  colnames(test_probs) <- stim_seq[stage == "test"]

  # Assemble data for output
  obsv_data <- list(stage = stage, stim_seq = stim_seq, r = r, avail = avail, elg = elg) # observable data
  class(obsv_data) <- "obsv_data"
  sim_data <- list(s = s, v = v[1:n_trials,,], eta = eta, a = a, a_new = a_new, w = w[1:n_trials,,], z = z, delta = delta, delta_new = delta_new, choice_probs = choice_probs, test_probs = test_probs) # simulation data
  class(sim_data) <- "sim_data"
  end_time <- Sys.time()
  output <- list(obsv_data = obsv_data, sim_data = sim_data, sim_time = end_time - start_time)

  output
}
