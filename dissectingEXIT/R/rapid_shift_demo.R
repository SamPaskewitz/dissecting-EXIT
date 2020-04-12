rapid_shift_demo <- function(pars = c(0.1, 0.1, 2, 1), eta0 = c(1.2, 0.9), nTrials = 50) {
  # This runs Model 5 on a simplified task (A + X -> cat 1) for the purpose of analyzing rapid attention shifts.
  # The task represents transfer stages of e.g. correlation and blocked cue designs.
  # We don't simulate choice probabilities with this function.

  # 8/11/2019: UPDATE AND DOUBLE CHECK THIS FUNCTION.

  lambda <- pars[1]
  mu <- pars[2] # learning rate for attention (determines amount of shift retained between trials)
  rho <- pars[3] # shift rate for attention (determines rapidity of shifts within a trial)
  p <- pars[4] # determines the metric used for normalization

  nActions <- 2
  nStimuli <- 1
  nCues <- 2

  cueNames <- c("A", "X")
  actionNames <- c("cat 1", "cat 2")
  trialNames <- paste("trial", 1:nTrials)
  stepNames <- paste("step", 1:11)

  zPrime <- array(0, dim = c(nTrials, 11, nActions), dimnames = list(trialNames, stepNames, actionNames))
  deltaPrime <- array(0, dim = c(nTrials, 11, nActions), dimnames = list(trialNames, stepNames, actionNames))
  gPrime <- array(0, dim = c(nTrials, 11, nCues), dimnames = list(trialNames, stepNames, cueNames))
  aPrime <- array(0, dim = c(nTrials, 11, nCues), dimnames = list(trialNames, stepNames, cueNames))

  z <- matrix(ncol = nTrials, nrow = nActions, dimnames = list(actionNames, trialNames)) # predictions
  delta <- matrix(ncol = nTrials, nrow = nActions, dimnames = list(actionNames, trialNames)) # prediction error
  eta <- matrix(0, ncol = nTrials + 1, nrow = nCues, dimnames = list(cueNames, c(trialNames, "final")))
  a <- eta
  aNew <- eta
  eta[,1] <- eta0
  w <- array(0, c(nTrials + 1, nActions, nCues), dimnames = list(c(trialNames, "final") , actionNames, cueNames)) # association weights

  for(h in 1:nTrials) {
    s <- c(1, 1) # the stimulus is constant; both cues are present on each trial
    g <- eta[,h]*s # attention gain
    gNorm <- sum(g^p)^(1/p) # norm of attention gain vector
    a[,h] <- g/gNorm # attention weights
    z[,h] <- w[h,,] %*% (a[,h]*s) # predictions
    r <- matrix(c(1, 0), nrow = 2, ncol = 1) # the feedback vector is also constant; response 1 is always correct
    delta[,h] <- r - z[,h]

    # Rapid attention shift
    gPrime[h,1,] <- g
    zAlone <- w[h,,]%*%diag(s)
    for(j in 1:10) {
      gPrimeNorm <- sum(gPrime[h,j,]^p)^(1/p)
      aPrime[h,j,] <- gPrime[h,j,]/gPrimeNorm
      zPrime[h,j,] <- w[h,,] %*% (aPrime[h,j,]*s) # new prediction
      deltaPrime[h,j,] <- r - zPrime[h,j,] # new prediction error
      competFactor <- t(aPrime[h,j,]^(p-1))
      zPrimeDif <- zAlone - zPrime[h,j,] %*% competFactor
      shift.g <- rho * (1 / gPrimeNorm) * t(zPrimeDif) %*% deltaPrime[h,j,]
      gPrime[h,j+1,] <- gPrime[h,j,] + shift.g
      gPrime[h,j+1,][gPrime[h,j+1,] < .00001] <- .00001 # If only one cue is present and that cue has eta = 0, there's a problem.
    }
    gNew <- gPrime[h,11,] # attention gain
    gNewNorm <- sum(gNew^p)^(1/p) # norm of attention gain vector
    aNew[,h] <- gNew/gNewNorm # attention weights
    # Association learning
    change.w <- lambda * delta[,h] %*% t(aNew[,h]*s) # compute update to w
    w[h+1,,] <- w[h,,] + change.w # update w
    # Attention learning
    change.eta <- mu * (gNew - g) * s
    eta[,h+1] <- eta[,h] + change.eta
    eta[,h+1][eta[,h+1] < .00001] <- .00001 # If only one cue is present and that cue has eta = 0, there's a problem.
  }

  list(a = a, eta = eta, g = g, delta = delta, w = w, z = z)
}
