\name{model4}
\alias{model4}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simulates a learning model with competitive attention (a.k.a. CompAct).
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
model4(experiment, pars = c(0.1, 0.1, 1, 5), w0 = 0, eta0 = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{experiment}{
%%     ~~Describe \code{experiment} here~~
}
  \item{pars}{
%%     ~~Describe \code{pars} here~~
}
  \item{w0}{
%%     ~~Describe \code{w0} here~~
}
  \item{eta0}{
%%     ~~Describe \code{eta0} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (experiment, pars = c(0.1, 0.1, 1, 5), w0 = 0, eta0 = 1)
{
    startTime <- Sys.time()
    lambda <- pars[1]
    mu <- pars[2]
    p <- pars[3]
    phi <- pars[4]
    nActions <- length(experiment$actionNames)
    nStimuli <- length(experiment$stimNames)
    nTestTypes <- nrow(experiment$testStimuli)
    nTest <- nTestTypes * experiment$nTestReps
    nCues <- length(experiment$cueNames)
    nTrials <- length(experiment$stimSeq)
    cueNames <- experiment$cueNames
    trialNames <- paste("trial", 1:nTrials)
    testTrialNames <- paste("testTrial", 1:nTest)
    choiceProbs <- matrix(nrow = nTrials + nTest, ncol = nActions,
        dimnames = list(c(trialNames, testTrialNames), experiment$actionNames))
    avail <- matrix(nrow = nTrials + nTest, ncol = nActions,
        dimnames = list(c(trialNames, testTrialNames), experiment$actionNames))
    eligibility <- matrix(nrow = nTrials + nTest, ncol = nActions,
        dimnames = list(c(trialNames, testTrialNames), experiment$actionNames))
    r <- matrix(nrow = nTrials + nTest, ncol = nActions, dimnames = list(c(trialNames,
        testTrialNames), experiment$actionNames))
    stimSeq <- c(experiment$stimSeq, rep(experiment$testStimNames,
        experiment$nTestReps))
    delta <- matrix(ncol = nTrials, nrow = nActions, dimnames = list(experiment$actionNames,
        trialNames))
    choices <- vector()
    eta <- matrix(nrow = nTrials + 1, ncol = nCues, dimnames = list(paste("trial",
        1:(nTrials + 1)), cueNames))
    change.eta <- eta
    eta[1, ] <- eta0
    w <- array(0, c(nTrials + 1, nActions, nCues), dimnames = list(paste("trial",
        1:(nTrials + 1)), experiment$actionNames, cueNames))
    w[1, , ] <- w0
    a <- matrix(nrow = nTrials, ncol = nCues, dimnames = list(paste("trial",
        1:nTrials), cueNames))
    change.w <- array(0, c(nTrials, nActions, nCues), dimnames = list(paste("trial",
        1:nTrials), experiment$actionNames, cueNames))
    for (h in 1:nTrials) {
        s <- experiment$stimDefs[stimSeq[h], ]
        avail[h, ] <- experiment$actionAvail(s, stimSeq, h)
        g <- eta[h, ] * s
        gNorm <- sum(g^p)^(1/p)
        a[h, ] <- g/gNorm
        z <- w[h, , ] \%*\% (a[h, ] * s)
        choiceProbs[h, ] <- (avail[h, ] * exp(z * phi))/sum(avail[h,
            ] * exp(z * phi))
        choices[h] <- sample(1:nActions, size = 1, prob = choiceProbs[h,
            ])
        r[h, ] <- experiment$FBfunction(experiment$stimDefs[stimSeq[h],
            ], stimSeq, h)
        eligibility[h, ] <- experiment$eligibilityFunction(choices[h],
            h) * avail[h, ]
        delta[, h] <- matrix(eligibility[h, ] * (r[h, ] - z),
            ncol = 1)
        change.w[h, , ] <- lambda * delta[, h] \%*\% t(s * a[h,
            ])
        zAlone <- w[h, , ] \%*\% diag(s)
        competFactor <- matrix(a[h, ]^(p - 1), nrow = 1, ncol = nCues)
        change.eta[h, ] <- t(mu * s * (1/gNorm) * delta[, h] \%*\%
            (zAlone - z \%*\% competFactor))
        w[h + 1, , ] <- w[h, , ] + change.w[h, , ]
        eta[h + 1, ] <- eta[h, ] + change.eta[h, ]
        eta[h + 1, ][eta[h + 1, ] < 1e-05] <- 1e-05
    }
    etaTest <- eta[nrow(eta), ]
    wTest <- w[dim(w)[1], , ]
    for (h in (nTrials + 1):(nTrials + nTest)) {
        s <- experiment$testStimuli[stimSeq[h], ]
        avail[h, ] <- experiment$testActionAvail(s, stimSeq,
            h)
        g <- etaTest * s
        gNorm <- sum(g^p)^(1/p)
        aTest <- g/gNorm
        z <- wTest \%*\% (aTest * s)
        choiceProbs[h, ] <- (avail[h, ] * exp(z * phi))/sum(avail[h,
            ] * exp(z * phi))
        choices[h] <- sample(1:nActions, size = 1, prob = choiceProbs[h,
            ])
    }
    testProbs <- matrix(choiceProbs[(nTrials + 1):(nTrials +
        nTestTypes), ], nrow = nTestTypes)
    colnames(testProbs) <- experiment$actionNames
    rownames(testProbs) <- experiment$testStimNames
    obsvData <- list(stimSeq = stimSeq, r = r, choices = choices,
        avail = avail, eligibility = eligibility, stimDefs = experiment$stimDefs)
    class(obsvData) <- "obsvData"
    simData <- list(delta = t(delta), eta = eta, change.eta = change.eta,
        a = a, w = w, change.w = change.w, choiceProbs = choiceProbs,
        testProbs = testProbs)
    class(simData) <- "simData"
    endTime <- Sys.time()
    output <- list(obsvData = obsvData, simData = simData, simTime = endTime -
        startTime, change.w = change.w)
    output
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
