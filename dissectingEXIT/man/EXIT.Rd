\name{EXIT}
\alias{EXIT}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simulates a learning model (EXIT) with competitive attention, rapid attention shifts and exemplar-mediation of attention.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
EXIT(experiment, pars = c(0.1, 0.1, 2, 1, 0.5, 5), w0 = 0, v0 = 0)
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
  \item{v0}{
%%     ~~Describe \code{v0} here~~
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
function (experiment, pars = c(0.1, 0.1, 2, 1, 0.5, 5), w0 = 0,
    v0 = 0)
{
    startTime <- Sys.time()
    lambda <- pars[1]
    mu <- pars[2]
    rho <- pars[3]
    p <- pars[4]
    C <- pars[5]
    phi <- pars[6]
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
    psi <- experiment$stimDefs
    calcMatrix <- matrix(1, nrow = nStimuli, ncol = 1)
    v <- array(0, c(nTrials + 1, nCues, nStimuli), dimnames = list(paste("trial",
        1:(nTrials + 1)), cueNames, experiment$stimNames))
    v[1, , ] <- v0
    w <- array(0, c(nTrials + 1, nActions, nCues), dimnames = list(paste("trial",
        1:(nTrials + 1)), experiment$actionNames, cueNames))
    w[1, , ] <- w0
    a <- matrix(nrow = nTrials, ncol = nCues, dimnames = list(paste("trial",
        1:nTrials), cueNames))
    aNew <- a
    for (h in 1:nTrials) {
        s <- experiment$stimDefs[stimSeq[h], ]
        avail[h, ] <- experiment$actionAvail(s, stimSeq, h)
        distance <- rowSums(abs(psi - calcMatrix \%*\% s))
        u <- exp(-C * distance)
        eta <- exp(v[h, , ] \%*\% u)
        g <- s * eta
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
        shift.g <- vector()
        gNew <- g
        zNew <- z
        deltaNew[, h] <- delta[, h]
        aNew[h, ] <- a[h, ]
        for (j in 1:10) {
            zAlone <- w[h, , ] \%*\% diag(s)
            competFactor <- matrix(aNew[h, ]^(p - 1), nrow = 1,
                ncol = nCues)
            shift.g <- zAlone - z \%*\% competFactor
            shift.g <- deltaNew[, h] \%*\% shift.g
            shift.g <- t(rho * (1/gNorm) * shift.g)
            gNew <- gNew + shift.g
            gNew[gNew < 1e-05] <- 1e-05
            gNorm <- sum(gNew^p)^(1/p)
            aNew[h, ] <- gNew/gNorm
            zNew <- w[h, , ] \%*\% (aNew[h, ] * s)
            deltaNew[, h] <- matrix(eligibility[h, ] * (r[h,
                ] - zNew), ncol = 1)
        }
        change.w <- lambda * deltaNew[, h] \%*\% t(aNew[h, ] *
            s)
        w[h + 1, , ] <- w[h, , ] + change.w
        change.v <- mu * matrix((gNew - g) * g, nrow = nCues,
            ncol = 1) \%*\% u
        v[h + 1, , ] <- v[h, , ] + change.v
    }
    vTest <- v[dim(v)[1], , ]
    wTest <- w[dim(w)[1], , ]
    for (h in (nTrials + 1):(nTrials + nTest)) {
        s <- experiment$testStimuli[stimSeq[h], ]
        avail[h, ] <- experiment$testActionAvail(s, stimSeq,
            h)
        distance <- rowSums(abs(psi - calcMatrix \%*\% s))
        u <- exp(-C * distance)
        eta <- exp(vTest \%*\% u)
        g <- eta * s
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
    simData <- list(delta = t(delta), deltaNew = t(deltaNew),
        v = v, a = a, aNew = aNew, w = w, choiceProbs = choiceProbs,
        testProbs = testProbs)
    class(simData) <- "simData"
    endTime <- Sys.time()
    output <- list(obsvData = obsvData, simData = simData, simTime = endTime -
        startTime)
    output
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
