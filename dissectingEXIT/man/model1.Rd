\name{model1}
\alias{model1}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Simulates a simple learning model that lacks attention learning mechanisms.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
model1(experiment, pars = c(0.1, 5), w0 = 0, a0 = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{experiment}{
A list describing a simulated experiment.
}
  \item{pars}{
%%     ~~Describe \code{pars} here~~
}
  \item{w0}{
%%     ~~Describe \code{w0} here~~
}
  \item{a0}{
%%     ~~Describe \code{a0} here~~
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
function (experiment, pars = c(0.1, 5), w0 = 0, a0 = 1)
{
    startTime <- Sys.time()
    lambda <- pars[1]
    phi <- pars[2]
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
    a <- a0
    w <- array(0, c(nTrials + 1, nActions, nCues), dimnames = list(paste("trial",
        1:(nTrials + 1)), experiment$actionNames, cueNames))
    w[1, , ] <- w0
    for (h in 1:nTrials) {
        s <- experiment$stimDefs[stimSeq[h], ]
        avail[h, ] <- experiment$actionAvail(s, stimSeq, h)
        z <- w[h, , ] \%*\% (a * s)
        choiceProbs[h, ] <- (avail[h, ] * exp(z * phi))/sum(avail[h,
            ] * exp(z * phi))
        choices[h] <- sample(1:nActions, size = 1, prob = choiceProbs[h,
            ])
        r[h, ] <- experiment$FBfunction(s, stimSeq, h)
        eligibility[h, ] <- experiment$eligibilityFunction(choices[h],
            h) * avail[h, ]
        delta[, h] <- matrix(eligibility[h, ] * (r[h, ] - z),
            ncol = 1)
        change.w <- lambda * delta[, h] \%*\% t(a * s)
        w[h + 1, , ] <- w[h, , ] + change.w
    }
    r[(nTrials + 1):(nTrials + nTest), ] <- 0
    eligibility[(nTrials + 1):(nTrials + nTest), ] <- 0
    wTest <- w[nTrials + 1, , ]
    for (h in (nTrials + 1):(nTrials + nTest)) {
        s <- experiment$testStimuli[stimSeq[h], ]
        avail[h, ] <- experiment$testActionAvail(s, stimSeq,
            h)
        z <- wTest \%*\% (a * s)
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
        avail = avail, eligibility = eligibility)
    class(obsvData) <- "obsvData"
    simData <- list(delta = t(delta), a = a0, w = w, choiceProbs = choiceProbs,
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
