\name{unicox_LHF}
\alias{unicox_LHF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{unicox_LHF
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
unicox_LHF(var, surtime, surstatus, Dat)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{var}{
%%     ~~Describe \code{var} here~~
}
  \item{surtime}{
%%     ~~Describe \code{surtime} here~~
}
  \item{surstatus}{
%%     ~~Describe \code{surstatus} here~~
}
  \item{Dat}{
%%     ~~Describe \code{Dat} here~~
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
function (var, surtime, surstatus, Dat) 
{
    sur_data <- Surv(surtime, surstatus)
    FML <- as.formula(paste0("sur_data~", var))
    uniCOX <- coxph(FML, Dat)
    sum <- summary(uniCOX)
    pvalue <- c(NA, NA)
    for (i in (1:nrow(sum$coefficients))) {
        tmp <- paste0("p", i)
        tmp <- round(sum$coefficients[i, 5], 3)
        pvalue <- c(pvalue, tmp)
    }
    LCI <- c(NA, "ref")
    for (i in (1:nrow(sum$coefficients))) {
        tmp <- paste0("con", i)
        tmp <- paste0(round(sum$conf.int[i, 3], 3))
        LCI <- c(LCI, tmp)
    }
    UCI <- c(NA, "ref")
    for (i in (1:nrow(sum$coefficients))) {
        tmp <- paste0("con", i)
        tmp <- paste0(round(sum$conf.int[i, 4], 3))
        UCI <- c(UCI, tmp)
    }
    CI <- c(NA, "ref")
    for (i in (1:nrow(sum$coefficients))) {
        tmp <- paste0("con", i)
        tmp <- paste0(round(sum$conf.int[i, 3], 2), "-", round(sum$conf.int[i, 
            4], 2))
        CI <- c(CI, tmp)
    }
    HR <- c(NA, "ref")
    for (i in (1:nrow(sum$coefficients))) {
        tmp <- paste0("HR", i)
        tmp <- round(sum$coefficients[i, 2], 3)
        HR <- c(HR, tmp)
    }
    Char <- c(var, uniCOX$xlevels[[1]])
    result <- data.frame(characteristics = Char, `Hazard Ratio` = HR, 
        LCI = LCI, UCI = UCI, CI = CI, `P value` = pvalue)
    return(result)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
