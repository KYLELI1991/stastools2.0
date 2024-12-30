\name{sigunicox}
\alias{sigunicox}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{sigunicox
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sigunicox(surtime, surstatus, var, data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{surtime}{
%%     ~~Describe \code{surtime} here~~
}
  \item{surstatus}{
%%     ~~Describe \code{surstatus} here~~
}
  \item{var}{
%%     ~~Describe \code{var} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
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
function (surtime, surstatus, var, data) 
{
    if (length(table(data[, var])) == 2) {
        t <- grep(surtime, colnames(data), fixed = T)
        s <- grep(surstatus, colnames(data), fixed = T)
        sur_data <- Surv(time = data[, t], event = data[, s])
        FML <- as.formula(paste0("sur_data~", var))
        uniCOX <- coxph(FML, data)
        sum <- try(a <- summary(uniCOX))
        if ("try-error" \%in\% class(sum)) {
            arm <- "NA"
            HR <- "NA"
            pval <- "NA"
            CI <- "NA"
            LCI <- "NA"
            UCI <- "NA"
            result <- data.frame(Arm = arm, HR = HR, LCI = LCI, 
                UCI = UCI, CI = CI, p.value = pval)
        }
        else {
            sum <- summary(uniCOX)
            arm <- paste0(rev(uniCOX$xlevels[[1]]), collapse = " Versus ")
            HR <- round(sum$coefficients[2], 3)
            pval <- round(sum$coefficients[5], 3)
            CI <- paste0(round(sum$conf.int[3:4], 3), collapse = "-")
            LCI <- round(sum$conf.int[3], 3)
            UCI <- round(sum$conf.int[4], 3)
            result <- data.frame(Arm = arm, HR = HR, LCI = LCI, 
                UCI = UCI, CI = CI, p.value = pval)
        }
    }
    else {
        arm <- "NA"
        HR <- "NA"
        pval <- "NA"
        CI <- "NA"
        LCI <- "NA"
        UCI <- "NA"
        result <- data.frame(Arm = arm, HR = HR, LCI = LCI, UCI = UCI, 
            CI = CI, p.value = pval)
    }
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
