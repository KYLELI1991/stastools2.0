\name{sub_unicox}
\alias{sub_unicox}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{sub_unicox
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sub_unicox(surtime, surstatus, var, data, subfactor, subpos = "up")
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
  \item{subfactor}{
%%     ~~Describe \code{subfactor} here~~
}
  \item{subpos}{
%%     ~~Describe \code{subpos} here~~
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
function (surtime, surstatus, var, data, subfactor, subpos = "up") 
{
    sp <- split(data, data[, subfactor])
    tmp <- NULL
    submun <- NULL
    for (i in 1:length(sp)) {
        unitmp <- sigunicox(surtime = surtime, surstatus = surstatus, 
            var = var, data = sp[[i]])
        tmp <- rbind(tmp, unitmp)
        N <- nrow(sp[[i]])
        submun <- c(submun, N)
    }
    subgroup <- data.frame(Subgroup = names(sp), N = submun)
    result <- cbind(subgroup, tmp)
    result$Subgroup <- as.character(result$Subgroup)
    if (subpos == "up") {
        subhead <- c(subfactor, rep(NA, ncol(result) - 1))
        coln <- colnames(result)
        result <- rbind(subhead, result)
        colnames(result) <- coln
    }
    else if (subpos == "left") {
        subhead <- c(subfactor, rep(NA, nrow(result) - 1))
        coln <- colnames(result)
        result <- cbind(subhead, result)
        colnames(result) <- c("subcat", coln)
    }
    return(result)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
