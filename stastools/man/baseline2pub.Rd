\name{baseline2pub}
\alias{baseline2pub}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{baseline2pub
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
baseline2pub(data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
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
function (data) 
{
    if (anyNA(data)) {
        data[is.na(data)] <- "Not Availabe"
    }
    datfreq <- lapply(data, table)
    datprop <- lapply(datfreq, prop.table)
    if (F) {
        character <- c(names(datfreq[1]), names(datprop[[1]]))
        NOP <- c(NA, paste0(datfreq[[1]], "(", round(datprop[[1]] * 
            100, 2), "\%", ")"))
        tmp_base <- data.frame(character = character, N = NOP)
    }
    baseline_all <- NULL
    ncol(data)
    for (i in 1:ncol(data)) {
        character <- c(names(datfreq[i]), names(datprop[[i]]))
        NOP <- c(NA, paste0(datfreq[[i]], "(", round(datprop[[i]] * 
            100, 2), "\%", ")"))
        tmp_base <- data.frame(Character = character, N = NOP)
        baseline_all <- rbind(baseline_all, tmp_base)
    }
    return(baseline_all)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
