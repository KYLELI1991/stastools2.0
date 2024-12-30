\name{sub_cox}
\alias{sub_cox}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{sub_cox
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
sub_cox(surtime, surstatus, var, data, sub, subpos = "up")
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
  \item{sub}{
%%     ~~Describe \code{sub} here~~
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
function (surtime, surstatus, var, data, sub, subpos = "up") 
{
    res <- NULL
    for (i in 1:length(sub)) {
        tmp <- sub_unicox(surtime = surtime, surstatus = surstatus, 
            var = var, data = data, subfactor = sub[i], subpos)
        res <- rbind(res, tmp)
    }
    return(res)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
