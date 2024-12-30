\name{mean2table_wilcox.test}
\alias{mean2table_wilcox.test}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{mean2table_wilcox.test
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
mean2table_wilcox.test(data, compare_row, compare_group)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{compare_row}{
%%     ~~Describe \code{compare_row} here~~
}
  \item{compare_group}{
%%     ~~Describe \code{compare_group} here~~
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
mean2table_wilcox.test <-
  function(data,compare_row,compare_group){
    tmp_test <- t.test(data[,compare_row] ~ data[,compare_group])
    tmp_wilcox <- wilcox.test(data[,compare_row] ~ data[,compare_group])
    
    tmp_p <- tmp_wilcox$p.value
    tmp_ci <- tmp_test$conf.int
    tmp_mean <- tmp_test$estimate
    
    tmp <- data.frame("char" = compare_row,
                      "A" = tmp_mean[[1]],
                      "B" = tmp_mean[[2]],
                      "LCI" = tmp_ci[[1]],"UCI" = tmp_ci[[2]],
                      "CI" = paste0(signif(tmp_ci[1],2),"-",signif(tmp_ci[2],2)),
                      "pval" = signif(tmp_p))
    colnames(tmp) <- c("char",names(tmp_mean),"LCI","UCI","CI","pval")
    return(tmp)
    
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
