\name{table2pubraw}
\alias{table2pubraw}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{table_pub
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
table2pubraw(data, row, col, propdir, frname)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{row}{
%%     ~~Describe \code{row} here~~
}
  \item{col}{
%%     ~~Describe \code{col} here~~
}
  \item{propdir}{
%%     ~~Describe \code{propdir} here~~
}
  \item{frname}{
%%     ~~Describe \code{frname} here~~
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
function (data, row, col, propdir, frname) 
{
    tmptable <- with(data, table(row, col))
    tmpprop <- prop.table(tmptable, propdir)
    tmp1 <- as.data.frame.matrix(tmptable)
    tmp2 <- as.data.frame.matrix(tmpprop)
    factornum <- length(tmp1)
    a <- cbind(tmp1, tmp2)
    b <- colnames(a)
    for (i in 1:factornum) {
        a[, length(a) + 1] <- paste0(a[, i], "(", round(a[, factornum + 
            i] * 100, 2), "\%", ")")
    }
    name1 <- paste0(colnames(tmp1), "_N")
    name2 <- paste0(colnames(tmp1), "_P")
    name3 <- paste0(colnames(tmp1), "_Pub")
    colnames(a) <- c(name1, name2, name3)
    headna <- c(rep(NA, length(colnames(a))))
    rname <- c(frname, rownames(a))
    a <- rbind(headna, a)
    rownames(a) <- rname
    char <- data.frame(char = rownames(a))
    result <- cbind(char, a)
    fac <- rep(char[1, 1], nrow(result))
    result <- cbind(fac, result)
    result <- result[-1, ]
    return(result)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
