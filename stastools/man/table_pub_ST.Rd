\name{table_pub_ST}
\alias{table_pub_ST}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{table_pub
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
table_pub_ST(data, row, col, propdir, frname)
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
    if (class(data) != "data.frame") 
        data <- as.data.frame(data)
    tmptable <- with(data, table(row, col))
    tmpprop <- prop.table(tmptable, propdir)
    tmp1 <- as.data.frame.matrix(tmptable)
    tmp2 <- as.data.frame.matrix(tmpprop)
    factornum <- length(tmp1)
    a <- cbind(tmp1, tmp2)
    b <- colnames(a)
    oldlen <- length(a)
    for (i in 1:factornum) {
        a[, length(a) + 1] <- paste0(a[, i], "(", round(a[, factornum + 
            i] * 100, 2), "\%", ")")
    }
    newlen <- length(a)
    newtmp <- a[, (oldlen + 1):newlen]
    colnames(newtmp) <- b[1:(length(b)/2)]
    headna <- c(rep(NA, factornum))
    rname <- c(frname, rownames(newtmp))
    result <- rbind(headna, newtmp)
    rownames(result) <- rname
    options(warn = 2)
    chi <- try(test <- chisq.test(tmp1))
    if ("try-error" \%in\% class(chi)) 
        chi <- fisher.test(tmp1, simulate.p.value = T)
    pval <- round(chi$p.value, 3)
    pdata <- data.frame(p.value = rep(NA, length(rownames(result))), 
        Method = rep(NA, length(rownames(result))))
    rownames(pdata) <- rownames(result)
    pdata[1, 1] <- pval
    pdata[1, 2] <- chi$method
    options(warn = 0)
    result <- cbind(result, pdata)
    char <- data.frame(char = rownames(result))
    result <- cbind(char, result)
    return(result)
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
