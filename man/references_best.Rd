\name{references_best}
\alias{references_best}
\title{Best references}
\description{Compute best asymptotic references.}
\usage{references_best(L, unit.col.vec=NULL, more.units=NULL, fun.list=NULL)}
\arguments{
  \item{L}{List output from atime.}
  \item{unit.col.vec}{Named character vector of units, default NULL
    means standard units (kilobytes and seconds).}
  \item{more.units}{Named character vector of units to add to
    \code{unit.col.vec}, default NULL means nothing.}
  \item{fun.list}{List of asymptotic complexity reference functions,
    default NULL means to use package default.}
}
\value{list of class \code{"references_best"} with elements
  \code{references} (data table of references),
  \code{measurements} (data table of measurements).}

\author{Toby Dylan Hocking}

\examples{

## Example 1: polynomial and exponential time string functions.
string.result <- atime::atime(
  N=unique(as.integer(10^seq(0,3.5,l=100))),
  setup={
    subject <- paste(rep("a", N), collapse="")
    pattern <- paste(rep(c("a?", "a"), each=N), collapse="")
  },
  seconds.limit=0.001,
  PCRE.match=regexpr(pattern, subject, perl=TRUE),
  TRE.match=regexpr(pattern, subject, perl=FALSE),
  constant.replacement=gsub("a","constant size replacement",subject),
  linear.replacement=gsub("a",subject,subject))
(string.best <- atime::references_best(string.result))
## plot method shows each expr in a separate panel.
plot(string.best)
## predict method computes N for given units (default seconds limit).
string.pred = predict(string.best)
plot(string.pred)

}
