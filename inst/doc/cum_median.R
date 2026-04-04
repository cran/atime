## copied from library(cumstats), thanks @TimTaylor, https://github.com/tdhock/atime/issues/89
cummedian <- function(x) sapply(seq_along(x), function(k, z) median(z[1:k]), z = x)
expr.list <- c(
  if(requireNamespace("binsegRcpp"))atime::atime_grid(
    "binsegRcpp::cum_median"=binsegRcpp::cum_median(data.vec)),
  atime::atime_grid(
    cumsum=cumsum(data.vec),
    cummedian=cummedian(data.vec)))
atime.list <- atime::atime(
  N=2^seq(1, 20),
  setup={
    set.seed(1)
    data.vec <- rnorm(N)
  },
  result=TRUE,
  expr.list=expr.list,
  times=5)
plot(atime.list)

(best.list <- atime::references_best(atime.list))
## try() to avoid CRAN error 'from' must be a finite number, on
## https://www.stats.ox.ac.uk/pub/bdr/Rblas/README.txt, due to
## https://github.com/r-lib/scales/issues/307
plot(best.list)

