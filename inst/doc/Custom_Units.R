## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
gsub_replace <- function(replace,subject){
  string <- gsub("a",replace,subject)
  data.frame(string, nchar=nchar(string))
}
gsub_replace("-","foobar")
gsub_replace("--","foobar")

## -----------------------------------------------------------------------------
atime.gsub.list <- atime::atime(
  setup={
    subject <- paste(rep("a", N), collapse="")
    pattern <- paste(rep(c("a?", "a"), each=N), collapse="")
  },
  constant.replacement=gsub_replace("constant size replacement",subject),
  linear.replacement=gsub_replace(subject,subject),
  result=TRUE)
plot(atime.gsub.list)

## -----------------------------------------------------------------------------
ref.gsub.list <- atime::references_best(atime.gsub.list)
plot(ref.gsub.list)

## -----------------------------------------------------------------------------
library(data.table)
data(Mono27ac, package="PeakSegDisk", envir=environment())
setup <- quote({
  data.list <- list(real=Mono27ac$coverage[1:N])
  data.list$synthetic <- data.table(data.list$real)[, count := 1:.N]
})

## -----------------------------------------------------------------------------
penalty <- 1e6
(expr.list <- c(
  if(requireNamespace("PeakSegDisk"))atime::atime_grid(
    list(Data=c("real","synthetic")),
    FPOP={
      fit <- PeakSegDisk::PeakSegFPOP_df(data.list[[Data]], penalty)
      fit$loss[, .(intervals=mean.intervals, segments)]
    }),
  atime::atime_grid(mean={
    mean(data.list$real$count)
    data.frame(intervals=NA, segments=1)
  })
))

## -----------------------------------------------------------------------------
(atime.DP.lang <- substitute(atime::atime(
  N=as.integer(10^seq(1, 3, by=0.5)),
  setup=SETUP,
  expr.list=expr.list,
  seconds.limit=Inf,
  result=TRUE),
  list(SETUP=setup)))

## -----------------------------------------------------------------------------
atime.DP.list <- eval(atime.DP.lang)
plot(atime.DP.list)

## -----------------------------------------------------------------------------
ref.DP.list <- atime::references_best(atime.DP.list)
plot(ref.DP.list)

