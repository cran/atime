## ----setup, include = FALSE-------------------------------------------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)
options(width=120)

## ---------------------------------------------------------------------------------------------------------------------
library(Matrix)
N_seq <- as.integer(10^seq(1,7,by=0.25))
vec.mat.result <- atime::atime(
  N=N_seq,
  vector=numeric(N),
  matrix=matrix(0, N, N),
  Matrix=Matrix(0, N, N),
  result=function(x)data.frame(length=length(x)))
plot(vec.mat.result)

## ---------------------------------------------------------------------------------------------------------------------
seconds.limit <- 0.01
done.vec <- NULL
measure.vars <- c("seconds","kilobytes","length")
press_result <- bench::press(
  N = N_seq,
  {
    exprs <- function(...){
      as.list(match.call()[-1])
    }
    elist <- exprs(
      vector=numeric(N),
      matrix=matrix(0, N, N),
      Matrix=Matrix(0, N, N))
    elist[names(done.vec)] <- NA #Don't run exprs which already exceeded limit.
    mark.args <- c(elist, list(iterations=10, check=FALSE))
    mark.result <- do.call(bench::mark, mark.args)
    ## Rename some columns for easier interpretation.
    desc.vec <- attr(mark.result$expression, "description")
    mark.result$description <- desc.vec
    mark.result$seconds <- as.numeric(mark.result$median)
    mark.result$kilobytes <- as.numeric(mark.result$mem_alloc/1024)
    ## Compute length column to measure in addition to time/memory.
    mark.result$length <- NA
    for(desc.i in seq_along(desc.vec)){
      description <- desc.vec[[desc.i]]
      result <- eval(elist[[description]])
      mark.result$length[desc.i] <- length(result)
    }
    ## Set NA time/memory/length for exprs which were not run.
    mark.result[desc.vec %in% names(done.vec), measure.vars] <- NA
    ## If expr went over time limit, indicate it is done.
    over.limit <- mark.result$seconds > seconds.limit
    over.desc <- desc.vec[is.finite(mark.result$seconds) & over.limit]
    done.vec[over.desc] <<- TRUE
    mark.result
  }
)

## ---------------------------------------------------------------------------------------------------------------------
library(data.table)
(press_long <- melt(
  data.table(press_result),
  measure.vars=measure.vars,
  id.vars=c("N","description"),
  na.rm=TRUE))
if(require(ggplot2)){
  gg <- ggplot()+
    ggtitle("bench::press results for comparison")+
    facet_grid(variable ~ ., labeller=label_both, scales="free")+
    geom_line(aes(
      N, value,
      color=description),
      data=press_long)+
    scale_x_log10(limits=c(NA, max(press_long$N*2)))+
    scale_y_log10("")
  if(requireNamespace("directlabels")){
    directlabels::direct.label(gg,"right.polygons")
  }else gg
}

## ---------------------------------------------------------------------------------------------------------------------
vec.mat.best <- atime::references_best(vec.mat.result)
plot(vec.mat.best)

## ---------------------------------------------------------------------------------------------------------------------
vec.mat.pred <- predict(
  vec.mat.best,
  seconds=vec.mat.result$seconds.limit,
  kilobytes=1000,
  length=1e6)
plot(vec.mat.pred)

## ---------------------------------------------------------------------------------------------------------------------
library(data.table)
dcast(vec.mat.pred$prediction[
, ratio := N[expr.name=="Matrix"]/N, by=unit
], unit + unit.value ~ expr.name, value.var="ratio")

## ---------------------------------------------------------------------------------------------------------------------
library(Matrix)
sparse.prop <- 0.9
dense.prop <- 1-sparse.prop
mult.result <- atime::atime(
  N=as.integer(10^seq(1,4,by=0.25)),
  setup={
    m <- matrix(0, N, N)
    set.seed(1)
    w <- rnorm(N)
    N.not.zero <- as.integer(dense.prop*N^2)
    m[sample(N^2, N.not.zero)] <- rnorm(N.not.zero)
    M <- Matrix(m)
  },
  sparse = M %*% w,
  dense = m %*% w,
  result=TRUE)
plot(mult.result)

## ---------------------------------------------------------------------------------------------------------------------
mult.best <- atime::references_best(mult.result)
plot(mult.best)

## ---------------------------------------------------------------------------------------------------------------------
library(data.table)
mult.compare <- dcast(
  mult.result$measurements, N ~ expr.name, value.var="result"
)[
, equal := paste(all.equal(as.numeric(dense[[1]]), as.numeric(sparse[[1]])))
, by=N
][]
tibble::tibble(mult.compare)

## ---------------------------------------------------------------------------------------------------------------------
library(Matrix)
mult.result <- atime::atime(
  N=as.integer(10^seq(1,4,by=0.25)),
  setup={
    m <- matrix(0, N, N)
    set.seed(1)
    w <- rnorm(N)
    N.not.zero <- N
    m[sample(N^2, N.not.zero)] <- rnorm(N.not.zero)
    M <- Matrix(m)
  },
  sparse = M %*% w,
  dense = m %*% w,
  result=TRUE)
plot(mult.result)

## ---------------------------------------------------------------------------------------------------------------------
mult.best <- atime::references_best(mult.result)
plot(mult.best)

## ---------------------------------------------------------------------------------------------------------------------
library(data.table)
mult.compare <- dcast(
  mult.result$measurements, N ~ expr.name, value.var="result"
)[
, equal := paste(all.equal(as.numeric(dense[[1]]), as.numeric(sparse[[1]])))
, by=N
][]
tibble::tibble(mult.compare)

## ---------------------------------------------------------------------------------------------------------------------
param.list <- list(
  non_zeros=c("N","N^2/10"),
  fun=c("matrix","Matrix")
)

## ---------------------------------------------------------------------------------------------------------------------
(expr.list <- atime::atime_grid(
  param.list,
  Mw=L[[fun]][[non_zeros]]%*%w,
  collapse="\n"))

## ---------------------------------------------------------------------------------------------------------------------
mult.result <- atime::atime(
  N=as.integer(10^seq(1,3.5,by=0.25)),
  setup={
    L <- list()
    set.seed(1)
    w <- rnorm(N)
    for(non_zeros in param.list$non_zeros){
      N.not.zero <- as.integer(eval(str2lang(non_zeros)))
      m <- matrix(0, N, N)
      m[sample(N^2, N.not.zero)] <- rnorm(N.not.zero)
      for(fun in param.list$fun){
        L[[fun]][[non_zeros]] <- get(fun)(as.numeric(m), N, N)
      }
    }
  },
  expr.list=expr.list)
plot(mult.result)

## ---------------------------------------------------------------------------------------------------------------------
mult.best <- atime::references_best(mult.result)
plot(mult.best)

## ---------------------------------------------------------------------------------------------------------------------
only.seconds <- mult.best
only.seconds$measurements <- mult.best$measurements[unit=="seconds"]
only.seconds$plot.references <- mult.best$plot.references[unit=="seconds"]
if(require(ggplot2)){
  plot(only.seconds)+
    facet_grid(non_zeros ~ fun, labeller=label_both)
}

