## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -------------------------------------------------------------------------------------------------
old.opt <- options(width=100)
data(Mono27ac, package="PeakSegDisk", envir=environment())
library(data.table)
penalty <- 1e6
atime.list <- atime::atime(
  N=as.integer(10^seq(1, 3, by=0.5)),
  setup={
    some.cov <- Mono27ac$coverage[1:N]
    some.inc <- data.table(some.cov)
    some.inc[, count := 1:.N]
  },
  seconds.limit=Inf,
  real={
    PeakSegDisk::PeakSegFPOP_df(some.cov, penalty)
  },
  synthetic={
    PeakSegDisk::PeakSegFPOP_df(some.inc, penalty)
  })
plot(atime.list)

## -------------------------------------------------------------------------------------------------
atime.list$measurements[, intervals := sapply(result, function(L)L$loss$mean.intervals)]
best.list <- atime::references_best(atime.list, more.units="intervals")
plot(best.list)

## -------------------------------------------------------------------------------------------------
if(require(ggplot2)){
  gg <- ggplot()+
    theme_bw()+
    facet_grid(unit ~ ., scales="free")+
    geom_line(aes(
      N, empirical, color=expr.name),
      data=best.list$meas)+
    geom_ribbon(aes(
      N, ymin=min, ymax=max, fill=expr.name),
      data=best.list$meas[unit=="seconds"],
      alpha=0.5)+
    scale_x_log10()+
    scale_y_log10("median line, min/max band")
  (gg.show <- if(require(directlabels)){
    gg+
      directlabels::geom_dl(aes(
        N, empirical, color=expr.name, label=expr.name),
        method="right.polygons",
        data=best.list$meas)+
      theme(legend.position="none")+
      coord_cartesian(xlim=c(5,3000))
  }else{
    gg
  })
}

## -------------------------------------------------------------------------------------------------
best.ref <- best.list$ref[each.sign.rank==1]
if(require(ggplot2)){
  gg.ref <- gg.show+
    geom_line(aes(
      N, reference, group=paste(fun.name, expr.name)),
      color="grey",
      data=best.ref)
  if(require(directlabels)){
    gg.ref+
      directlabels::geom_dl(aes(
        N, reference,
        label.group=paste(fun.name, expr.name),
        label=fun.name),
        data=best.ref,
        color="grey",
        method="left.polygons")
  }else{
    gg.ref
  }
}

## -----------------------------------------------------------------------------
options(old.opt)

