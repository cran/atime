## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
atime.list <- atime::atime(
  N=2^seq(1, 20),
  setup={
    set.seed(1)
    data.vec <- rnorm(N)
  },
  "cumstats::cummedian"={
    cumstats::cummedian(data.vec)
  },
  "binsegRcpp::cum_median"={
    binsegRcpp::cum_median(data.vec)
  },
  times=5)
plot(atime.list)

## -----------------------------------------------------------------------------
(best.list <- atime::references_best(atime.list))
(ref.dt <- best.list$ref[each.sign.rank==1])
library(data.table)
## try() to avoid CRAN error 'from' must be a finite number, on
## https://www.stats.ox.ac.uk/pub/bdr/Rblas/README.txt, due to
## https://github.com/r-lib/scales/issues/307
try(if(require(ggplot2)){
  hline.df <- with(atime.list, data.frame(seconds.limit, unit="seconds"))
  gg <- ggplot()+
    theme_bw()+
    facet_grid(unit ~ ., scales="free")+
    geom_line(aes(
      N, reference, group=paste(expr.name, fun.name)),
      color="grey",
      data=ref.dt)+
    geom_hline(aes(
      yintercept=seconds.limit),
      color="grey",
      data=hline.df)+
    geom_line(aes(
      N, empirical, color=expr.name),
      data=best.list$measurements)+
    geom_ribbon(aes(
      N, ymin=min, ymax=max, fill=expr.name),
      data=best.list$measurements[unit=="seconds"],
      alpha=0.5)+
    scale_x_log10()+
    scale_y_log10("median line, min/max band")+
    coord_cartesian(xlim=c(1,1e7))
  if(require("directlabels")){
    gg+
      theme(legend.position="none")+
      directlabels::geom_dl(aes(
        N, empirical, color=expr.name, label=expr.name),
        data=best.list$meas,
        method="last.polygons")+
      directlabels::geom_dl(aes(
        N, reference, label.group=paste(expr.name, fun.name), label=fun.name),
        data=ref.dt,
        color="grey",
        method="bottom.polygons")
  }else{
    gg
  }
})

## -----------------------------------------------------------------------------
result.wide <- dcast(atime.list$meas, N ~ expr.name, value.var="result")
result.complete <- result.wide[sapply(`cumstats::cummedian`, length) == N]
result.complete[, identical(`binsegRcpp::cum_median`,`cumstats::cummedian`)]

