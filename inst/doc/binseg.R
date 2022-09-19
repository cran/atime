## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
library(data.table)
atime.list <- atime::atime(
  N=2^seq(2, 20),
  setup={
    max.segs <- as.integer(N/2)
    max.changes <- max.segs-1L
    set.seed(1)
    data.vec <- 1:N
  },
  "changepoint::cpt.mean"={
    cpt.fit <- changepoint::cpt.mean(data.vec, method="BinSeg", Q=max.changes)
    sort(c(N,cpt.fit@cpts.full[max.changes,]))
  },
  "binsegRcpp::binseg_normal"={
    binseg.fit <- binsegRcpp::binseg_normal(data.vec, max.segs)
    sort(binseg.fit$splits$end)
  },
  "fpop::multiBinSeg"={
    mbs.fit <- fpop::multiBinSeg(data.vec, max.changes)
    sort(c(mbs.fit$t.est, N))
  },
  "wbs::sbs"={
    wbs.fit <- wbs::sbs(data.vec)
    split.dt <- data.table(wbs.fit$res)[order(-min.th, scale)]
    sort(split.dt[, c(N, cpt)][1:max.segs])
  },
  binsegRcpp.list={
    binseg.fit <- binsegRcpp::binseg(
      "mean_norm", data.vec, max.segs, container.str="list")
    sort(binseg.fit$splits$end)
  },
  ##seconds.limit=0.1,
  times=5)
plot(atime.list)

## -----------------------------------------------------------------------------
best.list <- atime::references_best(atime.list)
if(require(ggplot2)){
  hline.df <- with(atime.list, data.frame(seconds.limit, unit="seconds"))
  gg.both <- ggplot()+
    theme_bw()+
    facet_grid(unit ~ ., scales="free")+
    geom_hline(aes(
      yintercept=seconds.limit),
      color="grey",
      data=hline.df)+
    geom_line(aes(
      N, empirical, color=expr.name),
      data=best.list$meas)+
    geom_ribbon(aes(
      N, ymin=min, ymax=max, fill=expr.name),
      data=best.list$meas[unit=="seconds"],
      alpha=0.5)+
    scale_x_log10()+
    scale_y_log10("median line, min/max band")
  if(require(directlabels)){
    gg.both+
      directlabels::geom_dl(aes(
        N, empirical, color=expr.name, label=expr.name),
        method="right.polygons",
        data=best.list$meas)+
      theme(legend.position="none")+
      coord_cartesian(xlim=c(1,2e7))
  }else{
    gg.both
  }
}

## -----------------------------------------------------------------------------
best.refs <- best.list$ref[each.sign.rank==1]
(time.refs <- best.refs[unit=="seconds"])

## -----------------------------------------------------------------------------
ref.color <- "red"
## try() to avoid CRAN error 'from' must be a finite number, on
## Flavors: r-devel-linux-x86_64-debian-gcc, r-release-linux-x86_64,
## due to https://github.com/r-lib/scales/issues/307
(seconds.dt <- best.list$meas[unit=="seconds"])
try(if(require(ggplot2)){
  gg <- ggplot()+
    geom_line(aes(
      N, reference, group=fun.name),
      color=ref.color,
      data=time.refs)+
    geom_line(aes(
      N, empirical),
      size=1,
      data=seconds.dt)+
    scale_x_log10()+
    scale_y_log10("median line, min/max band")+
    facet_wrap("expr.name")+
    theme_bw()
  if(require(directlabels)){
    gg+
      directlabels::geom_dl(aes(
        N, reference, label=fun.name),
        data=time.refs,
        color=ref.color,
        method="bottom.polygons")
  }else{
    gg
  }
})

## -----------------------------------------------------------------------------
my.refs <- list(
  "N \\log N"=function(N)log10(N) + log10(log(N)),
  "N^2"=function(N)2*log10(N),
  "N^3"=function(N)3*log10(N))
my.best <- atime::references_best(atime.list, fun.list=my.refs)

## -----------------------------------------------------------------------------
(my.best.time.refs <- my.best$ref[unit=="seconds"])
try(if(require(ggplot2)){
  gg <- ggplot()+
    geom_line(aes(
      N, reference, group=fun.name),
      color=ref.color,
      data=my.best.time.refs)+
    geom_line(aes(
      N, empirical),
      size=1,
      data=seconds.dt)+
    scale_x_log10()+
    scale_y_log10("median line, min/max band")+
    facet_wrap("expr.name")+
    theme_bw()
  if(require(directlabels)){
    gg+
      directlabels::geom_dl(aes(
        N, reference, label=fun.name),
        data=my.best.time.refs,
        color=ref.color,
        method="bottom.polygons")
  }else{
    gg
  }
})

