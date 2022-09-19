## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = TRUE
)

## -------------------------------------------------------------------------------------------------
old.opt <- options(width=100)
pkg.path <- tempfile()
dir.create(pkg.path)
git2r::clone("https://github.com/tdhock/binsegRcpp", pkg.path)

## -------------------------------------------------------------------------------------------------
tmp.lib.path <- tempfile()
dir.create(tmp.lib.path)
lib.path.vec <- c(tmp.lib.path, .libPaths())
.libPaths(lib.path.vec)

## -------------------------------------------------------------------------------------------------
run.atime.versions <- function(PKG.PATH, LIB.PATH){
  if(!missing(LIB.PATH)).libPaths(LIB.PATH)
  atime::atime_versions(
    pkg.path=PKG.PATH,
    N=2^seq(2, 20),
    setup={
      max.segs <- as.integer(N/2)
      data.vec <- 1:N
    },
    expr=binsegRcpp::binseg_normal(data.vec, max.segs),
    cv="908b77c411bc7f4fcbcf53759245e738ae724c3e",
    "rm unord map"="dcd0808f52b0b9858352106cc7852e36d7f5b15d",
    "mvl_construct"="5942af606641428315b0e63c7da331c4cd44c091")
}

## -------------------------------------------------------------------------------------------------
atime.ver.list <- if(requireNamespace("callr")){
  requireNamespace("atime")
  callr::r(run.atime.versions, list(pkg.path, lib.path.vec))
}else{
  run.atime.versions(pkg.path)
}
names(atime.ver.list$measurements)
atime.ver.list$measurements[, .(N, expr.name, min, median, max, kilobytes)]

## -------------------------------------------------------------------------------------------------
best.ver.list <- atime::references_best(atime.ver.list)
names(best.ver.list$measurements)
best.ver.list$measurements[, .(N, expr.name, unit, empirical)]

## -------------------------------------------------------------------------------------------------
if(require(ggplot2)){
  hline.df <- with(atime.ver.list, data.frame(seconds.limit, unit="seconds"))
  gg <- ggplot()+
    theme_bw()+
    facet_grid(unit ~ ., scales="free")+
    geom_hline(aes(
      yintercept=seconds.limit),
      color="grey",
      data=hline.df)+
    geom_line(aes(
      N, empirical, color=expr.name),
      data=best.ver.list$meas)+
    geom_ribbon(aes(
      N, ymin=min, ymax=max, fill=expr.name),
      data=best.ver.list$meas[unit=="seconds"],
      alpha=0.5)+
    scale_x_log10()+
    scale_y_log10("median line, min/max band")
  if(require(directlabels)){
    gg+
      directlabels::geom_dl(aes(
        N, empirical, color=expr.name, label=expr.name),
        method="right.polygons",
        data=best.ver.list$meas)+
      theme(legend.position="none")+
      coord_cartesian(xlim=c(1,2e7))
  }else{
    gg
  }
}

## -------------------------------------------------------------------------------------------------
(ver.list <- atime::atime_versions_exprs(
  pkg.path=pkg.path,
  expr=binsegRcpp::binseg_normal(data.vec, max.segs),
  cv="908b77c411bc7f4fcbcf53759245e738ae724c3e",
  "rm unord map"="dcd0808f52b0b9858352106cc7852e36d7f5b15d",
  "mvl_construct"="5942af606641428315b0e63c7da331c4cd44c091"))

## -------------------------------------------------------------------------------------------------
expr.list <- c(ver.list, if(requireNamespace("changepoint")){
  list(changepoint=substitute(changepoint::cpt.mean(
    data.vec, penalty="Manual", pen.value=0, method="BinSeg",
    Q=max.segs-1)))
})

## -------------------------------------------------------------------------------------------------
run.atime <- function(ELIST, LIB.PATH){
  if(!missing(LIB.PATH)).libPaths(LIB.PATH)
  atime::atime(
    N=2^seq(2, 20),
    setup={
      max.segs <- as.integer(N/2)
      data.vec <- 1:N
    },
    expr.list=ELIST)
}
atime.list <- if(requireNamespace("callr")){
  requireNamespace("atime")
  callr::r(run.atime, list(expr.list, lib.path.vec))
}else{
  run.atime(expr.list)
}

## -------------------------------------------------------------------------------------------------
atime.list$measurements[, .(N, expr.name, median, kilobytes)]

## -------------------------------------------------------------------------------------------------
refs.best <- atime::references_best(atime.list)
plot(refs.best)

## -----------------------------------------------------------------------------
atime::atime_versions_remove("binsegRcpp")
options(old.opt)

