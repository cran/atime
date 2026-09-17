library(data.table)
expr.list <- list()
if(requireNamespace("changepoint"))expr.list[["changepoint\n::cpt.mean"]] <- quote({
  cpt.fit <- changepoint::cpt.mean(data.vec, method="BinSeg", Q=max.changes)
  sort(c(N,cpt.fit@cpts.full[max.changes,]))
})
if(requireNamespace("binsegRcpp")){
  expr.list[["binsegRcpp\nmultiset"]] <- quote({
    binseg.fit <- binsegRcpp::binseg(
      "mean_norm", data.vec, max.segs, container.str="multiset")
    sort(binseg.fit$splits$end)
  })
  expr.list[["binsegRcpp\nlist"]] <- quote({
    binseg.fit <- binsegRcpp::binseg(
      "mean_norm", data.vec, max.segs, container.str="list")
    sort(binseg.fit$splits$end)
  })
}
if(requireNamespace("wbs"))expr.list[["wbs::sbs"]] <- quote({
  wbs.fit <- wbs::sbs(data.vec)
  split.dt <- data.table(wbs.fit$res)[order(-min.th, scale)]
  sort(split.dt[, c(N, cpt)][1:max.segs])
})
seg.result <- atime::atime(
  N=2^seq(2, 20),
  setup={
    max.segs <- as.integer(N/2)
    max.changes <- max.segs-1L
    set.seed(1)
    data.vec <- 1:N
  },
  expr.list=expr.list)
plot(seg.result)

seg.best <- atime::references_best(seg.result)
plot(seg.best)

(seg.pred <- predict(seg.best))
plot(seg.pred)

my.refs <- list(# names should be LaTeX math mode expressions.
  "N"=function(N)log10(N),
  "N \\log N"=function(N)log10(N) + log10(log(N)),
  "N^2"=function(N)2*log10(N),
  "N^3"=function(N)3*log10(N))
my.best <- atime::references_best(seg.result, fun.list=my.refs)
dcast(my.best$references, expr.name + fun.name ~ unit, length)

plot(my.best)

some.best <- my.best
some.best$plot.references <- my.best$ref[unit=="seconds" & fun.name %in% c("N log N","N^2","N^3")]
some.best$measurements <- my.best$meas[unit=="seconds"]
plot(some.best)

