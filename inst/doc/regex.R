## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
(subject.size.vec <- unique(as.integer(10^seq(0,3.5,l=100))))
(backtrackers <- c(
  if(requireNamespace("stringi"))atime::atime_grid(
    ICU=stringi::stri_match(subject, regex=pattern)),
  atime::atime_grid(
    PCRE=regexpr(pattern, subject, perl=TRUE),
    TRE=regexpr(pattern, subject, perl=FALSE))))
backtrackers.result <- atime::atime(
  N=subject.size.vec,
  setup={
    subject <- paste(rep("a", N), collapse="")
    pattern <- paste(rep(c("(a)?", "\\1"), each=N), collapse="")
  },
  expr.list=backtrackers)
backtrackers.best <- atime::references_best(backtrackers.result)
plot(backtrackers.best)

## -----------------------------------------------------------------------------
all.exprs <- c(
  if(requireNamespace("re2"))atime::atime_grid(
    RE2=re2::re2_match(subject, pattern)),
  backtrackers)
all.result <- atime::atime(
  N=subject.size.vec,
  setup={
    subject <- paste(rep("a", N), collapse="")
    pattern <- paste(rep(c("a?", "a"), each=N), collapse="")
  },
  expr.list=all.exprs)
all.best <- atime::references_best(all.result)
plot(all.best)

## -----------------------------------------------------------------------------
(all.pred <- predict(all.best))
summary(all.pred)

## -----------------------------------------------------------------------------
plot(all.pred)

## -----------------------------------------------------------------------------
nc.exprs <- atime::atime_grid(
  list(ENGINE=c(
    if(requireNamespace("re2"))"RE2",
    "PCRE",
    if(requireNamespace("stringi"))"ICU")),
  nc=nc::capture_first_vec(subject, pattern, engine=ENGINE))
nc.result <- atime::atime(
  N=subject.size.vec,
  setup={
    rep.collapse <- function(chr)paste(rep(chr, N), collapse="")
    subject <- rep.collapse("a")
    pattern <- list(maybe=rep.collapse("a?"), rep.collapse("a"))
  },
  expr.list=nc.exprs)
nc.best <- atime::references_best(nc.result)
plot(nc.best)

