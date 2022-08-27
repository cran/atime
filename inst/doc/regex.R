## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## -----------------------------------------------------------------------------
max.N <- 20
atime.list <- atime::atime(
  N=1:max.N,
  setup={
    subject <- paste(rep("a", N), collapse="")
    pattern <- paste(rep(c("(a)?", "\\1"), each=N), collapse="")
  },
  ICU=stringi::stri_match(subject, regex=pattern),
  PCRE=regexpr(pattern, subject, perl=TRUE),
  TRE=regexpr(pattern, subject, perl=FALSE)
)
plot(atime.list)

if(require(ggplot2)){
  gg <- ggplot()+
    geom_ribbon(aes(
      N, ymin=min, ymax=max, fill=expr.name),
      data=atime.list$meas,
      alpha=0.5)+
    geom_line(aes(
      N, median, color=expr.name),
      data=atime.list$meas)+
    scale_x_log10()+
    scale_y_log10("seconds (median line, min/max band)")
  if(require("directlabels")){
    direct.label(gg, "top.polygons")
  }else{
    gg
  }
}

## -----------------------------------------------------------------------------
atime.list <- atime::atime(
  ICU=stringi::stri_match(subject, regex=pattern),
  RE2=re2::re2_match(subject, pattern),
  PCRE=regexpr(pattern, subject, perl=TRUE),
  TRE=regexpr(pattern, subject, perl=FALSE),
  setup={
    subject <- paste(rep("a", N), collapse="")
    pattern <- paste(rep(c("a?", "a"), each=N), collapse="")
  },
  N=1:25,
  times=3)
plot(atime.list)

best.list <- atime::references_best(atime.list)
if(require(ggplot2)){
  gg <- ggplot()+
    geom_ribbon(aes(
      N, ymin=min, ymax=max, fill=expr.name),
      data=best.list$meas[unit=="seconds"],
      alpha=0.5)+
    geom_line(aes(
      N, empirical, color=expr.name),
      data=best.list$meas)+
    facet_grid(unit ~ ., scales="free")+
    theme(legend.position="none")+
    scale_x_log10()+
    scale_y_log10("median line, min/max band")
  if(require("directlabels")){
    gg+
      geom_dl(aes(
        N, empirical, color=expr.name, label=expr.class),
        method="right.polygons",
        data=best.list$meas)+
      coord_cartesian(xlim=c(1,30))
  }else{
    gg
  }
}

