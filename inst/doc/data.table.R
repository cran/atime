## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)

## -----------------------------------------------------------------------------
#  tdir <- tempfile()
#  dir.create(tdir)
#  git2r::clone("https://github.com/Rdatatable/data.table", tdir)

## -----------------------------------------------------------------------------
#  run.atime <- function(TDIR){
#    atime::atime_versions(
#      pkg.path=TDIR,
#      pkg.edit.fun=function(old.Package, new.Package, sha, new.pkg.path){
#        pkg_find_replace <- function(glob, FIND, REPLACE){
#          atime::glob_find_replace(file.path(new.pkg.path, glob), FIND, REPLACE)
#        }
#        Package_regex <- gsub(".", "_?", old.Package, fixed=TRUE)
#        R_init_pkg <- paste0("R_init_", Package_regex)
#        Package_ <- gsub(".", "_", old.Package, fixed=TRUE)
#        new.Package_ <- paste0(Package_, "_", sha)
#        pkg_find_replace(
#          "DESCRIPTION",
#          paste0("Package:\\s+", old.Package),
#          paste("Package:", new.Package))
#        pkg_find_replace(
#          file.path("src","Makevars.*in"),
#          Package_regex,
#          new.Package_)
#        pkg_find_replace(
#          file.path("R", "onLoad.R"),
#          Package_regex,
#          new.Package_)
#        pkg_find_replace(
#          file.path("R", "onLoad.R"),
#          sprintf('packageVersion\\("%s"\\)', old.Package),
#          sprintf('packageVersion\\("%s"\\)', new.Package))
#        pkg_find_replace(
#          file.path("src", "init.c"),
#          R_init_pkg,
#          paste0("R_init_", new.Package_))
#        pkg_find_replace(
#          "NAMESPACE",
#          sprintf('useDynLib\\("?%s"?', Package_regex),
#          paste0('useDynLib(', new.Package_))
#      },
#      N = 10^seq(3, 8),
#      setup={
#        n <- N/100
#        set.seed(1L)
#        dt <- data.table(
#          g = sample(seq_len(n), N, TRUE),
#          x = runif(N),
#          key = "g")
#      },
#      expr={
#        dt_mod <- copy(dt)
#        data.table:::`[.data.table`(dt_mod, , N := .N, by = g)
#      },
#      results = FALSE,
#      verbose = TRUE,
#      "news item tweak"="be2f72e6f5c90622fe72e1c315ca05769a9dc854",
#      "simplify duplication in memrecycle"="e793f53466d99f86e70fc2611b708ae8c601a451",
#      "1.14.0 on CRAN. Bump to 1.14.1"="263b53e50241914a22f7ba6a139b52162c9d7927",
#      "1.14.3 dev master"="c4a2085e35689a108d67dacb2f8261e4964d7e12")
#  }
#  atime.list <- if(requireNamespace("callr")){
#    requireNamespace("atime")
#    callr::r(run.atime, list(tdir))
#  }else{
#    run.atime(tdir)
#  }

## -----------------------------------------------------------------------------
#  best.list <- atime::references_best(atime.list)
#  both.dt <- best.list$meas
#  if(require(ggplot2)){
#    hline.df <- with(atime.list, data.frame(seconds.limit, unit="seconds"))
#    gg <- ggplot()+
#      theme_bw()+
#      facet_grid(unit ~ ., scales="free")+
#      geom_hline(aes(
#        yintercept=seconds.limit),
#        color="grey",
#        data=hline.df)+
#      geom_line(aes(
#        N, empirical, color=expr.name),
#        data=best.list$meas)+
#      geom_ribbon(aes(
#        N, ymin=min, ymax=max, fill=expr.name),
#        data=best.list$meas[unit=="seconds"],
#        alpha=0.5)+
#      scale_x_log10()+
#      scale_y_log10("median line, min/max band")
#    if(require(directlabels)){
#      gg+
#        directlabels::geom_dl(aes(
#          N, empirical, color=expr.name, label=expr.name),
#          method="right.polygons",
#          data=best.list$meas)+
#        theme(legend.position="none")+
#        coord_cartesian(xlim=c(1e3,1e10))
#    }else{
#      gg
#    }
#  }

## -----------------------------------------------------------------------------
#  atime::atime_versions_remove("data.table")

