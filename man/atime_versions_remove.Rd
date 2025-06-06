\name{atime_versions_remove}
\alias{atime_versions_remove}
\title{Remove packages installed by atime}
\description{\code{atime_versions_exprs} installs different git versions
  of a package, and this function removes them. This can be useful if
  you have changed compilers, which may have different performance
  characteristics, so you can then re-compile all package versions using
  the same compiler.}
\usage{atime_versions_remove(Package)}
\arguments{
  \item{Package}{Name of package without SHA.}
}
\details{The library searched is the first on \code{.libPaths()}.}
\value{integer exit status code from unlink, non-zero if removal failed.}
\author{Toby Dylan Hocking}

