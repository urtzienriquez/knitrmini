.onLoad <- function(libname, pkgname) {
  opts_knit$set(progress = interactive())
}

.onAttach <- function(libname, pkgname) {
  if (interactive()) {
    packageStartupMessage("knitrmini v0.1.0 -- Minimal knitr for Rnw -> TeX -> PDF")
  }
}
