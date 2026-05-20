make_rnw <- function(...) {
  body <- paste0(c(...), collapse = "\n")
  preamble <- c(
    "\\documentclass{article}",
    "\\begin{document}"
  )
  tf <- tempfile(fileext = ".Rnw")
  writeLines(c(preamble, body, "\\end{document}"), tf)
  tf
}

read_tex <- function(rnw_path) {
  tex_path <- sub("Rnw$", "tex", rnw_path)
  if (!file.exists(tex_path) || file.info(tex_path)$size == 0)
    return(character())
  readLines(tex_path, warn = FALSE)
}

knit_and_read <- function(rnw_path, ...) {
  tex_path <- sub("Rnw$", "tex", rnw_path)
  suppressMessages(
    knitrmini::knit(rnw_path, tex_path, compile = FALSE, quiet = TRUE)
  )
  readLines(tex_path, warn = FALSE)
}

skip_if_no_tikz <- function() {
  skip_if_not_installed("tikzDevice")
}

skip_if_no_highr <- function() {
  skip_if_not_installed("highr")
}

skip_if_no_digest <- function() {
  skip_if_not_installed("digest")
}
