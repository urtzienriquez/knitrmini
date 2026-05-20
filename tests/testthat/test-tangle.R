library(knitrmini)

make_rnw <- function(text) {
  tmp <- tempfile(fileext = ".Rnw")
  writeLines(text, tmp)
  tmp
}

test_that("tangle extracts code from a simple Rnw", {
  f <- make_rnw(c(
    "\\documentclass{article}",
    "\\begin{document}",
    "Some text.",
    "<<test>>=",
    "x <- 1",
    "y <- 2",
    "@",
    "More text.",
    "\\end{document}"
  ))
  out <- tangle(f, quiet = TRUE)
  result <- readLines(out)
  expect_match(result[1], "## ----test----")
  expect_match(result[2], "x <- 1", fixed = TRUE)
  expect_match(result[3], "y <- 2", fixed = TRUE)
})

test_that("tangle respects tangle=FALSE chunk option", {
  f <- make_rnw(c(
    "\\documentclass{article}",
    "\\begin{document}",
    "<<keep, tangle=TRUE>>=",
    "a <- 1",
    "@",
    "<<skip, tangle=FALSE>>=",
    "b <- 2",
    "@",
    "\\end{document}"
  ))
  out <- tangle(f, quiet = TRUE)
  result <- readLines(out)
  expect_true(any(grepl("a <- 1", result, fixed = TRUE)))
  expect_false(any(grepl("b <- 2", result, fixed = TRUE)))
})

test_that("eval=FALSE comments out code with #", {
  f <- make_rnw(c(
    "\\documentclass{article}",
    "\\begin{document}",
    "<<chunk, eval=FALSE>>=",
    "x <- 1",
    "@",
    "\\end{document}"
  ))
  out <- tangle(f, quiet = TRUE)
  result <- readLines(out)
  expect_match(result[2], "# x <- 1", fixed = TRUE)
})

test_that("error=TRUE wraps code in try({})", {
  f <- make_rnw(c(
    "\\documentclass{article}",
    "\\begin{document}",
    "<<chunk, error=TRUE>>=",
    "x <- 1",
    "y <- 2",
    "@",
    "\\end{document}"
  ))
  out <- tangle(f, quiet = TRUE)
  result <- readLines(out)
  expect_match(result[2], "try({", fixed = TRUE)
  expect_match(result[3], "x <- 1", fixed = TRUE)
  expect_match(result[4], "y <- 2", fixed = TRUE)
  expect_match(result[5], "})", fixed = TRUE)
})

test_that("non-R engine code is commented out", {
  f <- make_rnw(c(
    "\\documentclass{article}",
    "\\begin{document}",
    "<<chunk, engine='python'>>=",
    "print('hello')",
    "@",
    "\\end{document}"
  ))
  out <- tangle(f, quiet = TRUE)
  result <- readLines(out)
  expect_match(result[1], "# print", fixed = TRUE)
})

test_that("documentation=0 gives bare code without headers", {
  f <- make_rnw(c(
    "\\documentclass{article}",
    "\\begin{document}",
    "<<chunk>>=",
    "x <- 1",
    "@",
    "\\end{document}"
  ))
  out <- tangle(f, documentation = 0, quiet = TRUE)
  result <- readLines(out)
  expect_false(any(grepl("^## ----", result)))
  expect_true(any(grepl("x <- 1", result, fixed = TRUE)))
})

test_that("documentation=2 wraps text as roxygen comments", {
  f <- make_rnw(c(
    "\\documentclass{article}",
    "\\begin{document}",
    "This is text before a chunk.",
    "<<chunk>>=",
    "x <- 1",
    "@",
    "Text after the chunk.",
    "\\end{document}"
  ))
  out <- tangle(f, documentation = 2, quiet = TRUE)
  result <- readLines(out)
  expect_true(any(grepl("#' This is text before", result, fixed = TRUE)))
  expect_true(any(grepl("## ----chunk----", result, fixed = TRUE)))
  expect_true(any(grepl("x <- 1", result, fixed = TRUE)))
  expect_true(any(grepl("#' Text after", result, fixed = TRUE)))
})

test_that("knitr.tangle.inline includes inline code", {
  f <- make_rnw(c(
    "\\documentclass{article}",
    "\\begin{document}",
    "Some text \\Sexpr{1+1} here.",
    "<<chunk>>=",
    "x <- 1",
    "@",
    "\\end{document}"
  ))
  o <- getOption("knitr.tangle.inline")
  options(knitr.tangle.inline = TRUE)
  on.exit(options(knitr.tangle.inline = o))
  out <- tangle(f, quiet = TRUE)
  result <- readLines(out)
  expect_true(any(grepl("1+1", result, fixed = TRUE)))
})

test_that("knit_child in inline code is evaluated", {
  child <- make_rnw(c(
    "\\documentclass{article}",
    "\\begin{document}",
    "<<child-chunk>>=",
    "child_x <- 1",
    "@",
    "\\end{document}"
  ))
  on.exit(unlink(child), add = TRUE)

  parent <- make_rnw(c(
    "\\documentclass{article}",
    "\\begin{document}",
    sprintf("\\Sexpr{knit_child('%s')}", basename(child)),
    "<<parent-chunk>>=",
    "parent_x <- 2",
    "@",
    "\\end{document}"
  ))
  on.exit(unlink(parent), add = TRUE)

  out <- tangle(parent, quiet = TRUE)
  result <- readLines(out)
  expect_true(any(grepl("child_x <- 1", result, fixed = TRUE)))
  expect_true(any(grepl("parent_x <- 2", result, fixed = TRUE)))
})

test_that("child document via child chunk option is tangled", {
  child <- make_rnw(c(
    "\\documentclass{article}",
    "\\begin{document}",
    "<<child-chunk>>=",
    "child_x <- 1",
    "@",
    "\\end{document}"
  ))
  on.exit(unlink(child), add = TRUE)

  parent <- make_rnw(c(
    "\\documentclass{article}",
    "\\begin{document}",
    sprintf("<<parent, child='%s'>>=", basename(child)),
    "@",
    "\\end{document}"
  ))
  on.exit(unlink(parent), add = TRUE)

  out <- tangle(parent, quiet = TRUE)
  result <- readLines(out)
  expect_true(any(grepl("child_x <- 1", result, fixed = TRUE)))
})

test_that("tangle auto-determines .R output from .Rnw", {
  f <- make_rnw(c(
    "\\documentclass{article}",
    "\\begin{document}",
    "<<chunk>>=",
    "x <- 1",
    "@",
    "\\end{document}"
  ))
  out <- tangle(f, quiet = TRUE)
  expect_equal(tolower(xfun::file_ext(out)), "r")
})

test_that("tangle handles multiple chunks", {
  f <- make_rnw(c(
    "\\documentclass{article}",
    "\\begin{document}",
    "<<chunk1>>=",
    "x <- 1",
    "@",
    "Some text.",
    "<<chunk2>>=",
    "y <- 2",
    "@",
    "\\end{document}"
  ))
  out <- tangle(f, quiet = TRUE)
  result <- readLines(out)
  expect_true(any(grepl("x <- 1", result, fixed = TRUE)))
  expect_true(any(grepl("y <- 2", result, fixed = TRUE)))
  expect_true(any(grepl("chunk1", result, fixed = TRUE)))
  expect_true(any(grepl("chunk2", result, fixed = TRUE)))
})

test_that("tangle handles empty chunks gracefully", {
  f <- make_rnw(c(
    "\\documentclass{article}",
    "\\begin{document}",
    "<<empty>>=",
    "@",
    "<<has-code>>=",
    "x <- 1",
    "@",
    "\\end{document}"
  ))
  out <- tangle(f, quiet = TRUE)
  result <- readLines(out)
  expect_true(any(grepl("x <- 1", result, fixed = TRUE)))
  expect_true(any(grepl("has-code", result, fixed = TRUE)))
})

test_that("tangle handles document with no chunks", {
  f <- make_rnw(c(
    "\\documentclass{article}",
    "\\begin{document}",
    "Just text, no code.",
    "\\end{document}"
  ))
  out <- tangle(f, quiet = TRUE)
  result <- readLines(out)
  expect_true(length(result) == 0L || (length(result) == 1L && result == ""))
})

test_that("tangle() with text parameter returns character output", {
  result <- tangle(text = c(
    "\\documentclass{article}",
    "\\begin{document}",
    "<<chunk>>=",
    "x <- 1",
    "@",
    "\\end{document}"
  ), quiet = TRUE)
  expect_type(result, "character")
  expect_true(any(grepl("x <- 1", result, fixed = TRUE)))
})
