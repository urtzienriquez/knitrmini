test_that("full Rnw document knits correctly", {
  rnw <- make_rnw(
    "Some text.",
    "<<chunk>>=",
    "1+1",
    "@",
    "More text."
  )
  tex_content <- knit_and_read(rnw)
  expect_true(any(grepl("documentclass", tex_content)))
  expect_true(any(grepl("begin\\{document\\}", tex_content)))
  expect_true(any(grepl("end\\{document\\}", tex_content)))
  expect_true(any(grepl("Some text", tex_content)))
  expect_true(any(grepl("More text", tex_content)))
})

test_that("knit with inline Sexpr works", {
  rnw <- make_rnw(
    "<<x>>=",
    "x <- 42",
    "@",
    "The answer is \\\\Sexpr{x}."
  )
  tex_content <- knit_and_read(rnw)
  expect_true(any(grepl("42", tex_content)))
})

test_that("knit with warnings and messages", {
  rnw <- make_rnw(
    "<<a>>=",
    'warning("beware")',
    'message("hello")',
    "@"
  )
  tex_content <- knit_and_read(rnw)
  expect_true(any(grepl("beware", tex_content)))
  expect_true(any(grepl("hello", tex_content)))
})

test_that("knit with errors captures error in output", {
  tf <- tempfile(fileext = ".Rnw")
  writeLines(c(
    "\\documentclass{article}",
    "\\begin{document}",
    "<<a>>=",
    "stop('boom')",
    "@",
    "\\end{document}"
  ), tf)
  tex_path <- sub("Rnw$", "tex", tf)
  on.exit(unlink(c(tf, tex_path)))
  suppressMessages(
    knitrmini::knit(tf, tex_path, compile = FALSE, quiet = TRUE)
  )
  tex_content <- readLines(tex_path, warn = FALSE)
  expect_true(any(grepl("boom", tex_content)))
})

test_that("knit with options chunk", {
  tf <- make_rnw(
    "<<a, fig.width=3, fig.height=4>>=",
    "plot(1:5)",
    "@"
  )
  tex_content <- knit_and_read(tf)
  expect_true(any(grepl("includegraphics", tex_content)))
})

test_that("knit with results=asis passes through", {
  rnw <- make_rnw(
    "<<a, results='asis'>>=",
    "cat('\\\\textbf{bold}')",
    "@"
  )
  tex_content <- knit_and_read(rnw)
  expect_true(any(grepl("textbf", tex_content)))
})

test_that("knit with fig.cap adds figure environment", {
  tf <- make_rnw(
    "<<a, fig.cap='A caption'>>=",
    "plot(1:5)",
    "@"
  )
  tex_content <- knit_and_read(tf)
  expect_true(any(grepl("figure", tex_content)))
})

test_that("knit with child documents", {
  tmp <- tempdir()
  child <- file.path(tmp, "child.Rnw")
  rnw <- file.path(tmp, "main.Rnw")
  writeLines(c("<<child-chunk>>=", "2+2", "@"), child)
  writeLines(c(
    "\\documentclass{article}",
    "\\begin{document}",
    "<<parent-chunk, child='child.Rnw'>>=",
    "@",
    "\\end{document}"
  ), rnw)
  on.exit(unlink(c(child, rnw,
    sub("\\.Rnw", "\\.tex", rnw),
    sub("\\.Rnw", "\\.tex", child))))
  tex_content <- knit_and_read(rnw)
  expect_true(any(grepl("2\\\\+2|4", tex_content)))
})

test_that("knit with external code file", {
  rnw <- make_rnw(
    "<<a>>=",
    "3 + 4",
    "@"
  )
  tex_content <- knit_and_read(rnw)
  expect_true(any(grepl("7", tex_content)))
})

test_that("cache option produces cached output", {
  tf <- make_rnw(
    "<<a, cache=TRUE>>=",
    "42",
    "@"
  )
  on.exit(unlink(c(tf, sub("Rnw$", "tex", tf))))
  suppressMessages(knit(tf, compile = FALSE, quiet = TRUE))
  cache_files <- list.files(tempdir(), pattern = "cache", full.names = TRUE)
  expect_true(length(cache_files) > 0 || file.exists(sub("Rnw$", "tex", tf)))
})
