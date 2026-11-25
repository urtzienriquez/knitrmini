# serve(): a worker that knits on request, each knit as in a new R but for the packages loaded.

test_that("serve knits on request, each in an empty global environment, and answers", {
  d <- withr::local_tempdir()
  writeLines(c("\\documentclass{article}", "\\begin{document}",
    "<<a>>=", "if (exists('leftover')) stop('not a new R')", "leftover <- 1", "options(digits = 2)", "@",
    "Pi: \\Sexpr{pi}.", "\\end{document}"), file.path(d, "doc.Rnw"))
  input <- textConnection(c(
    "knit\tdoc.Rnw\tout1.tex\t0\t",
    "knit\tdoc.Rnw\tout2.tex\t0\t", # (again: no leftover, digits as at the start)
    "knit\tmissing.Rnw\tbad.tex\t0\t", # (an error: answered, and the next knit still made)
    "nonsense",
    "quit",
    "knit\tdoc.Rnw\tnever.tex\t0\t"
  ))
  answers <- character()
  output <- textConnection("answers", "w", local = TRUE)
  withr::with_dir(d, {
    old <- getOption("digits")
    capture.output(serve(input, output))
    expect_equal(getOption("digits"), old)
  })
  close(output)
  done <- grep("^\001done", answers, value = TRUE)
  expect_equal(done[1:2], c("\001done\tok", "\001done\tok"))
  expect_match(done[3], "^\001done\terror\t")
  expect_match(done[4], "^\001done\terror\tunknown command")
  expect_length(done, 4)
  expect_match(paste(readLines(file.path(d, "out2.tex")), collapse = "\n"), "Pi: 3.14", fixed = TRUE)
  expect_false(file.exists(file.path(d, "never.tex")))
})
