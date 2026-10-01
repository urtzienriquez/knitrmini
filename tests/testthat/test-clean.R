make_build_dir <- function() {
  root <- tempfile("clean")
  dir <- file.path(root, "doc")
  dir.create(file.path(dir, "sub"), recursive = TRUE)
  files <- c(
    "doc.tex", "doc.pdf", "doc.Rnw", "doc.R", "doc.bib", "doc_diff.tex",
    "doc-labels.aux", "doc.aux", "doc.bbl", "doc.log", "doc.synctex.gz",
    "other.aux", "sub/doc.aux"
  )
  file.create(file.path(dir, files))
  file.create(file.path(root, "doc.aux"))
  writeLines(c(
    "# Fdb version 4",
    "[\"lualatex\"] 1 \"doc.tex\" \"doc.pdf\" \"doc\" 1 0",
    "  \"doc.tex\" 1 1 abc \"\"",
    "  (generated)",
    "  \"doc.aux\"",
    "  \"doc.pdf\"",
    "  \"other.aux\"",
    "  \"../doc.aux\"",
    "  \"sub/doc.aux\"",
    "  (rewritten before read)",
    "[\"biber doc\"] 1 \"doc.bcf\" \"doc.bbl\" \"doc\" 1 0",
    "  \"doc.bcf\" 1 1 abc \"lualatex\"",
    "  (generated)",
    "  \"doc.bbl\""
  ), file.path(dir, "doc.fdb_latexmk"))
  writeLines(c(
    "PWD /somewhere",
    "INPUT doc.tex",
    "OUTPUT doc.log",
    "OUTPUT doc.R",
    "OUTPUT /tmp/doc.aux"
  ), file.path(dir, "doc.fls"))
  dir
}

test_that("clean_aux removes only files recorded as generated", {
  dir <- make_build_dir()
  on.exit(unlink(dirname(dir), recursive = TRUE))

  would <- clean_aux(file.path(dir, "doc.tex"), dry_run = TRUE, quiet = TRUE)
  expected <- c("doc.aux", "doc.bbl", "doc.fdb_latexmk", "doc.fls", "doc.log",
    "doc.synctex.gz")
  expect_setequal(basename(would), expected)
  expect_true(all(file.exists(would)))

  removed <- clean_aux(file.path(dir, "doc.Rnw"), quiet = TRUE)
  expect_setequal(basename(removed), expected)
  expect_false(any(file.exists(removed)))
  kept <- c("doc.tex", "doc.pdf", "doc.Rnw", "doc.R", "doc.bib", "doc_diff.tex",
    "doc-labels.aux", "other.aux", "sub/doc.aux")
  expect_true(all(file.exists(file.path(dir, kept))))
  expect_true(file.exists(file.path(dirname(dir), "doc.aux")))
})

test_that("clean_aux removes nothing without a latexmk record", {
  dir <- tempfile("clean")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  file.create(file.path(dir, c("doc.tex", "doc.aux", "doc.log")))
  expect_message(removed <- clean_aux(file.path(dir, "doc.tex")), "nothing removed")
  expect_length(removed, 0)
  expect_true(all(file.exists(file.path(dir, c("doc.aux", "doc.log")))))
})

test_that("knit(clean = TRUE) keeps unrelated files with the same base name", {
  skip_if(Sys.which("latexmk") == "", "latexmk not available")
  dir <- tempfile("clean")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  rnw <- file.path(dir, "doc.Rnw")
  writeLines(c("\\documentclass{article}", "\\begin{document}", "Hi",
    "\\end{document}"), rnw)
  writeLines("x <- 1", file.path(dir, "doc.R"))
  knit(rnw, quiet = TRUE, clean = TRUE)
  expect_true(all(file.exists(file.path(dir, c("doc.tex", "doc.pdf", "doc.R")))))
  expect_false(any(file.exists(file.path(dir, c("doc.aux", "doc.log", "doc.fls")))))
})
