pkgs_in <- function(code) {
  sort(unique(packages_in_exprs(parse(text = code, keep.source = FALSE))))
}

test_that("packages_in_exprs finds library, require, namespaces and ::", {
  code <- c(
    "library(aa)", "library('bb')", "require(cc, quietly = TRUE)",
    "if (requireNamespace(\"dd\")) loadNamespace('ee')",
    "x <- ff::g(1)", "gg:::h", "pacman::p_load(hh, 'ii')",
    "f <- function(a = jj::k()) kk::m(a)", "(function() ll::n())()"
  )
  expect_equal(pkgs_in(code), c(
    "aa", "bb", "cc", "dd", "ee", "ff", "gg", "hh", "ii", "jj", "kk", "ll", "pacman"
  ))
})

test_that("packages_in_exprs skips variables with character.only and other calls", {
  code <- c(
    "for (p in pkgs) library(p, character.only = TRUE)",
    "library('aa', character.only = TRUE)",
    "requireNamespace(pkg)", "mylibrary(bb)", "x$library"
  )
  expect_equal(pkgs_in(code), "aa")
})

test_that("r_code_blocks reads only R chunks", {
  d <- tempfile("bib")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE))
  rnw <- file.path(d, "a.Rnw")
  writeLines(c(
    "Text with terra::rast() in prose.",
    "<<setup>>=", "library(aa)", "<<other-chunk>>", "@",
    "\\Sexpr{bb::f()}"
  ), rnw)
  expect_equal(r_code_blocks(rnw), list("library(aa)"))
  qmd <- file.path(d, "a.qmd")
  writeLines(c(
    "```{julia}", "x::Int = 1", "```",
    "```{r label}", "cc::f()", "```",
    "```{r}", "#| echo: false", "library(dd)", "```"
  ), qmd)
  expect_equal(r_code_blocks(qmd), list("cc::f()", c("#| echo: false", "library(dd)")))
})

test_that("project_packages warns on code that does not parse and keeps going", {
  d <- tempfile("bib")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE))
  writeLines("library(aa", file.path(d, "bad.R"))
  writeLines("library(bb)", file.path(d, "good.R"))
  dir.create(file.path(d, "renv"))
  writeLines("library(cc)", file.path(d, "renv", "lib.R"))
  expect_warning(pkgs <- project_packages(d), "bad.R")
  expect_equal(pkgs, "bb")
})

test_that("project_packages scans the whole git project", {
  skip_if(Sys.which("git") == "", "git not available")
  root <- tempfile("bib")
  dir.create(file.path(root, "R"), recursive = TRUE)
  dir.create(file.path(root, "doc"))
  on.exit(unlink(root, recursive = TRUE))
  system2("git", c("-C", shQuote(root), "init", "-q"))
  writeLines("library(aa)", file.path(root, "R", "a.R"))
  writeLines("library(ignored)", file.path(root, "R", "skip.R"))
  writeLines("skip.R", file.path(root, ".gitignore"))
  writeLines(c("<<>>=", "bb::f()", "@"), file.path(root, "doc", "x.Rnw"))
  expect_equal(project_packages(file.path(root, "doc")), c("aa", "bb"))
})

make_renv_project <- function() {
  root <- tempfile("renv")
  dir.create(file.path(root, "doc"), recursive = TRUE)
  writeLines("{}", file.path(root, "renv.lock"))
  root
}

renv_lib_dir <- function(root) {
  v <- getRversion()
  lib <- file.path(root, "renv", "library", "linux-test", paste0("R-", v$major, ".", v$minor), "x86_64-test")
  dir.create(lib, recursive = TRUE)
  lib
}

test_that("pkg_library uses the renv library of an inactive renv project", {
  expect_null(pkg_library(tempdir()))

  root <- make_renv_project()
  on.exit(unlink(root, recursive = TRUE))
  expect_warning(lib <- pkg_library(file.path(root, "doc")), "no renv library")
  expect_null(lib)

  lib <- renv_lib_dir(root)
  expect_equal(pkg_library(file.path(root, "doc")), c(normalizePath(lib), .Library))

  old <- Sys.getenv("RENV_PROJECT")
  on.exit(Sys.setenv(RENV_PROJECT = old), add = TRUE)
  Sys.setenv(RENV_PROJECT = normalizePath(root))
  expect_null(pkg_library(file.path(root, "doc")))
})

test_that("write_pkg_bib looks packages up in the renv library", {
  skip_if_not_installed("digest")
  root <- make_renv_project()
  on.exit(unlink(root, recursive = TRUE))
  lib <- renv_lib_dir(root)
  file.copy(system.file(package = "xfun"), lib, recursive = TRUE)
  old <- Sys.getenv("RENV_PROJECT")
  on.exit(Sys.setenv(RENV_PROJECT = old), add = TRUE)
  Sys.unsetenv("RENV_PROJECT")

  # digest is installed in the session, but not in the project's library
  res <- write_pkg_bib(path = file.path(root, "doc"), packages = c("xfun", "digest"), quiet = TRUE)
  expect_equal(res$packages, c("base", "xfun"))
  expect_equal(res$missing, "digest")
  expect_equal(res$library[1], normalizePath(lib))
})

test_that("project_packages skips renv folders", {
  skip_if(Sys.which("git") == "", "git not available")
  root <- tempfile("bib")
  dir.create(file.path(root, "renv"), recursive = TRUE)
  dir.create(file.path(root, ".renv"))
  on.exit(unlink(root, recursive = TRUE))
  system2("git", c("-C", shQuote(root), "init", "-q"))
  writeLines("requireNamespace('renvhelper')", file.path(root, "renv", "activate.R"))
  writeLines("requireNamespace('renvhelper')", file.path(root, ".renv", "activate.R"))
  writeLines("library(aa)", file.path(root, "a.R"))
  expect_equal(project_packages(root), "aa")
})

test_that("renv_project_library finds a .renv library without asking renv", {
  root <- make_renv_project()
  on.exit(unlink(root, recursive = TRUE))
  v <- getRversion()
  lib <- file.path(root, ".renv", "library", "linux-test", paste0("R-", v$major, ".", v$minor), "arch")
  dir.create(lib, recursive = TRUE)
  local_mocked_bindings(requireNamespace = function(...) FALSE, .package = "base")
  expect_equal(renv_project_library(root), lib)
})

test_that("write_pkg_bib writes R and installed packages, then skips citation()", {
  f <- tempfile(fileext = ".bib")
  on.exit(unlink(f))
  res <- write_pkg_bib(f, packages = c("utils", "xfun", "notapkg123"), quiet = TRUE)
  expect_true(res$changed)
  expect_equal(res$packages, c("base", "xfun"))
  expect_equal(res$missing, "notapkg123")
  keys <- grep("^@", readLines(f), value = TRUE)
  expect_equal(sub("^@\\w+\\{([^,]+),", "\\1", keys), c("R-base", "R-xfun"))

  calls <- 0
  local_mocked_bindings(pkg_bib = function(...) {
    calls <<- calls + 1
    stop("should not be called")
  }, .package = "xfun")
  res <- write_pkg_bib(f, packages = "xfun", quiet = TRUE)
  expect_false(res$changed)
  expect_equal(calls, 0)
})

test_that("write_pkg_bib regenerates when a version is outdated", {
  f <- tempfile(fileext = ".bib")
  on.exit(unlink(f))
  write_pkg_bib(f, packages = "xfun", cite_r = FALSE, quiet = TRUE)
  lines <- readLines(f)
  writeLines(sub("R package version [^ },]+", "R package version 0.0.1", lines), f)
  res <- write_pkg_bib(f, packages = "xfun", cite_r = FALSE, quiet = TRUE)
  expect_true(res$changed)
  expect_equal(readLines(f), lines)
})

test_that("write_pkg_bib keeps going when citation() fails for a package", {
  f <- tempfile(fileext = ".bib")
  on.exit(unlink(f))
  real <- xfun::pkg_bib
  local_mocked_bindings(pkg_bib = function(x, ...) {
    if (identical(x, "xfun")) stop("no author")
    real(x, ...)
  }, .package = "xfun")
  res <- write_pkg_bib(f, packages = "xfun", quiet = TRUE)
  expect_equal(res$failed, "xfun")
  expect_equal(res$packages, "base")
  lines <- readLines(f)
  expect_length(grep("^@", lines), 1)
  expect_equal(lines[1:2], c(
    "% generated by knitrmini",
    paste("% citation() failed: xfun", packageVersion("xfun"))
  ))

  # not retried while its version is the same
  local_mocked_bindings(pkg_bib = function(...) stop("should not be called"), .package = "xfun")
  res <- write_pkg_bib(f, packages = "xfun", quiet = TRUE)
  expect_false(res$changed)
  expect_equal(res$failed, "xfun")
})

write_test_rnw <- function(d, preamble = character()) {
  rnw <- file.path(d, "doc.Rnw")
  writeLines(c(
    "\\documentclass{article}", preamble, "\\begin{document}",
    "<<>>=", "x <- xfun::file_ext('a.R')", "@",
    "\\end{document}"
  ), rnw)
  rnw
}

bib_keys <- function(f) {
  sub("^@\\w+\\{([^,]+),", "\\1", grep("^@", readLines(f), value = TRUE))
}

test_that("knit creates packages.bib only when the document uses it", {
  d <- tempfile("bib")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE))
  on.exit(rm(list = ".knitGlobal", envir = .knitEnv), add = TRUE)
  bib <- file.path(d, "packages.bib")

  knit(write_test_rnw(d), compile = FALSE, quiet = TRUE)
  expect_false(file.exists(bib))

  knit(write_test_rnw(d, "% \\addbibresource{packages.bib}"), compile = FALSE, quiet = TRUE)
  expect_false(file.exists(bib))

  knit(write_test_rnw(d, "\\addbibresource[datatype=bibtex]{packages.bib}"),
    compile = FALSE, quiet = TRUE)
  expect_equal(bib_keys(bib), c("R-base", "R-xfun"))
  expect_equal(readLines(bib, n = 1), "% generated by knitrmini")
})

test_that("knit finds \\bibliography{} and keeps an existing file up to date", {
  d <- tempfile("bib")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE))
  on.exit(rm(list = ".knitGlobal", envir = .knitEnv), add = TRUE)
  bib <- file.path(d, "packages.bib")

  knit(write_test_rnw(d, "\\bibliography{zotero, packages}"), compile = FALSE, quiet = TRUE)
  expect_equal(bib_keys(bib), c("R-base", "R-xfun"))

  # once written, it is kept up to date even without the \bibliography line
  file.create(bib)
  knit(write_test_rnw(d), compile = FALSE, quiet = TRUE)
  expect_equal(bib_keys(bib), c("R-base", "R-xfun"))
})

test_that("knitrmini never touches a packages.bib it did not write", {
  d <- tempfile("bib")
  dir.create(d)
  on.exit(unlink(d, recursive = TRUE))
  on.exit(rm(list = ".knitGlobal", envir = .knitEnv), add = TRUE)
  bib <- file.path(d, "packages.bib")
  mine <- c("@Manual{R-mine,", "  title = {Mine},", "}")
  writeLines(mine, bib)

  expect_message(
    knit(write_test_rnw(d, "\\addbibresource{packages.bib}"), compile = FALSE),
    "not written by knitrmini"
  )
  expect_equal(readLines(bib), mine)
  expect_error(write_pkg_bib(bib, packages = "xfun", quiet = TRUE), "not overwriting")
  expect_equal(readLines(bib), mine)

  write_pkg_bib(bib, packages = "xfun", force = TRUE, quiet = TRUE)
  expect_equal(bib_keys(bib), c("R-base", "R-xfun"))
})
