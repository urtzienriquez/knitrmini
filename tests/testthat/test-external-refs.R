test_that("brace_groups extracts balanced groups", {
  expect_equal(brace_groups("{a}{b{c}}{d}", 2), c("a", "b{c}"))
  expect_equal(brace_groups("  {x\\}y}"), "x\\}y")
  expect_equal(brace_groups("{a} b {c}"), "a")
})

test_that("read_aux_labels parses hyperref-style labels", {
  aux <- tempfile(fileext = ".aux")
  on.exit(unlink(aux))
  writeLines(c(
    "\\relax",
    "\\newlabel{fig:a}{{S2.1}{3}{A {nested} caption}{figure.caption.5}{}}",
    "\\newlabel{sec:b}{{1}{1}}",
    "\\abx@aux@cite{0}{key}"
  ), aux)
  labels <- read_aux_labels(aux)
  expect_equal(names(labels), c("fig:a", "sec:b"))
  expect_equal(brace_groups(labels[["fig:a"]], 2), c("S2.1", "3"))
  expect_equal(read_aux_labels(tempfile()), character())
})

test_that("snapshot labels: snapshot first, else the input's .aux", {
  input <- tempfile(fileext = ".Rnw")
  aux <- sub("Rnw$", "aux", input)
  snap <- label_snapshot_path(input)
  on.exit(unlink(c(aux, snap)))

  writeLines("\\newlabel{a}{{1}{1}}", aux)
  expect_equal(names(read_snapshot_labels(input)), "a")
  write_label_snapshot(c(b = "{2}{2}"), snap)
  expect_equal(names(read_snapshot_labels(input)), "b")
})

test_that("check_label_numbers warns on renumbered labels only", {
  full <- c(a = "{1}{1}", b = "{2}{5}", c = "{3}{3}")
  expect_silent(check_label_numbers(c(a = "{1}{9}", d = "{7}{1}"), full))
  expect_silent(check_label_numbers(
    c(`refsection:1` = "{2}{1}"), c(`refsection:1` = "{1}{1}")
  ))
  expect_warning(check_label_numbers(c(b = "{1}{1}"), full), "b \\(1 vs 2\\)")
})

test_that("resolve_external_refs replaces only external labels", {
  labels <- c(ext = "{S1.2}{7}{}{figure.3}{}", eq = "{4}{2}")
  doc <- paste(
    "\\label{int} Fig.~\\ref{ext}, p.~\\pageref{ext}, \\ref{int},",
    "\\eqref{eq}, \\autoref{int}"
  )
  out <- resolve_external_refs(doc, labels)
  expect_match(out, "Fig.~S1.2, p.~7, \\ref{int}, (4), \\autoref{int}", fixed = TRUE)

  expect_warning(
    out <- resolve_external_refs("\\ref{missing}", labels),
    "missing"
  )
  expect_equal(out, "\\ref{missing}")
  expect_warning(resolve_external_refs("\\cref{ext}", labels), "ext")
})

test_that("skipped children: references resolved from the last full build", {
  skip_if(Sys.which("latexmk") == "", "latexmk not available")
  dir <- tempfile("extrefs")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))
  writeLines(c(
    "See Table~\\ref{tab:b}.",
    "\\begin{equation}x\\label{eq:a}\\end{equation}"
  ), file.path(dir, "a.Rnw"))
  writeLines(c(
    "\\begin{table}\\caption{x}\\label{tab:b}\\end{table}",
    "\\begin{equation}y\\label{eq:b}\\end{equation}"
  ), file.path(dir, "b.Rnw"))
  parent <- function(eval_a, eval_b) c(
    "\\documentclass{article}",
    "\\begin{document}",
    paste0("<<a, child='a.Rnw', eval=", eval_a, ">>="), "@",
    paste0("<<b, child='b.Rnw', eval=", eval_b, ">>="), "@",
    "\\end{document}"
  )
  rnw <- file.path(dir, "index.Rnw")
  snap <- file.path(dir, "index-labels.aux")
  in_dir <- function(f) file.path(dir, f)

  # full build: writes the snapshot, named after the input
  writeLines(parent("TRUE", "TRUE"), rnw)
  knit(rnw, quiet = TRUE)
  expect_true(all(c("tab:b", "eq:a") %in% names(read_aux_labels(snap))))
  expect_true(any(grepl("\\ref{tab:b}", readLines(in_dir("index.tex")), fixed = TRUE)))

  # partial build under another output name uses the input's snapshot
  writeLines(parent("TRUE", "FALSE"), rnw)
  knit(rnw, output = in_dir("part.tex"), quiet = TRUE)
  expect_true(any(grepl("See Table~1.", readLines(in_dir("part.tex")), fixed = TRUE)))
  expect_false(any(grepl("undefined", readLines(in_dir("part.log")))))
  expect_false(file.exists(in_dir("part-labels.aux")))

  # without a snapshot, the labels of <input>.aux are used
  unlink(snap)
  knit(rnw, output = in_dir("part.tex"), quiet = TRUE)
  expect_true(any(grepl("See Table~1.", readLines(in_dir("part.tex")), fixed = TRUE)))
  expect_false(file.exists(snap))

  # a counter running on from a skipped child is reported
  writeLines(parent("FALSE", "TRUE"), rnw)
  expect_warning(
    knit(rnw, output = in_dir("bonly.tex"), quiet = TRUE),
    "eq:b \\(1 vs 2\\)"
  )
})
