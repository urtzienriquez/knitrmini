# R Markdown: knitted like Rnoweb (the same hooks, children, label snapshot, preamble only for
# what is used), its prose made LaTeX by pandoc, the document from a few YAML keys.

skip_if_no_pandoc <- function() {
  skip_if(!nzchar(Sys.which("pandoc")), "pandoc is not installed")
}

# Writes files (name = lines) in a new folder; returns the folder.
make_files <- function(...) {
  dir <- tempfile("rmd")
  dir.create(dir)
  files <- list(...)
  for (name in names(files)) {
    dir.create(dirname(file.path(dir, name)), showWarnings = FALSE, recursive = TRUE)
    writeLines(files[[name]], file.path(dir, name))
  }
  dir
}

knit_rmd <- function(dir, name = "paper.Rmd", ...) {
  out <- file.path(dir, sub("\\.Rmd$", ".tex", name))
  suppressMessages(suppressWarnings(
    knitrmini::knit(file.path(dir, name), out, compile = FALSE, quiet = TRUE, ...)
  ))
  paste(readLines(out, warn = FALSE), collapse = "\n")
}

has <- function(text, fixed) grepl(fixed, text, fixed = TRUE)

test_that("the Markdown pattern: chunks with braces, inline `r ...`", {
  p <- all_patterns$md
  expect_equal(knitrmini:::detect_pattern("", "rmd"), "md")
  expect_equal(knitrmini:::detect_pattern("", "qmd"), "md")
  expect_true(grepl(p$chunk.begin, "```{r label, echo=FALSE}"))
  expect_true(grepl(p$chunk.begin, "```{r}"))
  expect_true(grepl(p$chunk.begin, "  ```{python}"))
  expect_false(grepl(p$chunk.begin, "```r"))
  expect_false(grepl(p$chunk.begin, "```"))
  expect_true(grepl(p$chunk.end, "```"))
  expect_true(grepl(p$inline.code, "value `r 1 + 1` here", perl = TRUE))
  expect_false(grepl(p$inline.code, "a `code span`", perl = TRUE))
})

test_that("a chunk header: the engine, the options, and #| options", {
  b <- knitrmini:::parse_block(c("#| fig-cap: \"A cap\"", "#| echo: false", "plot(1)"), "r points, fig.width = 4", markdown_mode = TRUE)
  expect_equal(b$params$label, "points")
  expect_equal(b$params$fig.width, 4)
  expect_equal(b$params$fig.cap, "A cap")
  expect_false(b$params$echo)
  expect_equal(as.character(knit_code$get("points")), "plot(1)")
  p <- knitrmini:::parse_block("print(1)", "python", markdown_mode = TRUE)$params
  expect_equal(p$engine, "python")
  expect_false(p$eval) # (only R runs)
})

test_that("the YAML header is read, and the body keeps its line numbers", {
  r <- knitrmini:::md_split_yaml(c("---", "title: A", "bibliography: [a.bib, b.bib]", "---", "Body"))
  expect_equal(r$yaml$title, "A")
  expect_equal(r$yaml$bibliography, c("a.bib", "b.bib"))
  expect_equal(length(r$body), 5)
  expect_equal(r$body[5], "Body")
  expect_equal(knitrmini:::md_split_yaml("No header")$body, "No header")
})

test_that("a document: chunks as in Rnoweb, prose as LaTeX by hand, the preamble only for what is used", {
  skip_if_no_pandoc()
  dir <- make_files(
    "paper.Rmd" = c(
      "---", "documentclass: article", "classoption: [12pt]", "preamble: preamble.tex",
      "bibliography: refs.bib", "biblatex: style=authoryear", "---", "",
      "# Introduction {#sec:intro}", "",
      "The answer is `r 6 * 7`, *as* **known** [@smith2020; see @jones2019, p. 3].",
      "@smith2020 said so (Section \\@ref(sec:intro), Figure \\ref{fig:points}). A `code span` and $x^2$.", "",
      "```{r points, fig.cap = 'Some points.', fig.width = 4, fig.height = 3}", "plot(1:3)", "```", "",
      "```{r}", "x <- 1:3", "x", "```", "",
      "```{r, results = 'asis'}", "cat('A **bold** asis line.')", "```", "",
      "| a | b |", "|:--|--:|", "| 1 | 2 |", "", "Table: Two numbers. (\\#tab:two)", "",
      "# Unnumbered {-}", "",
      "- one", "- two", "",
      "Text with a [link](https://example.org)."
    ),
    "preamble.tex" = "\\usepackage{amsmath}"
  )
  tex <- knit_rmd(dir)
  expect_true(has(tex, "\\documentclass[12pt]{article}"))
  expect_true(has(tex, "\\usepackage{amsmath}")) # (its preamble file, inlined as in Rnoweb)
  expect_true(has(tex, "\\usepackage[style=authoryear]{biblatex}"))
  expect_true(has(tex, "\\addbibresource{refs.bib}"))
  expect_true(has(tex, "\\section{Introduction}\\label{sec:intro}"))
  expect_true(has(tex, "The answer is 42, \\emph{as} \\textbf{known} \\autocites{smith2020}[see][3]{jones2019}."))
  expect_true(has(tex, "\\textcite{smith2020} said so (Section \\ref{sec:intro}, Figure \\ref{fig:points})."))
  expect_true(has(tex, "\\texttt{code\\ span}") || has(tex, "\\texttt{code span}"))
  expect_true(has(tex, "$x^2$"))
  expect_true(has(tex, "\\begin{Shaded}")) # (the chunk's source, as in Rnoweb)
  expect_true(has(tex, "## [1] 1 2 3")) # (its output)
  expect_true(has(tex, "\\label{fig:points}")) # (its figure)
  expect_true(has(tex, "A \\textbf{bold} asis line.")) # (asis: Markdown)
  expect_true(has(tex, "\\begin{tabular}{lr}"))
  expect_true(has(tex, "\\caption{Two numbers.}\\label{tab:two}"))
  expect_true(has(tex, "\\section*{Unnumbered}"))
  expect_true(has(tex, "\\href{https://example.org}{link}"))
  expect_true(has(tex, "\\printbibliography"))
  # what it needs, once; nothing of pandoc's or of the intermediate Markdown
  for (pkg in c("graphicx", "booktabs", "hyperref", "xcolor")) {
    expect_equal(lengths(regmatches(tex, gregexpr(sprintf("\\\\usepackage\\{%s\\}", pkg), tex))), 1, label = pkg)
  }
  expect_true(has(tex, "\\providecommand{\\tightlist}"))
  for (noise in c("{=latex}", "````", "\\addcontentsline", "\\hypertarget", "longtable", "\\pandocbounded", "\\(x")) {
    expect_false(has(tex, noise), label = noise)
  }
  expect_true(grepl("\\\\begin\\{document\\}", tex) && grepl("\\\\end\\{document\\}$", tex))
})

test_that("YAML title, authors, date and abstract; a preamble's own biblatex and resources not repeated", {
  skip_if_no_pandoc()
  dir <- make_files(
    "paper.Rmd" = c(
      "---", "title: A *short* paper", "author:", "  - Ann Writer", "  - name: Bob Reader", "date: 2026",
      "abstract: It is **short**.", "preamble: [preamble.tex]", "bibliography: refs.bib", "print-bibliography: false", "---", "",
      "Text [@a]."
    ),
    "preamble.tex" = c("\\usepackage[backend=biber]{biblatex}", "\\addbibresource{refs.bib}")
  )
  tex <- knit_rmd(dir)
  expect_true(has(tex, "\\title{A \\emph{short} paper}"))
  expect_true(has(tex, "\\author{Ann Writer \\and Bob Reader}"))
  expect_true(has(tex, "\\date{2026}"))
  expect_true(has(tex, "\\maketitle"))
  expect_true(has(tex, "\\begin{abstract}\nIt is \\textbf{short}.\n\\end{abstract}"))
  expect_equal(lengths(regmatches(tex, gregexpr("usepackage(\\[[^]]*\\])?\\{biblatex\\}", tex))), 1)
  expect_equal(lengths(regmatches(tex, gregexpr("addbibresource\\{refs.bib\\}", tex))), 1)
  expect_false(has(tex, "\\printbibliography"))
})

test_that("inline code in code spans and fenced code is not evaluated", {
  skip_if_no_pandoc()
  dir <- make_files("paper.Rmd" = c(
    "Evaluated: `r 1 + 1`. Not: `` `r 1 + 1` ``.", "",
    "```", "not a chunk: `r 2 + 2`", "```"
  ))
  tex <- knit_rmd(dir)
  expect_true(has(tex, "Evaluated: 2."))
  expect_true(has(tex, "r 2 + 2"))
  expect_false(has(tex, "4"))
})

test_that("children: knitted in the parent's session; eval=FALSE skips one, its labels from the snapshot", {
  skip_if_no_pandoc()
  dir <- make_files(
    "paper.Rmd" = c(
      "---", "documentclass: article", "---", "",
      "```{r setup}", "n <- 7", "```", "",
      "```{r child = '1-main.Rmd'}", "```", "",
      "```{r child = '2-appendix.Rmd', eval = FALSE}", "```"
    ),
    "1-main.Rmd" = c("# Main {#sec:main}", "", "Seven is `r n`; see Appendix \\@ref(sec:app)."),
    "2-appendix.Rmd" = c("# Appendix {#sec:app}", "", "Only in the appendix.")
  )
  writeLines("\\newlabel{sec:app}{{A}{9}}", file.path(dir, "paper-labels.aux"))
  tex <- knit_rmd(dir)
  expect_true(has(tex, "Seven is 7; see Appendix A."))
  expect_false(has(tex, "Only in the appendix"))
})

test_that("a document is built by latexmk, as an Rnoweb one", {
  skip_if_no_pandoc()
  skip_if(!nzchar(Sys.which("latexmk")) || !nzchar(Sys.which("pdflatex")), "latexmk and pdflatex are needed")
  dir <- make_files("paper.Rmd" = c(
    "---", "title: A paper", "---", "", "# Results {#sec:r}", "", "Section \\@ref(sec:r) has `r 2 * 21`.", "",
    "```{r points, fig.cap = 'Points.'}", "plot(1:3)", "```"
  ))
  pdf <- suppressMessages(suppressWarnings(
    knitrmini::knit(file.path(dir, "paper.Rmd"), quiet = TRUE, engine = "pdflatex")
  ))
  expect_true(file.exists(file.path(dir, "paper.pdf")))
})

test_that("figure paths: as written in a .tex beside its source; absolute in one written elsewhere", {
  dir <- make_files("paper.Rnw" = c(
    "\\documentclass{article}", "\\begin{document}",
    "<<points, fig.cap='Points.'>>=", "plot(1:3)", "@",
    "\\includegraphics{../figs/drawn.pdf}", "\\end{document}"
  ))
  old <- setwd(dir)
  on.exit(setwd(old), add = TRUE)
  suppressMessages(suppressWarnings(knitrmini::knit("paper.Rnw", compile = FALSE, quiet = TRUE)))
  tex <- paste(readLines("paper.tex"), collapse = "\n")
  expect_true(has(tex, "{figure/points-1}"))
  expect_true(has(tex, "\\includegraphics{../figs/drawn.pdf}"))
  expect_false(has(tex, dir))
  elsewhere <- file.path(tempfile("out"), "paper.tex")
  dir.create(dirname(elsewhere))
  suppressMessages(suppressWarnings(knitrmini::knit("paper.Rnw", elsewhere, compile = FALSE, quiet = TRUE)))
  tex <- paste(readLines(elsewhere), collapse = "\n")
  expect_true(has(tex, file.path(normalizePath(dir), "figure", "points-1")))
  expect_true(has(tex, file.path(normalizePath(dirname(dir)), "figs", "drawn.pdf")))
})
