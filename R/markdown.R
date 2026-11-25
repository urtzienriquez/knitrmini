#' R Markdown to LaTeX
#'
#' An R Markdown document is knitted like an Rnoweb one: its chunks run with the
#' LaTeX hooks (see [render_markdown_latex()]), and its prose, knitted, becomes
#' LaTeX with pandoc: the body only, through knitrmini's filter, which writes
#' figures, tables, headings and math as one would by hand. The document around
#' it comes from a few keys of its YAML header, and nothing else:
#'
#' \describe{
#'   \item{documentclass, classoption}{\code{\\documentclass[...]{...}} (default
#'     \code{article}).}
#'   \item{preamble}{Files \code{\\input} before \code{\\begin{document}} (your
#'     packages and macros), relative to the document.}
#'   \item{header-includes}{LaTeX lines for the preamble.}
#'   \item{bibliography, biblatex}{\code{\\addbibresource} for each .bib file;
#'     \code{\\usepackage[<biblatex>]{biblatex}} unless the preamble loads it;
#'     \code{\\printbibliography} at the end, if anything is cited and the body
#'     doesn't print it (\code{print-bibliography: false} to leave it out).}
#'   \item{title, author, date, abstract}{\code{\\title}, \code{\\author} (several
#'     joined by \code{\\and}), \code{\\date}, and \code{\\maketitle} and the
#'     abstract at the start, when given.}
#' }
#'
#' Citations become biblatex's (\code{[@a]} \code{\\autocite{a}}, \code{@a}
#' \code{\\textcite{a}}); bookdown's \code{\\@ref(x)} and \code{(\\#x)} labels,
#' and pandoc's \code{{#x}}, become \code{\\ref{x}} and \code{\\label{x}}.
#'
#' @name rmarkdown
#' @keywords internal
NULL

# The YAML header of an R Markdown document (a list) and its body (lines).
md_split_yaml <- function(text) {
  text <- split_lines(one_string(text))
  if (!length(text) || !grepl("^---\\s*$", text[1])) {
    return(list(yaml = list(), body = text))
  }
  end <- which(grepl("^(---|\\.\\.\\.)\\s*$", text))
  end <- end[end > 1][1]
  if (is.na(end)) return(list(yaml = list(), body = text))
  lines <- if (end > 2) text[2:(end - 1)] else character()
  # (the yaml package if installed, as with rmarkdown: lists of names and of entries, nested...)
  yaml <- if (!length(lines)) list() else if (requireNamespace("yaml", quietly = TRUE)) {
    yaml::yaml.load(one_string(lines)) %n% list()
  } else {
    xfun::taml_load(lines)
  }
  # (the body keeps its line numbers: blank lines where the header was)
  list(yaml = yaml, body = c(rep("", end), text[-seq_len(end)]))
}

pandoc_path <- function() {
  p <- Sys.getenv("RSTUDIO_PANDOC")
  p <- if (nzchar(p)) file.path(p, "pandoc") else Sys.which("pandoc")
  if (!nzchar(p) || !file.exists(p)) {
    stop("Knitting R Markdown needs pandoc, and it was not found.")
  }
  p
}

# Markdown (one string) as LaTeX: the body only, through knitrmini's filter.
md_to_latex <- function(md) {
  md <- gsub("\\\\@ref\\(([^)]+)\\)", "\\\\ref{\\1}", md) # (bookdown's references)
  input <- tempfile(fileext = ".md")
  on.exit(unlink(input), add = TRUE)
  xfun::write_utf8(md, input)
  filter <- system.file("lua", "knitrmini.lua", package = "knitrmini")
  args <- c(
    shQuote(input), "-f", "markdown-auto_identifiers", "-t", "latex", "--biblatex",
    "--wrap=preserve", if (nzchar(filter)) c("--lua-filter", shQuote(filter))
  )
  out <- suppressWarnings(system2(pandoc_path(), args, stdout = TRUE, stderr = TRUE))
  status <- attr(out, "status")
  if (!is.null(status) && status != 0) {
    stop("pandoc could not convert the document:\n", one_string(out))
  }
  one_string(out)
}

# A YAML value as LaTeX: strings go through pandoc (their Markdown); nothing if empty.
yaml_latex <- function(x) {
  if (is.null(x) || !length(x)) return(NULL)
  x <- unlist(x)
  vapply(x, function(v) sub("\n+$", "", md_to_latex(as.character(v))), "", USE.NAMES = FALSE)
}

#' Assemble the LaTeX document of an R Markdown body
#'
#' @param body LaTeX of the body (from md_to_latex()).
#' @param yaml The document's YAML header (a list).
#' @param dir The document's folder (for its preamble files).
#' @return The document (one string).
#' @keywords internal
md_document <- function(body, yaml, dir = ".") {
  opt <- unlist(yaml$classoption)
  out <- sprintf(
    "\\documentclass%s{%s}",
    if (length(opt)) sprintf("[%s]", paste(opt, collapse = ",")) else "",
    yaml$documentclass %n% "article"
  )
  preamble <- unlist(yaml$preamble)
  out <- c(out, if (length(preamble)) sprintf("\\input{%s}", preamble))
  out <- c(out, unlist(yaml[["header-includes"]]))
  # (what the preamble files say: is biblatex loaded, which .bib files are added already)
  said <- one_string(c(out, unlist(lapply(preamble, function(f) {
    f <- if (is_abs_path(f)) f else file.path(dir, f)
    if (!grepl("\\.tex$", f) && !file.exists(f)) f <- paste0(f, ".tex")
    if (file.exists(f)) xfun::read_utf8(f)
  }))))
  bibs <- unlist(yaml$bibliography)
  cited <- grepl("\\\\(auto|text|paren|foot)?cites?\\*?(\\[|\\{)", body)
  if (length(bibs)) {
    if (!has_package(said, "biblatex")) {
      bo <- unlist(yaml$biblatex)
      out <- c(out, sprintf("\\usepackage%s{biblatex}",
        if (length(bo)) sprintf("[%s]", paste(bo, collapse = ",")) else ""))
    }
    for (b in bibs) {
      if (!grepl(sprintf("\\\\addbibresource(\\[[^]]*\\])?\\{%s\\}", gsub("([.\\\\])", "\\\\\\1", b)), said)) {
        out <- c(out, sprintf("\\addbibresource{%s}", b))
      }
    }
  }
  title <- yaml_latex(yaml$title)
  if (length(title)) {
    authors <- yaml$author
    if (is.list(authors)) {
      authors <- vapply(authors, function(a) if (is.list(a)) as.character(a$name %n% "") else as.character(a), "")
    }
    out <- c(out, sprintf("\\title{%s}", title[1]))
    if (length(authors)) out <- c(out, sprintf("\\author{%s}", paste(yaml_latex(authors), collapse = " \\and ")))
    if (!is.null(yaml$date)) out <- c(out, sprintf("\\date{%s}", yaml_latex(yaml$date)[1]))
  }
  out <- c(out, "\\begin{document}")
  if (length(title) && !grepl("\\\\maketitle", body)) out <- c(out, "\\maketitle")
  abstract <- yaml$abstract
  if (!is.null(abstract)) {
    out <- c(out, "\\begin{abstract}", sub("\n+$", "", md_to_latex(one_string(unlist(abstract)))), "\\end{abstract}")
  }
  out <- c(out, "", sub("^\n+", "", sub("\n+$", "", body)), "")
  if (length(bibs) && cited && !isFALSE(yaml[["print-bibliography"]]) &&
    !grepl("\\\\printbibliography", paste(said, body))) {
    out <- c(out, "\\printbibliography", "")
  }
  one_string(c(out, "\\end{document}"))
}
