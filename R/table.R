#' Create a LaTeX table
#'
#' A minimal table generator for knitrmini. Produces LaTeX tables with
#' \code{tabular} (or \code{longtable}) environment. Intended for use inside
#' knitrmini code chunks, but also usable interactively.
#'
#' @param x A matrix or data frame.
#' @param digits Maximum number of digits for numeric columns. Can be a vector
#'   of length \code{ncol(x)}.
#' @param row.names Logical: include row names? \code{NA} (default) includes
#'   them if they differ from \code{1:nrow(x)}.
#' @param col.names A character vector of column names.
#' @param align Column alignment: a character vector of \code{'l'}, \code{'c'},
#'   \code{'r'}, or a single string such as \code{'lcr'}. Numeric columns are
#'   right-aligned and others left-aligned by default.
#' @param caption Table caption. Defaults to the chunk option \code{tab.cap}.
#' @param label Table label for \code{\\label{}}. Defaults to the current chunk
#'   label. Set to \code{NA} to omit.
#' @param format.args A list of arguments passed to \code{\link{format}()}.
#' @param escape Logical: escape special LaTeX characters?
#' @param booktabs Logical: use \pkg{booktabs} rules?
#' @param longtable Logical: use \code{longtable} environment?
#' @param tabular Name of the tabular environment (e.g. \code{'tabular'},
#'   \code{'longtable'}, \code{'tabularx'}).
#' @param valign Vertical alignment of the tabular environment. Use e.g.
#'   \code{'t'} or \code{'b'}. For \code{tabularx} / \code{xltabular} the
#'   default is \code{'{\\linewidth}'}.
#' @param position Placement argument for the \code{table} environment
#'   (e.g. \code{'!htb'}).
#' @param centering Logical: add \code{\\centering}?
#' @param vline Vertical line separator between columns.
#' @param toprule Top rule command.
#' @param midrule Mid rule command (after header).
#' @param bottomrule Bottom rule command.
#' @param linesep Line separator between data rows.
#' @param caption.short Short caption for the list of tables.
#' @param raw Logical: when \code{TRUE}, emit only the \code{tabular} content
#'   (no \code{table} environment, no caption, no label, no centering).
#'   Useful when you want to wrap the table in a custom environment or
#'   place the caption/label outside the R chunk.
#' @param table.envir Name of the floating environment (default \code{'table'}
#'   when caption is not \code{NULL}, otherwise \code{NULL} for no float).
#' @param ... Additional arguments (currently unused).
#' @return A character vector of the table LaTeX code, with class
#'   \code{'knitr_kable'}.
#' @export
#' @examples
#' kable(mtcars[1:5, 1:4])
#' kable(mtcars[1:5, 1:4], caption = "My caption", booktabs = TRUE)
#' kable(mtcars[1:5, 1:4], raw = TRUE)
kable <- function(
  x, digits = getOption("digits"), row.names = NA, col.names = NA,
  align, caption = opts_chunk$get("tab.cap"), label = NULL,
  format.args = list(), escape = TRUE, raw = FALSE, booktabs = FALSE,
  longtable = FALSE,
  tabular = if (longtable) "longtable" else "tabular",
  valign = if (tabular %in% c("tabularx", "xltabular")) "{\\linewidth}" else "[t]",
  position = "", centering = TRUE,
  vline = getOption("knitr.table.vline", if (booktabs) "" else "|"),
  toprule = getOption("knitr.table.toprule", if (booktabs) "\\toprule" else "\\hline"),
  bottomrule = getOption("knitr.table.bottomrule", if (booktabs) "\\bottomrule" else "\\hline"),
  midrule = getOption("knitr.table.midrule", if (booktabs) "\\midrule" else "\\hline"),
  linesep = if (booktabs) c("", "", "", "", "\\addlinespace") else "\\hline",
  caption.short = "",
  table.envir = if (!is.null(caption)) "table",
  ...
) {
  if (!missing(align) && length(align) == 1L && !grepl("[^lcr]", align)) {
    align <- strsplit(align, "")[[1]]
  }
  if (raw) {
    table.envir <- NULL
    caption <- NULL
    centering <- FALSE
    label <- NA
  }
  if (is.data.frame(x)) {
    x <- as.data.frame(x)
  } else if (!is.matrix(x)) {
    x <- as.data.frame(x)
    if (ncol(x) == 0) x <- matrix(nrow = NROW(x), ncol = 0)
  }
  if (identical(col.names, NA)) col.names <- colnames(x)
  m <- ncol(x)
  isn <- if (is.matrix(x)) rep(is.numeric(x), m) else sapply(x, is.numeric)
  if (missing(align) || is.null(align)) {
    align <- ifelse(isn, "r", "l")
  }
  digits <- rep(digits, length.out = m)
  for (j in seq_len(m)) {
    if (is_numeric(x[, j])) x[, j] <- round(x[, j], digits[j])
  }
  if (any(isn)) {
    if (is.matrix(x)) {
      x <- format_matrix(x, format.args)
    } else {
      x[, isn] <- format_args(x[, isn], format.args)
    }
  }
  if (is.na(row.names)) row.names <- has_rownames(x)
  if (!is.null(align)) align <- rep(align, length.out = m)
  if (row.names) {
    x <- cbind(" " = rownames(x), x)
    if (!is.null(col.names)) col.names <- tail(c(" ", col.names), ncol(x))
    if (!is.null(align)) align <- c("l", align)
  }
  n <- nrow(x)
  x <- replace_na(to_character(x), is.na(x))
  if (!is.matrix(x)) x <- matrix(x, nrow = n)
  x <- gsub("^\\s+|(?<!\\\\)\\s+$", "", x, perl = TRUE)
  colnames(x) <- col.names
  attr(x, "align") <- align
  caption <- kable_caption(label, caption)
  res <- kable_latex(
    x = x, booktabs = booktabs, longtable = longtable, tabular = tabular,
    valign = valign, position = position, centering = centering,
    vline = vline, toprule = toprule, bottomrule = bottomrule,
    midrule = midrule, linesep = linesep, caption = caption,
    caption.short = caption.short, table.envir = table.envir,
    escape = escape
  )
  structure(res, format = "latex", class = "knitr_kable")
}

kable_caption <- function(label, caption) {
  if (is.null(label)) {
    labels <- .knitEnv$labels %n% character()
    label <- if (length(labels)) tail(labels, 1) else NA
  }
  if (!is.null(caption) && !anyNA(caption) && !anyNA(label)) {
    caption <- paste0(create_label(label), caption)
  }
  caption
}

create_label <- function(label) {
  prefix <- opts_knit$get("label.prefix")[["table"]] %n% "tab:"
  if (length(label) != 1 || is.na(label) || is.null(label)) return("")
  sprintf("\\label{%s%s}\n", prefix, label)
}

kable_latex <- function(
  x, booktabs = FALSE, longtable = FALSE,
  tabular = if (longtable) "longtable" else "tabular",
  valign = if (tabular %in% c("tabularx", "xltabular")) "{\\linewidth}" else "[t]",
  position = "", centering = TRUE,
  vline = getOption("knitr.table.vline", if (booktabs) "" else "|"),
  toprule = getOption("knitr.table.toprule", if (booktabs) "\\toprule" else "\\hline"),
  bottomrule = getOption("knitr.table.bottomrule", if (booktabs) "\\bottomrule" else "\\hline"),
  midrule = getOption("knitr.table.midrule", if (booktabs) "\\midrule" else "\\hline"),
  linesep = if (booktabs) c("", "", "", "", "\\addlinespace") else "\\hline",
  caption = NULL, caption.short = "",
  table.envir = if (!is.null(caption)) "table",
  escape = TRUE, ...
) {
  if (!is.null(align <- attr(x, "align"))) {
    align <- paste(align, collapse = vline)
    align <- paste0("{", align, "}")
  }
  centering <- if (centering && !is.null(caption)) "\n\\centering"
  valign <- if ((!is.null(caption) || !missing(valign) ||
    tabular %in% c("tabularx", "xltabular")) && valign != "") {
    if (grepl("^[[{]", valign)) valign else sprintf("[%s]", valign)
  } else ""
  if (identical(caption, NA)) caption <- NULL
  if (position != "") position <- paste0("[", position, "]")
  env1 <- sprintf("\\begin{%s}%s\n", table.envir, position)
  env2 <- sprintf("\n\\end{%s}", table.envir)
  if (caption.short != "") caption.short <- paste0("[", caption.short, "]")
  cap <- if (is.null(caption)) "" else sprintf("\n\\caption%s{%s}", caption.short, caption)
  if (nrow(x) == 0) midrule <- ""
  linesep <- if (nrow(x) > 1) {
    c(rep(linesep, length.out = nrow(x) - 1), "")
  } else rep("", nrow(x))
  linesep <- ifelse(linesep == "", linesep, paste0("\n", linesep))
  x <- escape_latex_table(x, escape, booktabs)
  if (!is.character(toprule)) toprule <- NULL
  if (!is.character(bottomrule)) bottomrule <- NULL
  paste(c(
    if (cap_env <- !tabular %in% c("longtable", "xltabular")) c(env1, cap, centering),
    sprintf("\n\\begin{%s}%s", tabular, valign), align,
    if (!cap_env && cap != "") c(cap, "\\\\"),
    sprintf("\n%s", toprule), "\n",
    if (!is.null(cn <- colnames(x))) {
      cn <- escape_latex_table(cn, escape, booktabs)
      paste0(paste(cn, collapse = " & "), sprintf("\\\\\n%s\n", midrule))
    },
    one_string(apply(x, 1, paste, collapse = " & "), sprintf("\\\\%s", linesep), sep = ""),
    sprintf("\n%s", bottomrule),
    sprintf("\n\\end{%s}", tabular),
    if (cap_env) env2
  ), collapse = "")
}

escape_latex_table <- function(x, escape = TRUE, brackets = TRUE) {
  if (escape) x <- escape_latex(x)
  if (brackets) x <- gsub("^(\\s*)(\\[)", "\\1{}\\2", x)
  x
}

has_rownames <- function(x) {
  !is.null(rownames(x)) && !identical(rownames(x), as.character(seq_len(NROW(x))))
}

to_character <- function(x) {
  if (is.character(x)) return(x)
  if (!is.matrix(x)) {
    for (j in seq_len(ncol(x))) x[, j] <- format_args(x[, j])
    x <- as.matrix(x)
  }
  x2 <- as.character(x)
  dim(x2) <- dim(x)
  dimnames(x2) <- dimnames(x)
  x2
}

format_matrix <- function(x, args) {
  nms <- rownames(x)
  rownames(x) <- NULL
  x <- as.matrix(format_args(as.data.frame(x), args))
  rownames(x) <- nms
  x
}

format_args <- function(x, args = list()) {
  args$x <- x
  args$trim <- TRUE
  replace_na(do.call(format, args), is.na(x))
}

replace_na <- function(x, which = is.na(x), to = getOption("knitr.kable.NA")) {
  if (is.null(to)) return(x)
  x[which] <- to
  x
}

is_numeric <- function(x) {
  inherits(x, "numeric") || inherits(x, "integer") ||
    inherits(x, "difftime") || inherits(x, "complex")
}

#' @exportS3Method NULL
knit_print.knitr_kable <- function(x, ...) {
  one_string(x)
}

#' @export
print.knitr_kable <- function(x, ...) {
  .kableEnv$active <- TRUE
  cat(x, sep = "\n")
}

.kableEnv <- new.env(parent = emptyenv())
.kableEnv$active <- FALSE
