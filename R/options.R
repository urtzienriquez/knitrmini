#' Create a new defaults container
#'
#' Creates an object with `get`, `set`, `delete`, `append`, `merge`, and `restore`
#' methods for managing a list of default values. Used internally by `opts_chunk`,
#' `opts_knit`, `knit_hooks`, `knit_patterns`, and `knit_code`.
#'
#' @param value A named list of initial default values.
#' @return A list of functions for manipulating the defaults.
#' @keywords internal
new_defaults <- function(value = list()) {
  defaults <- value
  get <- function(name, default = FALSE, drop = TRUE) {
    if (default) defaults <- value
    if (missing(name)) {
      defaults
    } else {
      if (drop && length(name) == 1) {
        defaults[[name]]
      } else {
        setNames(defaults[name], name)
      }
    }
  }
  resolve <- function(...) {
    dots <- list(...)
    if (length(dots) == 0) {
      return()
    }
    if (is.null(names(dots)) && length(dots) == 1 && is.list(dots[[1]])) {
      if (length(dots <- dots[[1]]) == 0) {
        return()
      }
    }
    dots
  }
  set <- function(...) {
    dots <- resolve(...)
    if (length(dots)) defaults <<- merge_list(defaults, dots)
    invisible(NULL)
  }
  merge <- function(values) merge_list(defaults, values)
  delete <- function(keys) {
    for (k in keys) defaults[[k]] <<- NULL
  }
  restore <- function(target = value) defaults <<- target
  append <- function(...) {
    dots <- resolve(...)
    for (i in names(dots)) dots[[i]] <- c(defaults[[i]], dots[[i]])
    set(dots)
  }
  list(get = get, set = set, delete = delete, append = append, merge = merge, restore = restore)
}

#' Default chunk options
#'
#' A list-like object (created by [new_defaults()]) that stores default chunk options.
#' Use `opts_chunk$set(...)` to change defaults and `opts_chunk$get()` to retrieve them.
#'
#' @format A list with methods: \code{get}, \code{set}, \code{delete}, \code{append},
#'   \code{merge}, and \code{restore}.
#' @export
#' @seealso \code{\link{opts_knit}} for global options, \code{\link{knit_hooks}} for output hooks
#'
#' @examples
#' opts_chunk$set(echo = FALSE, eval = TRUE)
#' opts_chunk$get("echo")
opts_chunk <- new_defaults(list(
  eval = TRUE, echo = TRUE, results = "markup",
  tidy = FALSE, tidy.opts = NULL,
  collapse = FALSE, prompt = FALSE, comment = "##", highlight = TRUE,
  size = "normalsize", background = "#F7F7F7",
  strip.white = TRUE,
  cache = FALSE, cache.path = "cache/", cache.rebuild = FALSE, cache.comments = TRUE,
  dependson = NULL,
  fig.keep = "high", fig.path = "figure/",
  dev = NULL, dev.args = NULL, dpi = 72, fig.ext = NULL,
  fig.width = 7, fig.height = 7,
  fig.env = NULL, fig.cap = NULL, fig.lp = "fig:",
  fig.pos = "", fig.align = "default", out.width = NULL, out.height = NULL,
  out.extra = NULL, interval = 1, aniopts = "controls,loop",
  warning = TRUE, error = TRUE, message = TRUE,
  render = NULL,
  ref.label = NULL, child = NULL, engine = "R", split = FALSE, include = TRUE,
  tangle = TRUE,
  minted = FALSE, minted_style = NULL,
  external = TRUE
))

#' Global knit options
#'
#' A list-like object (created by [new_defaults()]) that stores global knitrmini options.
#' Use `opts_knit$set(...)` to change options and `opts_knit$get()` to retrieve them.
#'
#' @format A list with methods: \code{get}, \code{set}, \code{delete}, \code{append},
#'   \code{merge}, and \code{restore}.
#' @export
#' @seealso \code{\link{opts_chunk}} for chunk defaults
#'
#' @examples
#' opts_knit$set(engine = "xelatex", progress = FALSE)
#' opts_knit$get("engine")
opts_knit <- new_defaults(list(
  progress = TRUE, verbose = FALSE, global.device = FALSE, global.par = FALSE,
  eval.after = c("fig.cap"),
  root.dir = NULL, child.path = "",
  concordance = FALSE, unnamed.chunk.label = "unnamed-chunk",
  out.format = NULL, child = FALSE, parent = FALSE,
  aliases = NULL, resolve_input = TRUE,
  header = c(highlight = "", framed = ""),
  minted_style = NULL, engine = "pdflatex",
  normalize_paths = TRUE, tangle = FALSE
))

#' Merge two named lists
#'
#' @param x Base list
#' @param y List whose values override \code{x}
#' @return A merged list.
#' @keywords internal
merge_list <- function(x, y) {
  x[names(y)] <- y
  x
}

#' Null-default operator
#'
#' Returns \code{y} if \code{x} is \code{NULL}, otherwise returns \code{x}.
#' Inspired by the `\%||\%` operator from \pkg{knitr}.
#'
#' @param x Value to check
#' @param y Fallback value
#' @return \code{x} if non-NULL, else \code{y}.
#' @keywords internal
#' @name op-null-default
`%n%` <- function(x, y) if (is.null(x)) y else x

parse_only <- function(code) {
  if (length(code) > 1) code <- one_string(code)
  parse(text = code, keep.source = FALSE)
}

one_string <- function(x, ...) {
  paste(x, collapse = "\n", ...)
}

split_lines <- function(x) {
  if (length(x) < 1) return(character(0))
  x <- gsub("\r\n", "\n", x, useBytes = TRUE)
  x <- gsub("\r", "\n", x, useBytes = TRUE)
  if (!nzchar(x)) return("")
  strsplit(x, "\n")[[1]]
}

is_blank <- function(x) {
  if (length(x)) all(grepl("^\\s*$", x)) else TRUE
}

sc_split <- function(string) {
  if (is.call(string)) string <- eval(string)
  if (is.symbol(string)) string <- deparse(string)
  if (is.numeric(string) || length(string) != 1L) {
    return(string)
  }
  trimws(strsplit(string, ";|,")[[1]])
}

valid_path <- function(prefix, label) {
  if (length(prefix) == 0L || is.na(prefix) || prefix == "NA") prefix <- ""
  paste0(prefix, label)
}
