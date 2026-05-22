#' All pattern lists
#'
#' A list of pattern lists for different document types. Currently only \code{rnw}
#' is defined, containing regex patterns for chunk delimiters, inline code,
#' and document structure markers.
#'
#' @format A named list with one entry \code{rnw} containing:
#' \describe{
#'   \item{chunk.begin}{Pattern for chunk start (\code{<<...>>=})}
#'   \item{chunk.end}{Pattern for chunk end (\code{@})}
#'   \item{inline.code}{Pattern for \code{\\Sexpr{...}}}
#'   \item{inline.comment}{Pattern for LaTeX comments}
#'   \item{ref.chunk}{Pattern for chunk references}
#'   \item{header.begin}{Pattern for \code{\\documentclass}}
#'   \item{document.begin}{Pattern for \code{\\begin{document}}}
#' }
#' @export
all_patterns <- list(
  `rnw` = list(
    chunk.begin = "^\\s*<<(.*)>>=.*$",
    chunk.end = "^\\s*@\\s*(%+.*|)$",
    inline.code = "\\\\Sexpr\\{([^}]+)\\}",
    inline.comment = "^\\s*%.*",
    ref.chunk = "^\\s*<<(.+)>>\\s*$",
    header.begin = "(^|\n)\\s*\\\\documentclass[^}]+\\}",
    document.begin = "\\s*\\\\begin\\{document\\}"
  )
)

#' Detect document pattern
#'
#' Detect which pattern list to use based on file extension or document content.
#'
#' @param text Document text as a character vector.
#' @param ext File extension (lowercase).
#' @return Pattern name (currently \code{"rnw"}) or \code{NULL}.
#' @keywords internal
detect_pattern <- function(text, ext) {
  if (ext %in% c("rnw", "snw", "stex")) {
    return("rnw")
  }
  lines <- split_lines(text)
  if (is.null(lines) || length(lines) == 0) return(NULL)
  for (n in names(all_patterns)) {
    p <- all_patterns[[n]]
    if (is.null(p$chunk.begin) || is.null(p$chunk.end)) next
    if (any(grepl(p$chunk.begin, lines)) && any(grepl(p$chunk.end, lines))) {
      return(n)
    }
  }
  NULL
}

group_pattern <- function(pattern) {
  length(pattern) == 1 && !is.na(pattern) && nzchar(pattern)
}

#' Check output format
#'
#' Get or test the current output format. If called with no arguments, returns
#' the current format name. If called with a format name, returns \code{TRUE}
#' if the current format matches.
#'
#' @param fmt Optional format name to test.
#' @return Character string or logical.
#' @keywords internal
out_format <- function(fmt = NULL) {
  cur <- opts_knit$get("out.format")
  if (is.null(fmt)) {
    cur
  } else if (is.null(cur)) {
    FALSE
  } else {
    cur %in% fmt
  }
}

#' Knit patterns
#'
#' A list-like object (created by [new_defaults()]) that stores the active set of
#' pattern definitions used to parse the input document. Defaults to the \code{rnw}
#' pattern list from [all_patterns()].
#'
#' @format A list with methods: \code{get}, \code{set}, \code{delete}, \code{append},
#'   \code{merge}, and \code{restore}.
#' @export
#' @seealso \code{\link{all_patterns}}
knit_patterns <- new_defaults(all_patterns$rnw)
