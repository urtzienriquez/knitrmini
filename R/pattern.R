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

knit_patterns <- new_defaults(all_patterns$rnw)
