knit_patterns = NULL

all_patterns = list(
  `rnw` = list(
    chunk.begin = '^\\s*<<(.*)>>=.*$',
    chunk.end = '^\\s*@\\s*(%+.*|)$',
    inline.code = '\\\\Sexpr\\{([^}]+)\\}',
    inline.comment = '^\\s*%.*',
    ref.chunk = '^\\s*<<(.+)>>\\s*$',
    header.begin = '(^|\n)\\s*\\\\documentclass[^}]+\\}',
    document.begin = '\\s*\\\\begin\\{document\\}'
  )
)

detect_pattern = function(text, ext) {
  if (ext %in% c('rnw', 'snw', 'stex')) return('rnw')
  for (n in names(all_patterns)) {
    p = all_patterns[[n]]
    if (is.null(p$chunk.begin) || is.null(p$chunk.end)) next
    if (any(grepl(p$chunk.begin, text)) && any(grepl(p$chunk.end, text)))
      return(n)
  }
  NULL
}

group_pattern = function(pattern) {
  length(pattern) == 1 && !is.na(pattern) && nzchar(pattern)
}

out_format = function(fmt = NULL) {
  if (is.null(fmt)) opts_knit$get('out.format') else {
    opts_knit$get('out.format') %in% fmt
  }
}

knit_patterns = new_defaults(all_patterns$rnw)
