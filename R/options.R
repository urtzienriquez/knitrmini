new_defaults = function(value = list()) {
  defaults = value
  get = function(name, default = FALSE, drop = TRUE) {
    if (default) defaults = value
    if (missing(name)) defaults else {
      if (drop && length(name) == 1) defaults[[name]] else
        setNames(defaults[name], name)
    }
  }
  resolve = function(...) {
    dots = list(...)
    if (length(dots) == 0) return()
    if (is.null(names(dots)) && length(dots) == 1 && is.list(dots[[1]]))
      if (length(dots <- dots[[1]]) == 0) return()
    dots
  }
  set = function(...) {
    dots = resolve(...)
    if (length(dots)) defaults <<- merge_list(defaults, dots)
    invisible(NULL)
  }
  merge = function(values) merge_list(defaults, values)
  delete = function(keys) {
    for (k in keys) defaults[[k]] <<- NULL
  }
  restore = function(target = value) defaults <<- target
  append = function(...) {
    dots = resolve(...)
    for (i in names(dots)) dots[[i]] <- c(defaults[[i]], dots[[i]])
    set2(dots)
  }
  list(get = get, set = set, delete = delete, append = append, merge = merge, restore = restore)
}

opts_chunk = new_defaults(list(
  eval = TRUE, echo = TRUE, results = 'markup',
  tidy = FALSE, tidy.opts = NULL,
  collapse = FALSE, prompt = FALSE, comment = '##', highlight = TRUE,
  size = 'normalsize', background = '#F7F7F7',
  strip.white = TRUE,
  cache = FALSE, cache.path = 'cache/', cache.rebuild = FALSE, cache.comments = TRUE,
  dependson = NULL,
  fig.keep = 'high', fig.path = 'figure/',
  dev = NULL, dev.args = NULL, dpi = 72, fig.ext = NULL,
  fig.width = 7, fig.height = 7,
  fig.env = NULL, fig.cap = NULL, fig.lp = 'fig:',
  fig.pos = '', out.width = NULL, out.height = NULL,
  out.extra = NULL, interval = 1, aniopts = 'controls,loop',
  warning = TRUE, error = TRUE, message = TRUE,
  render = NULL,
  ref.label = NULL, child = NULL, engine = 'R', split = FALSE, include = TRUE,
  minted = FALSE, minted_style = NULL
))

opts_current = new_defaults()
opts_current$restore(opts_chunk$get())

opts_knit = new_defaults(list(
  progress = TRUE, verbose = FALSE, global.device = FALSE, global.par = FALSE,
  eval.after = c('fig.cap'),
  root.dir = NULL, child.path = '',
  concordance = FALSE, unnamed.chunk.label = 'unnamed-chunk',
  out.format = NULL, child = FALSE, parent = FALSE,
  aliases = NULL, resolve_input = TRUE,
  header = c(highlight = '', framed = ''),
  minted_style = NULL, engine = 'pdflatex'
))

opts_hooks = new_defaults(list())

opts_template = new_defaults()

set_alias = function(...) {
  opts_knit$set(aliases = c(...))
}

merge_list = function(x, y) {
  x[names(y)] = y
  x
}

merge_vector = function(x, y, fun = `%n%`) {
  if (is.null(names(y))) {
    utils::head(c(x, y), length(x))
  } else {
    x[names(y)] = mapply(
      function(a, b) if (is.null(a)) b else if (identical(names(a), names(b))) a else b,
      x[names(y)], y, SIMPLIFY = FALSE
    )
    x
  }
}

`%n%` = function(x, y) if (is.null(x)) y else x

parse_only = function(code) {
  if (length(code) > 1) code = one_string(code)
  parse(text = code)
}

one_string = function(x, ...) {
  paste(x, collapse = '\n', ...)
}

split_lines = function(x) {
  x = gsub('\r\n', '\n', x, useBytes = TRUE)
  x = gsub('\r$', '', x, useBytes = TRUE)
  strsplit(x, '\n')[[1]]
}

is_blank = function(x) {
  if (length(x)) all(grepl('^\\s*$', x)) else TRUE
}

sc_split = function(string) {
  if (is.call(string)) string = eval(string)
  if (is.numeric(string) || length(string) != 1L) return(string)
  trimws(strsplit(string, ';|,')[[1]])
}

valid_path = function(prefix, label) {
  if (length(prefix) == 0L || is.na(prefix) || prefix == 'NA') prefix = ''
  paste0(prefix, label)
}
