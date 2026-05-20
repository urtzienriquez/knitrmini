knit_code = new_defaults()
dep_list = new_defaults()
.knitEnv = new.env(parent = emptyenv())
.knitEnv$labels = character()
.knitEnv$terminate = NULL
.knitEnv$input.dir = NULL

split_file = function(lines, set.preamble = TRUE, patterns = knit_patterns$get()) {
  n = length(lines)
  chunk.begin = patterns$chunk.begin
  chunk.end = patterns$chunk.end
  if (is.null(chunk.begin) || is.null(chunk.end))
    return(list(parse_inline(lines, patterns)))

  groups = divide_chunks(lines, chunk.begin, chunk.end)
  lapply(seq_along(groups), function(i) {
    g = groups[[i]]
    block = grepl(chunk.begin, g[1])
    if (block) {
      n = length(g)
      if (n >= 2 && grepl(chunk.end, g[n])) g = g[-n]
      g = strip_block(g, patterns$chunk.code)
      params.src = if (group_pattern(chunk.begin)) {
        trimws(gsub(chunk.begin, '\\1', g[1]))
      } else ''
      parse_block(g[-1], g[1], params.src, markdown_mode = FALSE)
    } else parse_inline(g, patterns)
  })
}

divide_chunks = function(x, begin, end, md = TRUE) {
  i = group_indices(grepl(begin, x), grepl(end, x), x, md)
  unname(split(x, i))
}

group_indices = function(begin, end, x, md = TRUE) {
  n = length(x)
  if (md) {
    i = begin | end
    i = .Call(stats::C_utf8ToInt, paste(i, collapse = ' '))
  } else {
    ints = cumsum(begin | end)
    i1 = which(begin)
    i2 = which(end)
    n1 = length(i1)
    n2 = length(i2)
    if (n1 && n1 == n2) {
      ints[i1] = ints[i2] = seq_len(n1)
    }
    i = ints
  }
  i
}

strip_block = function(x, prefix = NULL) {
  if (!is.null(prefix) && (length(x) > 1)) {
    x[-1L] = sub(prefix, '', x[-1L])
    spaces = min(attr(regexpr("^ *", x[-1L]), "match.length"))
    if (spaces > 0) x[-1L] = substring(x[-1L], spaces + 1)
  }
  x
}

parse_block = function(code, header, params.src, markdown_mode = FALSE) {
  params = params.src
  if (!markdown_mode) {
    params = gsub('^\\s*,*\\s*|\\s*,*\\s*$', '', params)
  }
  params = xfun::csv_options(params)
  if (is.null(params$label)) params$label = unnamed_chunk()
  label = params$label
  .knitEnv$labels = c(.knitEnv$labels, label)
  if (length(code) || length(params[['file']]) || length(params[['code']])) {
    if (label %in% names(knit_code$get())) {
      params$label = label = unnamed_chunk(label)
    }
    code = as.character(code)
    knit_code$set(setNames(list(structure(code, chunk_opts = params)), label))
  }
  if (!is.null(deps <- params$dependson)) {
    deps = sc_split(deps)
    if (is.character(deps)) {
      for (i in deps) dep_list$set(setNames(list(c(dep_list$get(i), label)), i))
    }
  }
  structure(class = 'block', list(
    params = params, params.src = params.src
  ))
}

parse_inline = function(input, patterns) {
  inline.code = patterns$inline.code
  inline.comment = patterns$inline.comment
  if (!is.null(inline.comment)) {
    idx = grepl(inline.comment, input)
    input[idx] = gsub(inline.code, '\\1', input[idx])
  }
  input = one_string(input)
  loc = cbind(start = numeric(0), end = numeric(0))
  if (group_pattern(inline.code)) loc = str_locate(input, inline.code)[[1]]
  code2 = character()
  if (nrow(loc)) {
    m = gregexec(inline.code, input, perl = TRUE)[[1]]
    if (length(m) > 0 && attr(m, 'match.length')[1] > 0) {
      code2 = substring(input, m[2, ], m[2, ] + attr(m, 'capture.length')[1, ] - 1)
    }
  }
  structure(list(
    input = input, location = loc, code = code2
  ), class = 'inline')
}

unnamed_chunk = function(prefix = NULL) {
  if (is.null(prefix)) prefix = opts_knit$get('unnamed.chunk.label')
  paste(prefix, chunk_counter(), sep = '-')
}

chunk_counter = local({
  n = 0L
  function(reset = FALSE) {
    if (reset) return(n <<- 0L)
    n <<- n + 1L
    n
  }
})

plot_counter = local({
  n = 1L
  function(reset = FALSE) {
    if (reset) return(n <<- 1L)
    n <<- n + 1L
    n - 1L
  }
})

all_labels = function() {
  .knitEnv$labels
}

str_locate = function(x, pattern, all = TRUE) {
  out = (if (all) gregexpr else regexpr)(pattern, x, perl = TRUE)
  if (all) lapply(out, function(x) {
    len = attr(x, 'match.length')
    if (length(x) == 1 && x == -1) x = integer()
    cbind(start = x, end = x + len - 1L)
  }) else {
    len = attr(out, 'match.length')
    if (length(out) == 1 && out == -1) out = integer()
    cbind(start = out, end = out + len - 1L)
  }
}

str_replace = function(x, pos, value) {
  if (length(x) != 1) stop("Only a character scalar is supported.")
  m = rbind(pos[, 1] - 1, pos[, 2] + 1)
  m = matrix(c(1, m, nchar(x)), nrow = 2)
  y = substring(x, m[1, ], m[2, ])
  paste(rbind(y, c(value, '')), collapse = '')
}
