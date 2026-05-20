.inline.hook = function(x) {
  if (is.numeric(x)) x = round_digits(x)
  paste(as.character(x), collapse = ', ')
}

.inline.hook.tex = function(x) {
  if (is.numeric(x)) {
    x = format_sci(x, 'latex')
    i = grep('[}]', x)
    x[i] = sprintf('\\ensuremath{%s}', x[i])
    if (getOption('OutDec') != '.') x = sprintf('\\text{%s}', x)
  }
  .inline.hook(x)
}

.verb.hook = function(x) {
  one_string(c('\\begin{verbatim}', sub('\n$', '', x), '\\end{verbatim}', ''))
}

.chunk.hook.tex = function(x, options) {
  ai = output_asis(x, options)
  if (ai) return(x)
  size = if (options$size == 'normalsize') '' else sprintf('\\%s', options$size)
  if (nzchar(size)) {
    sprintf('{%s\n%s\n}', size, x)
  } else x
}

hooks_latex = function() {
  list(
    source = function(x, options) {
      if (isFALSE(options$highlight)) return(.verb.hook(x))
      x = hilight_source(x, 'latex', options)
      one_string(c('\\begin{alltt}', x, '\\end{alltt}', ''))
    },
    output = function(x, options) {
      if (output_asis(x, options)) return(x)
      .verb.hook(x)
    },
    warning = .color.block('\\color{warningcolor}{', '}'),
    message = .color.block('\\itshape\\color{messagecolor}{', '}'),
    error = .color.block('\\bfseries\\color{errorcolor}{', '}'),
    inline = .inline.hook.tex,
    chunk = .chunk.hook.tex,
    plot = function(x, options) {
      hook_plot_tex(x, options)
    },
    text = identity,
    evaluate = function(...) evaluate::evaluate(...),
    evaluate.inline = function(code, envir = knit_global()) {
      v = withVisible(eval(parse_only(code), envir = envir))
      if (v$visible) knit_print(v$value, inline = TRUE, options = opts_chunk$get())
    },
    document = identity
  )
}

knit_hooks = new_defaults(.default.hooks)

.default.hooks = list(
  source = .out.hook, output = .out.hook, warning = .out.hook,
  message = .out.hook, error = .out.hook, plot = .plot.hook,
  inline = .inline.hook, chunk = .out.hook, text = identity,
  evaluate.inline = function(code, envir = knit_global()) {
    v = withVisible(eval(parse_only(code), envir = envir))
    if (v$visible) knit_print(v$value, inline = TRUE, options = opts_chunk$get())
  },
  evaluate = function(...) evaluate::evaluate(...),
  document = identity
)

.out.hook = function(x, options) x
.plot.hook = function(x, options) paste(x, collapse = '.')

knit_hooks$set(.default.hooks)

render_latex = function() {
  opts_chunk$set(out.width = '\\maxwidth', dev = 'pdf')
  opts_knit$set(out.format = 'latex')
  h = opts_knit$get('header')
  if (!nzchar(h['framed'])) set_header(framed = .header.framed)
  if (!nzchar(h['highlight'])) set_header(highlight = .header.hi.tex)
  knit_hooks$set(hooks_latex())
}

hook_plot_tex = function(x, options) {
  rw = options$resize.width
  rh = options$resize.height
  rc = options$resize.command

  resize1 = resize2 = ''
  if (is.null(rc)) {
    if (!is.null(rw) || !is.null(rh)) {
      resize1 = sprintf('\\resizebox{%s}{%s}{', rw %n% '!', rh %n% '!')
      resize2 = '} '
    }
  } else {
    resize1 = paste0('\\', rc, '{')
    resize2 = '} '
  }

  a = options$fig.align
  align1 = switch(a, left = '\n\n', center = '\n\n{\\centering ', right = '\n\n\\hfill{}', '\n')
  align2 = switch(a, left = '\\hfill{}\n\n', center = '\n\n}\n\n', right = '\n\n', '')

  cap = options$fig.cap
  fig1 = fig2 = ''
  if (length(cap) && !is.na(cap)) {
    lab = paste0(options$fig.lp, options$label)
    pos = options$fig.pos
    if (pos != '' && !grepl('^[[{]', pos)) pos = sprintf('[%s]', pos)
    fig1 = sprintf('\\begin{%s}%s\n', options$fig.env %n% 'figure', pos)
    fig2 = sprintf('\\caption{%s}\n\\label{%s}\n\\end{%s}\n', cap, lab, options$fig.env %n% 'figure')
  }

  ow = options$out.width
  if (is.numeric(ow)) ow = paste0(ow, 'px')
  size = paste(c(
    sprintf('width=%s', ow), sprintf('height=%s', options$out.height),
    options$out.extra
  ), collapse = ',')

  includegraphics = if (nzchar(size)) {
    sprintf('\\includegraphics[%s]{%s} ', size, sans_ext(x))
  } else {
    sprintf('\\includegraphics{%s} ', sans_ext(x))
  }

  paste0(fig1, align1, resize1, includegraphics, resize2, align2, fig2)
}

.color.block = function(color1 = '', color2 = '') {
  function(x, options) {
    x = gsub('\n*$', '', x)
    x = escape_latex(x, newlines = TRUE, spaces = TRUE)
    x = gsub('"', '"{}', x)
    sprintf('\n\n{\\ttfamily\\noindent%s%s%s}', color1, x, color2)
  }
}

output_asis = function(x, options) {
  is_blank(x) || identical(options$results, 'asis')
}

escape_latex = function(s, newlines = FALSE, spaces = FALSE) {
  s = gsub('\\', '\\textbackslash{}', s, fixed = TRUE)
  s = gsub('{', '\\{', s, fixed = TRUE)
  s = gsub('}', '\\}', s, fixed = TRUE)
  s = gsub('$', '\\$', s, fixed = TRUE)
  s = gsub('%', '\\%', s, fixed = TRUE)
  s = gsub('&', '\\&', s, fixed = TRUE)
  s = gsub('#', '\\#', s, fixed = TRUE)
  s = gsub('_', '\\_', s, fixed = TRUE)
  s = gsub('^', '\\^{}', s, fixed = TRUE)
  s = gsub('~', '\\textasciitilde{}', s, fixed = TRUE)
  s
}

sans_ext = function(x) {
  sub('([^.]+)\\.[[:alnum:]]+$', '\\1', x)
}

round_digits = function(x) {
  if (getOption('knitr.digits.signif', FALSE)) format(x) else {
    as.character(round(x, getOption('digits')))
  }
}

format_sci_one = function(x, format = 'latex') {
  if (!(class(x)[1] == 'numeric') || is.na(x) || x == 0) return(as.character(x))
  if (is.infinite(x)) {
    return(sprintf("%s\\infty{}", ifelse(x < 0, "-", "")))
  }
  if (abs(lx <- floor(log10(abs(x)))) < getOption('scipen') + 4L)
    return(round_digits(x))
  b = round_digits(x / 10^lx)
  sprintf('%s%s10^{%s}', b, '\\times ', lx)
}

format_sci = function(x, ...) {
  if (inherits(x, 'roman')) return(as.character(x))
  vapply(x, format_sci_one, character(1L), ..., USE.NAMES = FALSE)
}
