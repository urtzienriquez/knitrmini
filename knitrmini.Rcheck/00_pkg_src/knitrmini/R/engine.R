process_group = function(x) {
  if (inherits(x, 'block')) call_block(x) else {
    x = call_inline(x)
    knit_hooks$get('text')(x)
  }
}

call_block = function(block) {
  af = opts_knit$get('eval.after')
  al = opts_knit$get('aliases')
  if (!is.null(al) && !is.null(af)) af = c(af, names(al[af %in% al]))

  params = opts_chunk$merge(block$params)
  for (o in setdiff(names(params), af)) {
    params[o] = list(eval_lang(params[[o]]))
    if (o %in% names(block$params)) block$params[o] = params[o]
  }

  label = ref.label = params$label
  if (!is.null(params$ref.label)) {
    ref.label = sc_split(params$ref.label)
  }
  params[['code']] = get_code(params, label, ref.label)

  if (opts_knit$get('progress')) {
    cat('  |', label, '\n')
  }

  params$params.src = block$params.src

  if (!is.null(params$child)) {
    if (!is_blank(params[['code']]) && getOption('knitr.child.warning', TRUE)) warning(
      "The chunk '", params$label, "' has the 'child' option, ",
      "and this code chunk must be empty. Its code will be ignored."
    )
    if (!params$eval) return('')
    cmds = lapply(sc_split(params$child), knit_child, options = block$params)
    out = one_string(unlist(cmds))
    return(out)
  }

  block_exec(params)
}

get_code = function(params, label, ref.label) {
  code = params[['code']]
  file = params[['file']]
  if (is.null(code) && is.null(file))
    return(unlist(knit_code$get(ref.label), use.names = FALSE))
  if (!is.null(file)) code = in_input_dir(xfun::read_all(file))
  set_code(label, code)
  code
}

set_code = function(label, code) {
  res = knit_code$get(label)
  attributes(code) = attributes(res)
  knit_code$set(setNames(list(code), label))
}

block_exec = function(options) {
  if (options$engine == 'R') return(eng_r(options))
  options$engine
}

eng_r = function(options) {
  env = knit_global()
  obj.before = ls(envir = env, all.names = TRUE)

  keep = options$fig.keep
  if (is.logical(keep)) keep = which(keep)

  tmp.fig = tempfile()
  on.exit(unlink(tmp.fig), add = TRUE)

  if (!opts_knit$get('global.device') || is.null(dev.list())) {
    if (!is.null(dev.list())) {
      dv0 = dev.cur()
      on.exit(dev.set(dv0), add = TRUE)
    }
    chunk_device(options, keep != 'none', tmp.fig)
    dv = dev.cur()
    if (!opts_knit$get('global.device')) on.exit(dev.off(dv), add = TRUE)
  }

  code = options$code
  echo = options$echo

  if (keep != 'none') options$fig.ext = dev2ext(options)

  evaluate = knit_hooks$get('evaluate')

  res = if (is_blank(code)) list() else if (isFALSE(options$eval)) {
    as.source(code)
  } else in_input_dir(
    evaluate(
      code, envir = env, new_device = FALSE,
      keep_warning = if (is.numeric(options$warning)) TRUE else options$warning,
      keep_message = if (is.numeric(options$message)) TRUE else options$message,
      stop_on_error = if (is.numeric(options$error)) options$error else {
        if (options$error && options$include) 0L else 2L
      }
    )
  )

  if (!isFALSE(options$eval)) {
    for (o in opts_knit$get('eval.after'))
      options[o] = list(eval_lang(options[[o]], env))
  }

  if (isFALSE(echo)) {
    res = Filter(Negate(evaluate::is.source), res)
  }

  if (options$results == 'hide') res = Filter(Negate(is.character), res)

  if (options$results == 'hold') {
    i = vapply(res, is.character, logical(1))
    if (any(i)) res = c(res[!i], merge_character(res[i]))
  }

  res = filter_evaluate(res, options$warning, evaluate::is.warning)
  res = filter_evaluate(res, options$message, evaluate::is.message)

  if (length(res)) {
    fig.num = sum(sapply(res, function(x) {
      if (is_plot_output(x)) 1 else 0
    }))
  } else fig.num = 0L
  options$fig.num = fig.num

  output = unlist(sew(res, options))
  output = paste(c(output), collapse = '')
  output = knit_hooks$get('chunk')(output, options)

  if (options$include) output else ''
}

chunk_device = function(options, record = TRUE, tmp = tempfile()) {
  width = options$fig.width[1L]
  height = options$fig.height[1L]
  dev = options$dev %n% 'pdf'
  dpi = options$dpi %n% 72L
  if (identical(dev, 'pdf')) {
    grDevices::pdf(tmp, width = width, height = height)
  } else if (identical(dev, 'png')) {
    grDevices::png(tmp, width = width, height = height, units = 'in', res = dpi)
  } else if (identical(dev, 'svg')) {
    grDevices::svg(tmp, width = width, height = height)
  } else {
    grDevices::pdf(tmp, width = width, height = height)
  }
  dev.control(displaylist = if (record) 'enable' else 'inhibit')
}

dev2ext = function(options) {
  dev = options$dev %n% 'pdf'
  switch(dev, pdf = 'pdf', png = 'png', svg = 'svg', jpeg = 'jpg', 'pdf')
}

sew = function(res, options, inline = FALSE) {
  if (inline) return(knit_hooks$get('inline')(res))
  lapply(res, function(x) {
    if (evaluate::is.source(x)) {
      knit_hooks$get('source')(x$src, options)
    } else if (is_plot_output(x)) {
      save_plot(x, options)
    } else if (evaluate::is.warning(x)) {
      knit_hooks$get('warning')(x$message, options)
    } else if (evaluate::is.message(x)) {
      knit_hooks$get('message')(x$message, options)
    } else if (evaluate::is.error(x)) {
      knit_hooks$get('error')(x$message, options)
    } else if (is.character(x)) {
      knit_hooks$get('output')(x, options)
    } else ''
  })
}

is_plot_output = function(x) {
  evaluate::is.recordedplot(x) || inherits(x, 'knit_image_paths')
}

save_plot = function(plot, options) {
  fig.cur = plot_counter()
  fig.num = options$fig.num %n% 1L
  ext = options$fig.ext %n% dev2ext(options)
  fig.path = options$fig.path %n% 'figure/'
  if (!dir.exists(fig.path)) dir.create(fig.path, recursive = TRUE, showWarnings = FALSE)
  prefix = paste0(fig.path, options$label)
  fname = paste0(prefix, '-', fig.cur, '.', ext)
  if (evaluate::is.recordedplot(plot)) {
    replay.plot = function(x) {
      for (i in seq_along(x)) {
        if (is.call(x[[i]])) eval(x[[i]], envir = .GlobalEnv)
      }
    }
    tryCatch({
      dev = dev.cur()
      pdf(fname, width = options$fig.width[1L], height = options$fig.height[1L])
      tryCatch({
        grDevices:::replayPlot(plot)
      }, error = function(e) {
        print(plot)
      })
      dev.off()
      dev.set(dev)
    }, error = function(e) {
      dev.set(dev)
      warning('Failed to save plot: ', e$message)
    })
  }
  knit_hooks$get('plot')(fname, options)
}

merge_character = function(res) {
  if ((n <- length(res)) <= 1) return(res)
  k = NULL
  for (i in 1:(n - 1)) {
    cls = class(res[[i]])
    if (identical(cls, class(res[[i + 1]]))) {
      res[[i + 1]] = paste0(res[[i]], res[[i + 1]])
      class(res[[i + 1]]) = cls
      k = c(k, i)
    }
  }
  if (length(k)) res = res[-k]
  res
}

filter_evaluate = function(res, opt, test) {
  if (length(res) == 0 || !is.numeric(opt) || !any(idx <- sapply(res, test)))
    return(res)
  idx = which(idx)
  idx = setdiff(idx, na.omit(idx[opt]))
  if (length(idx) == 0) res else res[-idx]
}

as.source = function(code) {
  list(structure(list(src = code), class = 'source'))
}

call_inline = function(block) {
  if (opts_knit$get('progress')) cat('  | inline\n')
  in_input_dir(inline_exec(block))
}

inline_exec = function(block, envir = knit_global(), hook = knit_hooks$get('inline'),
                       hook_eval = knit_hooks$get('evaluate.inline')) {
  code = block$code
  input = block$input
  if ((n <- length(code)) == 0) return(input)
  loc = block$location

  ans = character(n)
  for (i in 1:n) {
    res = hook_eval(code[i], envir)
    if (length(res)) ans[i] = paste(hook(res), collapse = '')
  }
  if (nrow(loc) > 0) {
    str_replace(input, loc, ans)
  } else input
}

knit_child = function(child, options = list()) {
  child_path = paste0(opts_knit$get('child.path'), child)
  if (!file.exists(child_path)) {
    child_path = file.path(input_dir(), child_path)
  }
  if (!file.exists(child_path)) stop('Child document not found: ', child)

  optk = opts_knit$get()
  opth = opts_knit$get('header')
  on.exit(opts_knit$restore(optk), add = TRUE)
  opts_knit$set(child = TRUE, parent = FALSE)

  content = xfun::read_utf8(child_path)
  patterns = knit_patterns$get()
  groups = split_file(content, patterns = patterns)

  output = character(length(groups))
  for (i in seq_along(groups)) {
    output[i] = process_group(groups[[i]])
  }
  one_string(output)
}

child_mode = function() isTRUE(opts_knit$get('child'))

in_input_dir = function(expr) {
  in_dir(input_dir(), expr)
}

input_dir = function() {
  root = opts_knit$get('root.dir')
  root %n% .knitEnv$input.dir %n% '.'
}

in_dir = function(dir, expr) {
  owd = setwd(dir)
  on.exit(setwd(owd))
  force(expr)
}

eval_lang = function(x, envir = knit_global()) {
  if (is.expression(x) || is.call(x) || is.name(x)) {
    tryCatch(eval(x, envir = envir), error = function(e) x)
  } else x
}

parse_only = function(code) {
  if (length(code) > 1) code = one_string(code)
  parse(text = code)
}

knit_print = function(x, ...) {
  UseMethod('knit_print')
}

knit_print.default = function(x, inline = FALSE, options = NULL) {
  if (inline) x else {
    if (is.character(x)) paste(x, collapse = '\n') else {
      paste(capture.output(print(x)), collapse = '\n')
    }
  }
}

hilight_source = function(x, format = 'latex', options = NULL) {
  if (!requireNamespace('highr', quietly = TRUE)) return(x)
  if (format == 'latex') {
    highr::hilight(x, format = 'latex')
  } else x
}
