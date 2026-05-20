knit = function(
  input, output = NULL, text = NULL, quiet = FALSE,
  envir = parent.frame(), encoding = 'UTF-8'
) {
  in.file = !missing(input) && is.character(input)

  if (child_mode()) {
    setwd(opts_knit$get('output.dir') %n% '.')
    if (in.file && !is_abs_path(input)) {
      input = paste0(opts_knit$get('child.path'), input)
      input = file.path(input_dir(), input)
    }
    optk = opts_knit$get()
    on.exit(opts_knit$restore(optk), add = TRUE)
    opts_knit$set(progress = opts_knit$get('progress') && !quiet)
    quiet = !opts_knit$get('progress')
  } else {
    on.exit(chunk_counter(reset = TRUE), add = TRUE)
    on.exit(plot_counter(reset = TRUE), add = TRUE)
    on.exit({
      if (opts_knit$get('global.device')) dev.off()
    }, add = TRUE)
    knit_code$restore()
    dep_list$restore()
    .knitEnv$labels = character()
    .knitEnv$terminate = NULL
  }

  if (in.file) {
    input <- normalizePath(input, mustWork = FALSE)
    .knitEnv$input.dir <- dirname(input)
    text = xfun::read_utf8(input)
  } else input = ''

  txt = one_string(text)

  ext = tolower(xfun::file_ext(input))
  if (is.null(detect_pattern(txt, ext))) {
    stop("Failed to detect the pattern for the input file.")
  }

  if (!child_mode()) {
    pattern_name = detect_pattern(txt, ext)
    if (!is.null(pattern_name)) knit_patterns$restore(all_patterns[[pattern_name]])
    render_latex()
  }

  if (!quiet) cat("Processing ", input, "\n", sep = "")

  knit_global_set(envir)

  groups = split_file(text, patterns = knit_patterns$get())

  output = character(length(groups))
  for (i in seq_along(groups)) {
    if (isTRUE(.knitEnv$terminate)) break
    output[i] = process_group(groups[[i]])
  }

  if (any(nzchar(output))) {
    output = one_string(output[!is.na(output)])
  } else output = ''

  output = paste(text_to_tex(output), collapse = '\n')

  if (opts_knit$get('resolve_input')) {
    output = resolve_inputs(output)
  }

  if (in.file) {
    if (is.null(output)) output <- guess_output(input)
    output_path = if (is.character(output) && length(output) == 1 && !is.na(output)) output else guess_output(input)
    xfun::write_utf8(output, output_path)
    if (!quiet) cat("Output: ", output_path, "\n", sep = "")
    invisible(output_path)
  } else {
    output
  }
}

guess_output = function(input) {
  ext = xfun::file_ext(input)
  if (ext %in% c('rnw', 'snw', 'stex')) {
    xfun::sans_ext(input) %n% '.tex'
  } else {
    paste0(input, '-out.tex')
  }
}

knit2pdf = function(
  input, output = NULL, compiler = 'pdflatex', quiet = FALSE,
  envir = parent.frame(), ...
) {
  tex_file = knit(input, output = output, quiet = quiet, envir = envir, ...)
  if (!file.exists(tex_file)) stop("LaTeX file not found: ", tex_file)
  if (!requireNamespace('tinytex', quietly = TRUE)) {
    stop("tinytex is required for knit2pdf. Install with install.packages('tinytex')")
  }
  if (!quiet) cat("Compiling ", tex_file, " -> .pdf\n", sep = "")
  tinytex::latexmk(tex_file, engine = compiler)
}

knit_global_set = function(envir) {
  assign('.knitGlobal', envir, envir = .knitEnv)
}

is_abs_path = function(x) {
  grepl('^(/|[A-Za-z]:)', x)
}

text_to_tex = function(text) {
  text = gsub('\r\n', '\n', text, useBytes = TRUE)
  split_lines(text)
}

resolve_inputs = function(doc) {
  doc = one_string(doc)
  max_depth = 10
  for (i in seq_len(max_depth)) {
    m = regexpr('\\\\(input|include)\\{([^}]+)\\}', doc, perl = TRUE)
    if (m == -1) break
    start = as.integer(m)
    end = start + attr(m, 'match.length') - 1
    cmd = substr(doc, start, end)
    fname = gsub('\\\\(input|include)\\{([^}]+)\\}', '\\2', cmd)
    if (!grepl('\\.tex$', fname)) fname <- paste0(fname, '.tex')
    if (file.exists(fname)) {
      content <- xfun::read_utf8(fname)
      content <- one_string(content)
      content <- resolve_inputs(content)
      doc <- paste0(substr(doc, 1, start - 1), content, substr(doc, end + 1, nchar(doc)))
    } else {
      break
    }
  }
  doc
}
