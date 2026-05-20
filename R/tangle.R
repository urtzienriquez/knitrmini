tangle <- function(input, output = NULL, documentation = 1L, text = NULL, quiet = FALSE) {
  in.file <- !missing(input) && is.character(input)

  if (in.file) {
    input <- normalizePath(input, mustWork = FALSE)
    .knitEnv$input.dir <- dirname(input)
    text <- xfun::read_utf8(input)
  } else {
    input <- ""
  }

  txt <- one_string(text)

  ext <- tolower(xfun::file_ext(input))
  if (is.null(detect_pattern(txt, ext))) {
    stop("Failed to detect the pattern for the input file.")
  }

  pattern_name <- detect_pattern(txt, ext)
  if (!is.null(pattern_name)) knit_patterns$restore(all_patterns[[pattern_name]])

  if (!quiet) cat("Processing ", input, "\n", sep = "")

  optk <- opts_knit$get()
  on.exit(opts_knit$restore(optk), add = TRUE)
  opts_knit$set(tangle = TRUE, documentation = documentation)

  groups <- split_file(text, patterns = knit_patterns$get())

  n <- length(groups)
  res <- character(n)
  for (i in seq_len(n)) {
    res[i] <- process_tangle(groups[[i]])
  }

  out_text <- if (any(nzchar(res))) {
    one_string(res[!is.na(res)])
  } else {
    ""
  }

  out_lines <- split_lines(out_text)
  while (length(out_lines) && !nzchar(trimws(out_lines[1]))) {
    out_lines <- out_lines[-1]
  }
  while (length(out_lines) && !nzchar(trimws(out_lines[length(out_lines)]))) {
    out_lines <- out_lines[-length(out_lines)]
  }
  out_text <- one_string(out_lines)

  if (in.file) {
    out_path <- output %n% paste0(xfun::sans_ext(input), ".R")
    xfun::write_utf8(out_text, out_path)
    if (!quiet) cat("Output: ", out_path, "\n", sep = "")
    invisible(out_path)
  } else {
    out_text
  }
}

process_tangle <- function(x) {
  if (inherits(x, "block")) tangle_block(x) else tangle_inline(x)
}

tangle_block <- function(block) {
  params <- opts_chunk$merge(block$params)

  for (o in c("tangle", "eval", "child")) {
    if (inherits(try(params[o] <- list(eval_lang(params[[o]])), silent = TRUE),
                 "try-error")) {
      params[["tangle"]] <- FALSE
    }
  }

  if (isFALSE(params$tangle)) return("")

  label <- params$label
  ev <- params$eval

  if (!isFALSE(ev) && !is.null(params$child)) {
    cmds <- lapply(sc_split(params$child), knit_child)
    return(one_string(unlist(cmds)))
  }

  code <- knit_code$get(label)
  if (is.null(code) || length(code) == 0L) return("")

  if (params$engine != "R") {
    return(one_string(paste0("# ", code)))
  }

  code <- tangle_mask(code, ev, block$params$error)

  doc <- opts_knit$get("documentation")
  if (doc == 0L) return(one_string(code))

  label_code(code, block)
}

tangle_inline <- function(x) {
  doc <- opts_knit$get("documentation")
  output <- if (doc == 2L) {
    paste("#'", gsub("\n", "\n#' ", x$input, fixed = TRUE))
  } else {
    ""
  }

  code <- x$code
  if (length(code) == 0L) return(output)

  if (isTRUE(getOption("knitr.tangle.inline", FALSE))) {
    output <- c(output, code)
  }

  idx <- grepl("knit_child\\(.+\\)", code)
  if (any(idx)) {
    cout <- sapply(code[idx], function(z) eval(parse_only(z)))
    output <- c(output, cout, "")
  }

  one_string(output)
}

tangle_mask <- function(code, eval, error) {
  if (isFALSE(eval)) code <- paste0("# ", code)
  if (isTRUE(error)) code <- c("try({", code, "})")
  code
}

label_code <- function(code, block) {
  header <- paste0("## ----", block$params.src, "----")
  one_string(c(header, code, ""))
}
