knit <- function(
  input, output = NULL, text = NULL, quiet = FALSE,
  envir = parent.frame(), encoding = "UTF-8",
  compile = TRUE, engine = NULL,
  minted_style = NULL, clean = FALSE
) {
  in.file <- !missing(input) && is.character(input)

  if (child_mode()) {
    setwd(opts_knit$get("output.dir") %n% ".")
    if (in.file && !is_abs_path(input)) {
      input <- paste0(opts_knit$get("child.path"), input)
      input <- file.path(input_dir(), input)
    }
    optk <- opts_knit$get()
    on.exit(opts_knit$restore(optk), add = TRUE)
    opts_knit$set(progress = opts_knit$get("progress") && !quiet)
    quiet <- !opts_knit$get("progress")
  } else {
    on.exit(chunk_counter(reset = TRUE), add = TRUE)
    on.exit(plot_counter(reset = TRUE), add = TRUE)
    on.exit(
      {
        if (opts_knit$get("global.device")) dev.off()
      },
      add = TRUE
    )
    opts_chunk$restore()
    opts_knit$restore()
    knit_code$restore()
    dep_list$restore()
    .knitEnv$labels <- character()
    .knitEnv$terminate <- NULL
  }

  if (in.file) {
    input <- normalizePath(input, mustWork = FALSE)
    .knitEnv$input.dir <- dirname(input)
    text <- xfun::read_utf8(input)
  } else {
    input <- ""
  }

  if (!child_mode()) {
    style <- minted_style %n% opts_chunk$get("minted_style")
    has_user_knitbg <- grepl(
      "\\\\definecolor\\{knitbg\\}",
      strip_latex_comments(one_string(text))
    )
    opts_knit$set(minted_style = style)
    .knitEnv$has_user_knitbg <- has_user_knitbg
  }

  if (!child_mode() && !is.null(text) && length(text)) {
    check_color_definition(strip_latex_comments(one_string(text)))
  }

  txt <- one_string(text)

  ext <- tolower(xfun::file_ext(input))
  if (is.null(detect_pattern(txt, ext))) {
    stop("Failed to detect the pattern for the input file.")
  }

  if (!child_mode()) {
    pattern_name <- detect_pattern(txt, ext)
    if (!is.null(pattern_name)) knit_patterns$restore(all_patterns[[pattern_name]])
    render_latex()
    set_preamble(text)
  }

  if (!quiet) cat("Processing ", input, "\n", sep = "")

  knit_global_set(envir)

  groups <- split_file(text, patterns = knit_patterns$get())

  output <- character(length(groups))
  for (i in seq_along(groups)) {
    if (isTRUE(.knitEnv$terminate)) break
    output[i] <- process_group(groups[[i]])
  }

  out_text <- if (any(nzchar(output))) {
    one_string(output[!is.na(output)])
  } else {
    ""
  }
  out_text <- paste(text_to_tex(out_text), collapse = "\n")

  out_text <- insert_header(out_text)

  if (opts_knit$get("resolve_input")) {
    out_text <- resolve_inputs(out_text)
  }

  out_text <- clean_output(out_text)

  if (in.file) {
    out_path <- guess_output(input)
    xfun::write_utf8(out_text, out_path)
    if (!quiet) cat("Output: ", out_path, "\n", sep = "")

    if (!quiet && !child_mode()) {
      style <- opts_knit$get("minted_style") %n% opts_chunk$get("minted_style")
      has_style <- !is.null(style) && is.character(style) && nzchar(style)
      if (has_style && !isTRUE(.knitEnv$has_user_knitbg)) {
        cat("Note: background removed for minted style ", dQuote(style), ".\n",
          "      To add a custom background, put\n",
          "        \\definecolor{knitbg}{rgb}{0.969, 0.969, 0.969}\n",
          "      in your .Rnw preamble (before \\begin{document}).\n",
          sep = ""
        )
      }
    }

    if (compile) {
      if (!quiet) cat("Compiling ", out_path, " -> .pdf\n", sep = "")
      pdf_path <- compile_pdf(out_path,
        engine = engine %n% opts_knit$get("engine") %n% "pdflatex",
        quiet = quiet
      )
      if (clean) {
        aux_files <- Sys.glob(paste0(xfun::sans_ext(out_path), ".*"))
        aux_files <- aux_files[!grepl("\\.(tex|pdf|Rnw)$", aux_files)]
        unlink(aux_files)
      }
      invisible(pdf_path)
    } else {
      invisible(out_path)
    }
  } else {
    out_text
  }
}

guess_output <- function(input) {
  ext <- tolower(xfun::file_ext(input))
  if (ext %in% c("rnw", "snw", "stex")) {
    paste0(xfun::sans_ext(input), ".tex")
  } else {
    paste0(input, "-out.tex")
  }
}

compile_pdf <- function(tex_file, engine = "pdflatex", bib_engine = NULL, quiet = FALSE) {
  tex_file <- normalizePath(tex_file, mustWork = TRUE)
  work_dir <- dirname(tex_file)
  base_name <- xfun::sans_ext(basename(tex_file))

  tex_content <- one_string(xfun::read_utf8(tex_file))
  if (is.null(bib_engine)) {
    bib_engine <- detect_bib_engine(tex_content)
  }

  run_latex <- function(pass) {
    if (!quiet) cat("  ", engine, " (pass ", pass, "/3)...\n", sep = "")
    owd <- setwd(work_dir)
    on.exit(setwd(owd), add = TRUE)
    res <- system2(engine, c(
      "-interaction=nonstopmode", "-shell-escape",
      basename(tex_file)
    ),
    stdout = FALSE, stderr = FALSE
    )
    if (res != 0) warning(engine, " (pass ", pass, "/3) had non-zero exit")
    invisible(res == 0)
  }

  run_bib <- function() {
    if (is.null(bib_engine)) {
      return(invisible(TRUE))
    }
    if (!quiet) cat("  ", bib_engine, "...\n", sep = "")
    aux_file <- paste0(base_name, ".aux")
    cmd <- if (bib_engine == "bibtex") c(aux_file) else base_name
    owd <- setwd(work_dir)
    on.exit(setwd(owd), add = TRUE)
    res <- system2(bib_engine, cmd, stdout = FALSE, stderr = FALSE)
    if (res != 0) warning(bib_engine, " step failed, continuing without bibliography")
    invisible(res == 0)
  }

  run_latex(1)
  run_bib()
  run_latex(2)
  run_latex(3)

  pdf_file <- file.path(work_dir, paste0(base_name, ".pdf"))

  if (!quiet) {
    log_path <- file.path(work_dir, paste0(base_name, ".log"))
    summary <- parse_latex_log(log_path)
    if (length(summary$errors)) {
      print_log_summary(summary, "[Knit-error]", 5)
    } else if (length(summary$warnings) || length(summary$badboxes) ||
      length(summary$undefined_refs) || length(summary$undefined_citations)) {
      print_log_summary(summary, "[Knit-warning]", 5)
    }
  }

  if (file.exists(pdf_file)) {
    invisible(pdf_file)
  } else {
    warning("PDF not produced: ", pdf_file)
    invisible(pdf_file)
  }
}

detect_bib_engine <- function(tex_content) {
  if (grepl("\\\\addbibresource\\{", tex_content)) {
    return("biber")
  }
  if (grepl("\\\\bibliography\\{|\\\\bibliographystyle\\{", tex_content)) {
    return("bibtex")
  }
  NULL
}

check_color_definition <- function(clean_preamble) {
  if (grepl("\\\\definecolor\\{", clean_preamble) &&
    !grepl("\\\\usepackage(\\[.*?\\])?\\{xcolor\\}", clean_preamble)) {
    stop(
      "You used \\definecolor{...} in your preamble without loading the xcolor package.\n",
      "       Add \\usepackage{xcolor} *before* \\definecolor{...} in your .Rnw file."
    )
  }
}

parse_latex_log <- function(log_path) {
  empty <- list(
    errors = character(), warnings = character(),
    badboxes = character(), undefined_refs = character(),
    undefined_citations = character()
  )
  if (!file.exists(log_path)) {
    return(empty)
  }

  content <- readLines(log_path, warn = FALSE)
  n <- length(content)
  errors <- character()
  warnings <- character()
  badboxes <- character()
  undefined_refs <- character()
  undefined_citations <- character()

  i <- 1
  while (i <= n) {
    line <- trimws(content[i])
    if (grepl("^! ", line)) {
      ctx <- line
      i <- i + 1
      while (i <= n) {
        l <- trimws(content[i])
        if (grepl("^! ", l)) {
          i <- i - 1
          break
        }
        ctx <- c(ctx, l)
        i <- i + 1
      }
      while (length(ctx) > 0 && !nzchar(ctx[length(ctx)])) {
        ctx <- ctx[-length(ctx)]
      }
      errors <- c(errors, paste(ctx, collapse = "\n"))
      i <- i + 1
      next
    }
    m <- regmatches(line, regexpr("^LaTeX Warning: .+$", line))
    if (length(m) == 1 && nzchar(m)) {
      warnings <- c(warnings, m)
      if (grepl("Reference.*undefined", m)) {
        undefined_refs <- c(undefined_refs, m)
      }
      if (grepl("Citation.*undefined", m)) {
        undefined_citations <- c(undefined_citations, m)
      }
      i <- i + 1
      next
    }
    m <- regmatches(line, regexpr("^Package .+ Warning: .+$", line))
    if (length(m) == 1 && nzchar(m)) {
      warn_text <- m
      i <- i + 1
      while (i <= n) {
        l <- trimws(content[i])
        if (!nzchar(l)) break
        if (grepl("^\\(", l)) {
          warn_text <- paste(warn_text, l)
          i <- i + 1
          next
        }
        if (grepl("^(LaTeX|Package|Overfull|Underfull|!|\\[|Output|No file)", l)) break
        warn_text <- paste(warn_text, l)
        i <- i + 1
      }
      warnings <- c(warnings, warn_text)
      next
    }
    m <- regmatches(line, regexpr("((Overfull|Underfull) \\\\+hbox.*)", line, perl = TRUE))
    if (length(m) == 0 || !nzchar(m)) {
      m <- regmatches(line, regexpr("((Overfull|Underfull) \\\\+vbox.*)", line, perl = TRUE))
    }
    if (length(m) == 1 && nzchar(m)) {
      badboxes <- c(badboxes, m)
      i <- i + 1
      next
    }
    if (grepl("LaTeX Warning:.*Reference.*undefined", line)) {
      undefined_refs <- c(undefined_refs, line)
    }
    if (grepl("LaTeX Warning:.*Citation.*undefined", line)) {
      undefined_citations <- c(undefined_citations, line)
    }
    i <- i + 1
  }

  list(
    errors = errors, warnings = warnings, badboxes = badboxes,
    undefined_refs = undefined_refs, undefined_citations = undefined_citations
  )
}

print_log_summary <- function(summary, label = "", max_per = 5) {
  any_issues <- length(summary$errors) > 0 || length(summary$warnings) > 0 ||
    length(summary$badboxes) > 0 || length(summary$undefined_refs) > 0 ||
    length(summary$undefined_citations) > 0
  if (!any_issues) {
    return()
  }

  count <- length(summary$errors) + length(summary$warnings) +
    length(summary$badboxes) + length(summary$undefined_refs) +
    length(summary$undefined_citations)
  cat(label, " ", count, " log issue(s):\n", sep = "")

  if (length(summary$errors)) {
    n <- min(length(summary$errors), max_per)
    cat("Errors (", length(summary$errors), "):\n", sep = "")
    for (e in summary$errors[1:n]) {
      cat("  * ", shorten_error(e), "\n", sep = "")
    }
    if (length(summary$errors) > max_per) {
      cat("    ... and ", length(summary$errors) - max_per, " more\n", sep = "")
    }
  }

  if (length(summary$warnings)) {
    n <- min(length(summary$warnings), max_per)
    cat("Warnings (", length(summary$warnings), "):\n", sep = "")
    for (w in summary$warnings[1:n]) {
      cat("  * ", w, "\n", sep = "")
    }
    if (length(summary$warnings) > max_per) {
      cat("    ... and ", length(summary$warnings) - max_per, " more\n", sep = "")
    }
  }

  if (length(summary$badboxes)) {
    n <- min(length(summary$badboxes), max_per)
    cat("Overfull/Underfull boxes (", length(summary$badboxes), "):\n", sep = "")
    for (b in summary$badboxes[1:n]) {
      cat("  * ", b, "\n", sep = "")
    }
    if (length(summary$badboxes) > max_per) {
      cat("    ... and ", length(summary$badboxes) - max_per, " more\n", sep = "")
    }
  }

  if (length(summary$undefined_refs)) {
    cat("Undefined references (", length(summary$undefined_refs), ")\n", sep = "")
  }

  if (length(summary$undefined_citations)) {
    cat("Undefined citations (", length(summary$undefined_citations), ")\n", sep = "")
  }
}

shorten_error <- function(e) {
  lines <- strsplit(e, "\n")[[1]]
  first <- trimws(lines[1])
  for (l in lines) {
    m <- regmatches(l, regexpr("l\\.\\d+", l))
    if (length(m) == 1 && nzchar(m)) {
      rest <- trimws(sub("^l\\.\\d+\\s*", "", l))
      return(paste(first, "(", m, if (nzchar(rest)) paste0(" ", rest) else "", ")", sep = ""))
    }
  }
  first
}

knit2pdf <- function(
  input, output = NULL, compiler = NULL, quiet = FALSE,
  clean = FALSE, envir = parent.frame(), ...
) {
  compiler <- compiler %n% opts_knit$get("engine") %n% "pdflatex"
  tex_file <- knit(input,
    output = output, quiet = quiet, envir = envir,
    compile = FALSE, engine = compiler, ...
  )
  if (!quiet) cat("Compiling ", tex_file, " -> .pdf\n", sep = "")
  pdf_path <- compile_pdf(tex_file, engine = compiler, quiet = quiet)
  if (clean) {
    aux_files <- Sys.glob(paste0(xfun::sans_ext(tex_file), ".*"))
    aux_files <- aux_files[!grepl("\\.(tex|pdf|Rnw)$", aux_files)]
    unlink(aux_files)
  }
  invisible(pdf_path)
}

knit_global_set <- function(envir) {
  assign(".knitGlobal", envir, envir = .knitEnv)
}

is_abs_path <- function(x) {
  grepl("^(/|[A-Za-z]:)", x)
}

text_to_tex <- function(text) {
  text <- gsub("\r\n", "\n", text, useBytes = TRUE)
  split_lines(text)
}

resolve_inputs <- function(doc) {
  doc <- one_string(doc)
  max_depth <- 10
  for (i in seq_len(max_depth)) {
    m <- regexpr("\\\\(input|include)\\{([^}]+)\\}", doc, perl = TRUE)
    if (m == -1) break
    start <- as.integer(m)
    end <- start + attr(m, "match.length") - 1
    cmd <- substr(doc, start, end)
    fname <- gsub("\\\\(input|include)\\{([^}]+)\\}", "\\2", cmd)
    if (!grepl("\\.tex$", fname)) fname <- paste0(fname, ".tex")
    if (!is_abs_path(fname)) fname <- file.path(input_dir(), fname)
    if (file.exists(fname)) {
      if (grepl("\\.rnw$", fname, ignore.case = TRUE)) next
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

clean_output <- function(doc) {
  doc <- gsub("\\\\newif\\\\iftexlab[\\s\\S]*?\\\\fi\n?", "", doc, perl = TRUE)
  doc <- gsub("(?m)^[ \t]*%[ \t]*!.*root[ \t]*=.*\n?", "", doc, perl = TRUE)
  doc
}
