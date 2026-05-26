#' Knit a document
#'
#' The main entry point for knitrmini. Process an \code{.Rnw} file (or character
#' vector of text), executing R code chunks and producing a \code{.tex} file.
#' If \code{compile = TRUE}, the \code{.tex} file is compiled to PDF automatically.
#'
#' @param input Path to the input \code{.Rnw} file, or a character vector of text
#'   (when \code{text} is provided).
#' @param output Path for the output \code{.tex} file. Auto-guessed from input if \code{NULL}.
#' @param text Character vector of document text (used when \code{input} is missing).
#' @param quiet Suppress progress messages.
#' @param envir Environment in which to evaluate code chunks (default: parent frame).
#' @param compile Whether to compile the resulting \code{.tex} to PDF.
#' @param engine LaTeX engine (\code{"pdflatex"}, \code{"xelatex"}, \code{"lualatex"}).
#' @param minted_style Pygments style name for minted highlighting (e.g. \code{"tango"}).
#' @param clean Remove auxiliary files (except \code{.tex} and \code{.pdf}) after compilation.
#' @param pvc Enable continuous preview. Watches the source \code{.Rnw} file for
#'   changes, then re-knits and re-compiles automatically. Blocks until
#'   interrupted (Ctrl+C). Not recommended in non-interactive sessions.
#' @return Invisibly returns the path to the output \code{.tex} or \code{.pdf} file.
#' @export
#'
#' @examples
#' library(knitrmini)
#' f <- system.file("examples", "knitrmini-minimal.Rnw", package = "knitrmini")
#' knit(f, compile = FALSE)
#'
#' tangle(f)
#'
#' unlink(c("knitrmini-minimal.tex", "knitrmini-minimal.R"), recursive = TRUE)
knit <- function(
  input, output = NULL, text = NULL, quiet = FALSE,
  envir = parent.frame(),
  compile = TRUE, engine = NULL,
  minted_style = NULL, clean = FALSE, pvc = FALSE
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

  output_file <- output
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
  out_text <- paste(split_lines(out_text), collapse = "\n")

  out_text <- insert_header(out_text)

  if (opts_knit$get("resolve_input")) {
    out_text <- resolve_inputs(out_text)
  }

  if (opts_knit$get("normalize_paths")) {
    out_text <- resolve_includegraphics(out_text, input_dir())
  }

  out_text <- clean_output(out_text)

  if (in.file) {
    out_path <- output_file %n% guess_output(input)
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
      eng <- engine %n% opts_knit$get("engine") %n% "pdflatex"
      if (pvc) {
        compile_pdf(out_path, engine = eng, quiet = quiet)
        if (!quiet) cat("  Watching", basename(input), "for changes (Ctrl+C to stop)...\n")
        last_mtime <- file.mtime(input)
        while (TRUE) {
          Sys.sleep(1)
          cur_mtime <- file.mtime(input)
          if (is.na(cur_mtime) || identical(cur_mtime, last_mtime)) next
          last_mtime <- cur_mtime
          if (!quiet) cat("\n  Change detected, re-knitting...\n")
          knit(input = input, output = out_path, quiet = TRUE,
               envir = envir, compile = FALSE, clean = FALSE)
          compile_pdf(out_path, engine = eng, quiet = quiet)
          if (!quiet) cat("  Watching for changes (Ctrl+C to stop)...\n")
        }
      } else {
        pdf_path <- compile_pdf(out_path, engine = eng, quiet = quiet)
        if (clean) {
          aux_files <- Sys.glob(paste0(xfun::sans_ext(out_path), ".*"))
          aux_files <- aux_files[!grepl("\\.(tex|pdf|Rnw)$", aux_files)]
          unlink(aux_files)
        }
        invisible(pdf_path)
      }
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

#' Compile a TeX file to PDF
#'
#' Use \code{latexmk} to compile a \code{.tex} file to PDF. \code{latexmk}
#' automatically determines the required number of compilation passes, runs
#' bibliography tools (bibtex/biber) when needed, and tracks file dependencies
#' for incremental rebuilds. Passes \code{-pvc} for continuous preview mode.
#'
#' @param tex_file Path to the \code{.tex} file.
#' @param engine LaTeX engine (\code{"pdflatex"}, \code{"xelatex"}, \code{"lualatex"}).
#' @param bib_engine Deprecated and ignored. \code{latexmk} auto-detects the
#'   bibliography engine.
#' @param quiet Suppress progress messages.
#' @param pvc Enable continuous preview with \code{latexmk -pvc}. Watches the
#'   \code{.tex} file for changes and recompiles automatically. Blocks until
#'   interrupted (Ctrl+C). Only useful when you edit the \code{.tex} file
#'   directly; for \code{.Rnw} workflows use \code{pvc = TRUE} in \code{\link{knit}}.
#' @return Invisibly returns the path to the generated \code{.pdf} file.
#' @keywords internal
compile_pdf <- function(tex_file, engine = "pdflatex", bib_engine = NULL,
                        quiet = FALSE, pvc = FALSE) {
  tex_file <- normalizePath(tex_file, mustWork = TRUE)
  work_dir <- dirname(tex_file)
  base_name <- xfun::sans_ext(basename(tex_file))
  pdf_file <- file.path(work_dir, paste0(base_name, ".pdf"))

  latexmk_engine <- switch(engine,
    "pdflatex" = "-pdf",
    "lualatex"  = "-lualatex",
    "xelatex"   = "-xelatex",
    "-pdf"
  )

  args <- c(
    latexmk_engine,
    "-interaction=nonstopmode",
    "-shell-escape",
    "-f",
    "-synctex=1",
    basename(tex_file)
  )

  if (pvc) {
    args <- c(args, "-pvc")
  }

  if (!quiet) {
    cat("  latexmk (", engine, ")", if (pvc) " [preview mode]", "...\n", sep = "")
  }

  owd <- setwd(work_dir)
  on.exit(setwd(owd), add = TRUE)

  res <- system2("latexmk", args, stdout = FALSE, stderr = FALSE)
  if (res != 0) {
    warning("latexmk (", engine, ") had non-zero exit status")
  }

  if (!quiet && !pvc) {
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



up_to_date <- function(source_file, target_file) {
  if (!file.exists(target_file)) return(FALSE)
  file.mtime(target_file) > file.mtime(source_file)
}

#' Check color definition in preamble
#'
#' Validate that `\\definecolor{...}` in the document preamble is preceded by
#' `\\usepackage{xcolor}`. Raises an error if xcolor is missing.
#'
#' @param clean_preamble Document preamble with LaTeX comments stripped.
#' @keywords internal
check_color_definition <- function(clean_preamble) {
  if (grepl("\\\\definecolor\\{", clean_preamble) &&
    !grepl("\\\\usepackage(\\[.*?\\])?\\{xcolor\\}", clean_preamble)) {
    stop(
      "You used \\definecolor{...} in your preamble without loading the xcolor package.\n",
      "       Add \\usepackage{xcolor} *before* \\definecolor{...} in your .Rnw file."
    )
  }
}

#' Parse LaTeX log file
#'
#' Parse a LaTeX \code{.log} file and extract errors, warnings, bad boxes,
#' undefined references, and undefined citations.
#'
#' @param log_path Path to the \code{.log} file.
#' @return A list with components \code{errors}, \code{warnings}, \code{badboxes},
#'   \code{undefined_refs}, and \code{undefined_citations}.
#' @keywords internal
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
    i <- i + 1
  }

  list(
    errors = errors, warnings = warnings, badboxes = badboxes,
    undefined_refs = undefined_refs, undefined_citations = undefined_citations
  )
}

#' Print LaTeX log summary
#'
#' Print a formatted summary of issues found in a LaTeX compilation log.
#'
#' @param summary A list from [parse_latex_log()].
#' @param label Label prefix for the output.
#' @param max_per Maximum number of items to show per category.
#' @keywords internal
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

#' Knit and compile to PDF
#'
#' Convenience function that runs [knit()] followed by [compile_pdf()]. Skips
#' both steps if the output is already up to date.
#'
#' @param input Path to the input \code{.Rnw} file.
#' @param output Path for the output \code{.tex} file (auto-guessed if \code{NULL}).
#' @param compiler LaTeX engine (\code{"pdflatex"}, \code{"xelatex"}, \code{"lualatex"}).
#' @param quiet Suppress progress messages.
#' @param clean Remove auxiliary files after compilation.
#' @param envir Environment for code evaluation.
#' @param pvc Enable continuous preview. Watches the source \code{.Rnw} file for
#'   changes, then re-knits and re-compiles automatically. Blocks until
#'   interrupted (Ctrl+C).
#' @param ... Additional arguments passed to [knit()].
#' @return Invisibly returns the path to the generated \code{.pdf} file.
#' @export
knit2pdf <- function(
  input, output = NULL, compiler = NULL, quiet = FALSE,
  clean = FALSE, envir = parent.frame(), pvc = FALSE, ...
) {
  compiler <- compiler %n% opts_knit$get("engine") %n% "pdflatex"
  out_path <- output %n% guess_output(input)
  pdf_file <- paste0(xfun::sans_ext(out_path), ".pdf")
  if (up_to_date(input, out_path) && up_to_date(input, pdf_file)) {
    if (!quiet) cat("Output is up to date.\n", sep = "")
    return(invisible(pdf_file))
  }
  tex_file <- knit(input, output = output, quiet = quiet, envir = envir,
    compile = FALSE, engine = compiler, ...
  )
  if (pvc) {
    if (!quiet) cat("Compiling ", tex_file, " -> .pdf\n", sep = "")
    compile_pdf(tex_file, engine = compiler, quiet = quiet)
    if (!quiet) cat("  Watching", basename(input), "for changes (Ctrl+C to stop)...\n")
    last_mtime <- file.mtime(input)
    while (TRUE) {
      Sys.sleep(1)
      cur_mtime <- file.mtime(input)
      if (is.na(cur_mtime) || identical(cur_mtime, last_mtime)) next
      last_mtime <- cur_mtime
      if (!quiet) cat("\n  Change detected, re-knitting...\n")
      knit(input = input, output = tex_file, quiet = TRUE,
           envir = envir, compile = FALSE, clean = FALSE)
      if (!quiet) cat("  Re-compiling...\n")
      compile_pdf(tex_file, engine = compiler, quiet = quiet)
      if (!quiet) cat("  Watching for changes (Ctrl+C to stop)...\n")
    }
  } else {
    if (!quiet) cat("Compiling ", tex_file, " -> .pdf\n", sep = "")
    pdf_path <- compile_pdf(tex_file, engine = compiler, quiet = quiet)
    if (clean) {
      aux_files <- Sys.glob(paste0(xfun::sans_ext(tex_file), ".*"))
      aux_files <- aux_files[!grepl("\\.(tex|pdf|Rnw)$", aux_files)]
      unlink(aux_files)
    }
    invisible(pdf_path)
  }
}

knit_global_set <- function(envir) {
  assign(".knitGlobal", envir, envir = .knitEnv)
}

is_abs_path <- function(x) {
  grepl("^(/|[A-Za-z]:)", x)
}

#' Resolve LaTeX input/include commands
#'
#' Recursively replace `\\input{...}` and `\\include{...}` commands with the
#' content of the referenced files (up to a depth of 10). Only `.tex` files
#' are inlined; `.Rnw` files are left as-is.
#'
#' @param doc LaTeX document string.
#' @return Document string with inputs resolved.
#' @keywords internal
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

normalize_path <- function(path) {
  tryCatch(
    normalizePath(path, mustWork = TRUE),
    error = function(e) {
      parts <- strsplit(path, "/|\\\\")[[1]]
      result <- character()
      for (p in parts) {
        if (p == "." || p == "") next
        if (p == ".." && length(result) > 0) {
          result <- result[-length(result)]
        } else {
          result <- c(result, p)
        }
      }
      prefix <- if (grepl("^/", path)) "/" else ""
      paste0(prefix, paste(result, collapse = "/"))
    }
  )
}

#' Resolve includegraphics paths
#'
#' Resolve relative paths in `\\includegraphics` commands to absolute paths
#' rooted at `input_dir`.
#'
#' @param doc LaTeX document string.
#' @param input_dir The directory of the input file.
#' @return Document string with paths resolved.
#' @keywords internal
resolve_includegraphics <- function(doc, input_dir) {
  doc <- one_string(doc)
  pattern <- paste0(
    "\\\\includegraphics\\*?",
    "(?:\\[[^]]*(?:\\{[^}]*\\}[^]]*)*\\])?",
    "\\{([^}]+)\\}"
  )
  idx <- 1
  while (idx <= nchar(doc)) {
    chunk <- substr(doc, idx, nchar(doc))
    m <- regexpr(pattern, chunk, perl = TRUE)
    if (m == -1) break
    match_len <- attr(m, "match.length")
    m <- m + idx - 1
    matched <- substr(doc, m, m + match_len - 1)
    path <- gsub(pattern, "\\1", matched)
    if (!is_abs_path(path)) {
      resolved <- normalize_path(file.path(input_dir, path))
      new_matched <- sub(paste0("{", path, "}"), paste0("{", resolved, "}"), matched, fixed = TRUE)
      doc <- paste0(substr(doc, 1, m - 1), new_matched, substr(doc, m + match_len, nchar(doc)))
      idx <- m + nchar(new_matched)
    } else {
      idx <- m + match_len
    }
  }
  doc
}

clean_output <- function(doc) {
  doc <- gsub("\\\\newif\\\\iftexlab[\\s\\S]*?\\\\fi\n?", "", doc, perl = TRUE)
  doc <- gsub("(?m)^[ \t]*%[ \t]*!.*root[ \t]*=.*\n?", "", doc, perl = TRUE)
  doc
}
