#' A worker for editors: knits on request, R started once
#'
#' Reads one command per line from \code{input}, and answers each on \code{output}, until
#' \code{quit} or the end of its input:
#'
#' \preformatted{knit<TAB>input<TAB>output<TAB>compile (1/0)<TAB>engine (or empty)}
#'
#' knits \code{input} into \code{output} (paths relative to where the worker started), printing
#' its progress as [knit()] does, then a line \code{\\001done<TAB>ok} (or
#' \code{\\001done<TAB>error<TAB>message}).
#'
#' What makes a build slow in a new R each time (R itself, and the packages a document loads:
#' seconds for some) is done once. Each knit is otherwise as in a new R: an empty global
#' environment, R's options as they were when the worker started, the document's folder as the
#' working directory, no graphics device left open. A package installed again since it was loaded
#' (its DESCRIPTION changed) makes the worker exit with status 3 before the knit: the editor starts
#' a new one, which loads it anew.
#'
#' @param input,output Connections (default: the standard input and output).
#' @return Nothing; it returns at \code{quit} or the end of \code{input}.
#' @export
serve <- function(input = file("stdin"), output = stdout()) {
  if (!isOpen(input)) {
    open(input)
    on.exit(close(input), add = TRUE)
  }
  options0 <- options()
  wd0 <- getwd()
  installed <- function() { # (each loaded package's DESCRIPTION: when it was installed)
    pkgs <- loadedNamespaces()
    files <- vapply(pkgs, function(p) system.file("DESCRIPTION", package = p), "")
    stats::setNames(file.mtime(files), pkgs)
  }
  seen <- installed()
  repeat {
    line <- readLines(input, n = 1, warn = FALSE)
    if (!length(line) || identical(line, "quit")) break
    parts <- strsplit(line, "\t", fixed = TRUE)[[1]]
    if (!length(parts) || !nzchar(parts[1])) next
    now <- installed()
    common <- intersect(names(seen), names(now))
    if (any(now[common] != seen[common], na.rm = TRUE)) quit(save = "no", status = 3)
    status <- "ok"
    message <- ""
    if (identical(parts[1], "knit") && length(parts) >= 4) {
      file <- normalizePath(file.path(wd0, parts[2]), mustWork = FALSE)
      if (is_abs_path(parts[2])) file <- normalizePath(parts[2], mustWork = FALSE)
      out <- if (is_abs_path(parts[3])) parts[3] else file.path(wd0, parts[3])
      engine <- if (length(parts) >= 5 && nzchar(parts[5])) parts[5]
      err <- tryCatch({
        rm(list = ls(globalenv(), all.names = TRUE), envir = globalenv())
        options(options0)
        setwd(dirname(file))
        knit(file, output = out, compile = identical(parts[4], "1"), engine = engine, envir = globalenv())
        NULL
      }, error = function(e) conditionMessage(e), finally = {
        setwd(wd0)
        grDevices::graphics.off()
      })
      if (!is.null(err)) {
        status <- "error"
        message <- gsub("[\r\n]+", " ", err)
      }
    } else {
      status <- "error"
      message <- paste("unknown command:", line)
    }
    seen <- installed()
    cat("\001done\t", status, if (nzchar(message)) paste0("\t", message), "\n", sep = "", file = output)
    flush(output)
  }
  invisible()
}
