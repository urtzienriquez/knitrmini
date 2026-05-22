#' Knit code store
#'
#' A list-like object (created by [new_defaults()]) that stores the code for each
#' chunk by label. Populated during parsing and consumed during execution.
#'
#' @format A list with methods: \code{get}, \code{set}, \code{delete}, \code{append},
#'   \code{merge}, and \code{restore}.
#' @export
knit_code <- new_defaults()
dep_list <- new_defaults()
.knitEnv <- new.env(parent = emptyenv())
.knitEnv$labels <- character()
.knitEnv$terminate <- NULL
.knitEnv$input.dir <- NULL

#' Split document into groups
#'
#' Split document lines into code chunks and prose blocks using the active
#' pattern definitions.
#'
#' @param lines Character vector of document lines.
#' @param patterns Pattern list (from [knit_patterns]).
#' @return A list of parsed groups (each either a \code{block} or \code{inline} object).
#' @keywords internal
split_file <- function(lines, patterns = knit_patterns$get()) {
  chunk.begin <- patterns$chunk.begin
  chunk.end <- patterns$chunk.end
  if (is.null(chunk.begin) || is.null(chunk.end)) {
    return(list(parse_inline(lines, patterns)))
  }

  groups <- divide_chunks(lines, chunk.begin, chunk.end)
  lapply(seq_along(groups), function(i) {
    g <- groups[[i]]
    block <- grepl(chunk.begin, g[1])
    if (block) {
      n <- length(g)
      if (n >= 2 && grepl(chunk.end, g[n])) g <- g[-n]
      g <- strip_block(g, patterns$chunk.code)
      params.src <- if (group_pattern(chunk.begin)) {
        trimws(gsub(chunk.begin, "\\1", g[1]))
      } else {
        ""
      }
      parse_block(g[-1], params.src, markdown_mode = FALSE)
    } else {
      parse_inline(g, patterns)
    }
  })
}

divide_chunks <- function(x, begin_pat, end_pat) {
  begin <- grepl(begin_pat, x, perl = TRUE)
  end <- grepl(end_pat, x, perl = TRUE)
  i <- group_indices(begin, end)
  unname(split(x, i))
}

group_indices <- function(begin, end) {
  n <- length(begin)
  g <- integer(n)
  in_chunk <- FALSE
  group <- 0L

  for (i in seq_len(n)) {
    if (!in_chunk) {
      if (begin[i]) {
        in_chunk <- TRUE
        group <- group + 1L
        g[i] <- group
      } else {
        g[i] <- group
      }
    } else {
      g[i] <- group
      if (end[i]) {
        in_chunk <- FALSE
        group <- group + 1L
      }
    }
  }
  g
}

strip_block <- function(x, prefix = NULL) {
  if (!is.null(prefix) && (length(x) > 1)) {
    x[-1L] <- sub(prefix, "", x[-1L])
    spaces <- min(attr(regexpr("^ *", x[-1L]), "match.length"))
    if (spaces > 0) x[-1L] <- substring(x[-1L], spaces + 1)
  }
  x
}

#' Parse a code block
#'
#' Parse a single code chunk: extract parameters from the header, register
#' the chunk label, and store the code for later execution.
#'
#' @param code Character vector of code lines.
#' @param params.src Raw chunk header string.
#' @param markdown_mode Logical; whether the source is R Markdown (not Rnw).
#' @return A \code{block} object (list with \code{params} and \code{params.src}).
#' @keywords internal
parse_block <- function(code, params.src, markdown_mode = FALSE) {
  params <- params.src
  if (!markdown_mode) {
    params <- gsub("^\\s*,*\\s*|\\s*,*\\s*$", "", params)
  }
  params <- xfun::csv_options(params)
  if (is.null(params$label)) params$label <- unnamed_chunk()
  label <- params$label
  .knitEnv$labels <- c(.knitEnv$labels, label)
  if (length(code) || length(params[["file"]]) || length(params[["code"]])) {
    if (label %in% names(knit_code$get())) {
      params$label <- label <- unnamed_chunk(label)
    }
    code <- as.character(code)
    knit_code$set(setNames(list(structure(code, chunk_opts = params)), label))
  }
  if (!is.null(deps <- params$dependson)) {
    deps <- sc_split(deps)
    if (is.character(deps)) {
      for (i in deps) dep_list$set(setNames(list(c(dep_list$get(i), label)), i))
    }
  }
  structure(class = "block", list(
    params = params, params.src = params.src
  ))
}

#' Parse inline expressions
#'
#' Extract inline code expressions (e.g. `\\Sexpr{...}`) from a block of text.
#'
#' @param input Character vector of text lines.
#' @param patterns Pattern list (from [knit_patterns]).
#' @return An \code{inline} object with fields \code{input}, \code{location}, and \code{code}.
#' @keywords internal
parse_inline <- function(input, patterns) {
  inline.code <- patterns$inline.code
  inline.comment <- patterns$inline.comment
  if (!is.null(inline.comment)) {
    idx <- grepl(inline.comment, input)
    input[idx] <- gsub(inline.code, "\\1", input[idx])
  }
  input <- one_string(input)
  loc <- cbind(start = numeric(0), end = numeric(0))
  if (group_pattern(inline.code)) loc <- str_locate(input, inline.code)[[1]]
  code2 <- character()
  if (is.matrix(loc) && nrow(loc) > 0) {
    m <- gregexpr(inline.code, input, perl = TRUE)[[1]]
    if (length(m) > 0 && m[1] > 0) {
      cs <- attr(m, "capture.start")
      cl <- attr(m, "capture.length")
      captures <- lapply(seq_along(m), function(i) {
        start <- cs[i, 1]
        end <- start + cl[i, 1] - 1
        substring(input, start, end)
      })
      code2 <- unlist(captures)
    }
  }
  structure(list(
    input = input, location = loc, code = code2
  ), class = "inline")
}

#' Generate unnamed chunk label
#'
#' @param prefix Label prefix (default from \code{opts_knit$get("unnamed.chunk.label")}).
#' @return A unique chunk label string.
#' @keywords internal
unnamed_chunk <- function(prefix = NULL) {
  if (is.null(prefix)) prefix <- opts_knit$get("unnamed.chunk.label")
  paste(prefix, chunk_counter(), sep = "-")
}

#' Chunk counter
#'
#' A closure that tracks the number of chunks processed. Resets on each
#' top-level \code{knit()} call.
#'
#' @param reset If \code{TRUE}, reset the counter to 0.
#' @return The current chunk number (incremented after each call).
#' @keywords internal
chunk_counter <- local({
  n <- 0L
  function(reset = FALSE) {
    if (reset) {
      return(n <<- 0L)
    }
    n <<- n + 1L
    n
  }
})

plot_counter <- local({
  n <- 1L
  function(reset = FALSE) {
    if (reset) {
      return(n <<- 1L)
    }
    n <<- n + 1L
    n - 1L
  }
})

str_locate <- function(x, pattern, all = TRUE) {
  out <- (if (all) gregexpr else regexpr)(pattern, x, perl = TRUE)
  if (all) {
    lapply(out, function(x) {
      len <- attr(x, "match.length")
      if (length(x) == 1 && x == -1) x <- integer()
      cbind(start = x, end = x + len - 1L)
    })
  } else {
    len <- attr(out, "match.length")
    if (length(out) == 1 && out == -1) out <- integer()
    cbind(start = out, end = out + len - 1L)
  }
}

str_replace <- function(x, pos, value) {
  if (length(x) != 1) stop("Only a character scalar is supported.")
  m <- rbind(pos[, 1] - 1, pos[, 2] + 1)
  m <- matrix(c(1, m, nchar(x)), nrow = 2)
  y <- substring(x, m[1, ], m[2, ])
  paste(rbind(y, c(value, "")), collapse = "")
}
