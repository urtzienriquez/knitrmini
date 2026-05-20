.inline.hook <- function(x) {
  if (is.numeric(x)) x <- round_digits(x)
  paste(as.character(x), collapse = ", ")
}

.inline.hook.tex <- function(x) {
  if (is.numeric(x)) {
    x <- format_sci(x, "latex")
    i <- grep("[}]", x)
    x[i] <- sprintf("\\ensuremath{%s}", x[i])
    if (getOption("OutDec") != ".") x <- sprintf("\\text{%s}", x)
  }
  .inline.hook(x)
}

.verb.hook <- function(x) {
  one_string(c("\\begin{verbatim}", sub("\n$", "", x), "\\end{verbatim}", ""))
}

.chunk.hook.tex <- function(x, options) {
  ai <- output_asis(x, options)
  if (ai) {
    return(x)
  }
  size <- if (options$size == "normalsize") "" else sprintf("\\%s", options$size)
  if (nzchar(size)) {
    sprintf("{%s\n%s\n}", size, x)
  } else {
    x
  }
}

hooks_latex <- function() {
  list(
    source = function(x, options) {
      if (isFALSE(options$highlight)) {
        return(.verb.hook(x))
      }
      if (isTRUE(options$minted)) {
        x <- gsub("\n$", "", x)
        x <- paste(x, collapse = "\n")
        style <- opts_knit$get("minted_style") %n% options$minted_style
        has_style <- !is.null(style) && is.character(style) && nzchar(style)
        minted_bg <- !has_style || isTRUE(.knitEnv$has_user_knitbg)
        opts <- ""
        if (minted_bg) opts <- "bgcolor=knitbg"
        one_string(c(sprintf("\\begin{minted}[%s]{r}", opts), x, "\\end{minted}", ""))
      } else {
        x <- hilight_source(x, "latex", options)
        x <- x[nzchar(x)]
        x <- gsub("\n*$", "", x)
        one_string(c("\\begin{Shaded}\n\\begin{Highlighting}[]", x, "\\end{Highlighting}\n\\end{Shaded}", ""))
      }
    },
    output = function(x, options) {
      if (output_asis(x, options)) {
        return(x)
      }
      if (!is.na(options$comment) && nzchar(options$comment)) {
        x <- paste0(options$comment, " ", x)
        x <- gsub("\n", paste0("\n", options$comment, " "), x)
        x <- gsub(paste0("(\\n", options$comment, " )+$"), "", x)
      }
      .verb.hook(x)
    },
    warning = .color.block("\\color{warningcolor}{", "}"),
    message = .color.block("\\itshape\\color{messagecolor}{", "}"),
    error = .color.block("\\bfseries\\color{errorcolor}{", "}"),
    inline = .inline.hook.tex,
    chunk = .chunk.hook.tex,
    plot = function(x, options) {
      render_figures(x, options)
    },
    text = identity,
    evaluate = function(...) evaluate::evaluate(...),
    evaluate.inline = function(code, envir = knit_global()) {
      v <- withVisible(eval(parse_only(code), envir = envir))
      if (v$visible) knit_print(v$value, inline = TRUE, options = opts_chunk$get())
    },
    document = identity
  )
}

.out.hook <- function(x, options) x
.plot.hook <- function(x, options) paste(x, collapse = ".")

.default.hooks <- list(
  source = .out.hook, output = .out.hook, warning = .out.hook,
  message = .out.hook, error = .out.hook, plot = .plot.hook,
  inline = .inline.hook, chunk = .out.hook, text = identity,
  evaluate.inline = function(code, envir = knit_global()) {
    v <- withVisible(eval(parse_only(code), envir = envir))
    if (v$visible) knit_print(v$value, inline = TRUE, options = opts_chunk$get())
  },
  evaluate = function(...) evaluate::evaluate(...),
  document = identity
)

knit_hooks <- new_defaults(.default.hooks)

render_latex <- function() {
  opts_chunk$set(out.width = "\\maxwidth", dev = "pdf")
  opts_knit$set(out.format = "latex")
  knit_hooks$set(hooks_latex())
}

render_figures <- function(figures, options) {
  caption <- options$fig.cap
  width <- options$out.width
  height <- options$out.height
  f_pos <- options$fig.pos
  f_env <- options$fig.env
  f_lp <- options$fig.lp

  if (is.null(f_env) && !is.null(caption)) {
    f_env <- "figure"
  }
  if (is.null(f_pos) || !nzchar(f_pos)) {
    f_pos <- "!h"
  }

  attribs <- ""
  if (!is.null(width)) {
    if (is.numeric(width)) width <- paste0(width, "px")
    attribs <- paste0("width=", width)
  }
  if (nzchar(attribs) && !is.null(height)) {
    attribs <- paste0(attribs, ",")
  }
  if (!is.null(height)) {
    attribs <- paste0(attribs, "height=", height)
  }

  result <- ""
  if (!is.null(f_env)) {
    result <- paste0(result, sprintf("\\begin{%s}[%s]\n", f_env, f_pos))
  }

  for (fig in figures) {
    if (grepl("\\.tex$", fig)) {
      result <- paste0(result, sprintf("\\input{%s}\n", sans_ext(fig)))
    } else if (nzchar(attribs)) {
      result <- paste0(result, sprintf("\\includegraphics[%s]{%s}\n", attribs, sans_ext(fig)))
    } else {
      result <- paste0(result, sprintf("\\includegraphics{%s}\n", sans_ext(fig)))
    }
  }

  if (!is.null(caption)) {
    result <- paste0(result, "\\center\n")
    result <- paste0(result, sprintf("\\caption{%s}\n", caption))
  }

  if (!is.null(options$label) && !is.null(f_env)) {
    result <- paste0(result, sprintf("\\label{%s%s}\n", f_lp %n% "fig:", options$label))
  }

  if (!is.null(f_env)) {
    result <- paste0(result, sprintf("\\end{%s}\n", f_env))
  }

  result
}

.color.block <- function(color1 = "", color2 = "") {
  function(x, options) {
    x <- gsub("\n*$", "", x)
    x <- escape_latex(x, newlines = TRUE, spaces = TRUE)
    x <- gsub('"', '"{}', x)
    sprintf("\n\n{\\ttfamily\\noindent%s%s%s}", color1, x, color2)
  }
}

output_asis <- function(x, options) {
  is_blank(x) || identical(options$results, "asis")
}

escape_latex <- function(s, newlines = FALSE, spaces = FALSE) {
  s <- gsub("\\", "\\textbackslash{}", s, fixed = TRUE)
  s <- gsub("{", "\\{", s, fixed = TRUE)
  s <- gsub("}", "\\}", s, fixed = TRUE)
  s <- gsub("$", "\\$", s, fixed = TRUE)
  s <- gsub("%", "\\%", s, fixed = TRUE)
  s <- gsub("&", "\\&", s, fixed = TRUE)
  s <- gsub("#", "\\#", s, fixed = TRUE)
  s <- gsub("_", "\\_", s, fixed = TRUE)
  s <- gsub("^", "\\^{}", s, fixed = TRUE)
  s <- gsub("~", "\\textasciitilde{}", s, fixed = TRUE)
  s
}

sans_ext <- function(x) {
  sub("([^.]+)\\.[[:alnum:]]+$", "\\1", x)
}

round_digits <- function(x) {
  if (getOption("knitr.digits.signif", FALSE)) {
    format(x)
  } else {
    as.character(round(x, getOption("digits")))
  }
}

format_sci_one <- function(x, format = "latex") {
  if (!(class(x)[1] == "numeric") || is.na(x) || x == 0) {
    return(as.character(x))
  }
  if (is.infinite(x)) {
    return(sprintf("%s\\infty{}", ifelse(x < 0, "-", "")))
  }
  if (abs(lx <- floor(log10(abs(x)))) < getOption("scipen") + 4L) {
    return(round_digits(x))
  }
  b <- round_digits(x / 10^lx)
  sprintf("%s%s10^{%s}", b, "\\times ", lx)
}

format_sci <- function(x, ...) {
  if (inherits(x, "roman")) {
    return(as.character(x))
  }
  vapply(x, format_sci_one, character(1L), ..., USE.NAMES = FALSE)
}
