set_header <- function(...) {
  opts_knit$set(header = merge_list(opts_knit$get("header"), c(...)))
}

.header.hi.tex <-
  "\\newcommand{\\hlnum}[1]{\\textcolor[rgb]{0.000,0.000,0.812}{#1}}
\\newcommand{\\hlstr}[1]{\\textcolor[rgb]{0.306,0.604,0.024}{#1}}
\\newcommand{\\hlsng}[1]{\\textcolor[rgb]{0.306,0.604,0.024}{#1}}
\\newcommand{\\hlcom}[1]{\\textcolor[rgb]{0.561,0.349,0.008}{\\textit{#1}}}
\\newcommand{\\hlopt}[1]{\\textcolor[rgb]{0.808,0.361,0.000}{\\textbf{#1}}}
\\newcommand{\\hlstd}[1]{\\textcolor[rgb]{0.000,0.000,0.000}{#1}}
\\newcommand{\\hldef}[1]{\\textcolor[rgb]{0.000,0.000,0.000}{#1}}
\\newcommand{\\hlkwd}[1]{\\textcolor[rgb]{0.125,0.290,0.529}{\\textbf{#1}}}
\\newcommand{\\hlkwc}[1]{\\textcolor[rgb]{0.561,0.349,0.008}{#1}}
\\newcommand{\\hlkwa}[1]{\\textcolor[rgb]{0.125,0.290,0.529}{\\textbf{#1}}}
\\newcommand{\\hlkwb}[1]{\\textcolor[rgb]{0.561,0.349,0.008}{#1}}
\\newcommand{\\hlkwr}[1]{\\textcolor[rgb]{0.647,0.000,0.000}{\\textbf{#1}}}
"

.header.maxwidth <-
  "\\makeatletter
\\def\\maxwidth{ %
  \\ifdim\\Gin@nat@width>\\linewidth
    \\linewidth
  \\else
    \\Gin@nat@width
  \\fi
}
\\makeatother
"

insert_header <- function(doc) {
  if (!out_format(c("latex", "sweave"))) {
    return(doc)
  }

  header <- make_header_latex(doc)
  if (!nzchar(header)) {
    return(doc)
  }

  b <- knit_patterns$get("document.begin")
  if (is.null(b)) {
    return(doc)
  }

  m <- regexpr(b, doc, perl = TRUE)
  if (m == -1) {
    return(doc)
  }

  pos <- as.integer(m)
  before <- substr(doc, 1, pos - 1)
  after <- sub("^\n+", "", substr(doc, pos, nchar(doc)))
  if (grepl("\n$", before)) {
    paste0(before, header, "\n", after)
  } else {
    paste0(before, "\n", header, "\n", after)
  }
}

make_header_latex <- function(doc) {
  one_string(build_preamble(doc))
}

build_preamble <- function(doc) {
  needed <- character()

  has_highlight <- grepl("\\\\hl[a-z]{2,3}\\{", doc)
  has_shaded <- grepl("\\\\begin\\{Shaded\\}", doc)
  has_minted <- grepl("\\\\begin\\{minted\\}", doc)

  is_beamer <- grepl("\\\\documentclass(\\[.*?\\])?\\{beamer\\}", doc)

  if (has_highlight || has_shaded || has_minted || grepl("\\\\textcolor|\\\\color\\{|\\\\definecolor", doc)) {
    if (!has_package(doc, "xcolor")) {
      needed <- c(needed, "\\usepackage{xcolor}")
    }
  }

  if (has_minted) {
    if (!has_package(doc, "minted")) {
      needed <- c(needed, "\\usepackage{minted}")
    }
    style <- opts_knit$get("minted_style") %n% opts_chunk$get("minted_style")
    if (!is.null(style) && is.character(style) && nzchar(style)) {
      needed <- c(needed, sprintf("\\usemintedstyle{%s}", style))
    }
    has_style <- !is.null(style) && is.character(style) && nzchar(style)
    minted_bg <- !has_style || isTRUE(.knitEnv$has_user_knitbg)
    if (minted_bg && !has_definecolor(doc, "knitbg")) {
      needed <- c(needed, "\\definecolor{knitbg}{rgb}{0.969, 0.969, 0.969}")
    }
  }

  if (has_shaded) {
    if (!has_package(doc, "fancyvrb") &&
      !has_defineverbatimenvironment(doc, "Highlighting")) {
      needed <- c(needed, "\\usepackage{fancyvrb}")
    }
    if (!has_package(doc, "framed") &&
      !has_newenvironment(doc, "Shaded")) {
      needed <- c(needed, "\\usepackage{framed}")
    }
    if (!has_definecolor(doc, "shadecolor")) {
      needed <- c(needed, "\\definecolor{shadecolor}{RGB}{248,248,248}")
    }
    if (!has_defineverbatimenvironment(doc, "Highlighting")) {
      hl_opts <- "commandchars=\\\\\\{\\},breaklines=true"
      if (is_beamer) hl_opts <- paste0(hl_opts, ",fontsize=\\footnotesize")
      needed <- c(needed, sprintf("\\DefineVerbatimEnvironment{Highlighting}{Verbatim}{%s}", hl_opts))
    }
    if (!has_newenvironment(doc, "Shaded")) {
      needed <- c(needed, "\\newenvironment{Shaded}{\\begin{snugshade}}{\\end{snugshade}}")
    }
  }

  if (grepl("\\\\includegraphics", doc)) {
    if (!has_package(doc, "graphicx")) {
      needed <- c(needed, "\\usepackage{graphicx}")
    }
  }

  if (grepl("\\\\maxwidth", doc)) {
    needed <- c(needed, .header.maxwidth)
  }

  fig_path <- opts_chunk$get("fig.path") %n% "figure/"
  if (grepl(sprintf("\\\\input\\{%s", fig_path), doc) && !has_package(doc, "tikz")) {
    needed <- c(needed, "\\usepackage{tikz}")
  }

  if (has_highlight) {
    hl_cmds <- c(
      "hlnum", "hlstr", "hlsng", "hlcom", "hlopt", "hlstd",
      "hldef", "hlkwd", "hlkwc", "hlkwa", "hlkwb", "hlkwr"
    )
    all_missing <- FALSE
    for (cmd in hl_cmds) {
      if (!has_newcommand(doc, cmd)) {
        all_missing <- TRUE
        break
      }
    }
    if (all_missing) needed <- c(needed, .header.hi.tex)
  }

  colors_used <- character()

  if (grepl("shadecolor", doc) && !has_definecolor(doc, "shadecolor")) {
    colors_used <- c(colors_used, "\\definecolor{shadecolor}{rgb}{0.969, 0.969, 0.969}")
  }

  if (grepl("messagecolor", doc) && !has_definecolor(doc, "messagecolor")) {
    colors_used <- c(colors_used, "\\definecolor{messagecolor}{rgb}{0, 0, 0}")
  }

  if (grepl("warningcolor", doc) && !has_definecolor(doc, "warningcolor")) {
    colors_used <- c(colors_used, "\\definecolor{warningcolor}{rgb}{1, 0, 1}")
  }

  if (grepl("errorcolor", doc) && !has_definecolor(doc, "errorcolor")) {
    colors_used <- c(colors_used, "\\definecolor{errorcolor}{rgb}{1, 0, 0}")
  }

  if (length(colors_used)) {
    needed <- c(needed, colors_used)
  }

  needed
}

has_package <- function(doc, pkg) {
  grepl(sprintf("\\\\usepackage(\\[.*?\\])?\\{%s\\}", pkg), strip_latex_comments(doc), perl = TRUE)
}

has_newcommand <- function(doc, cmd) {
  grepl(sprintf("\\\\newcommand(\\*)?\\{%s\\}", cmd), strip_latex_comments(doc), perl = TRUE)
}

has_newenvironment <- function(doc, env) {
  grepl(sprintf("\\\\newenvironment\\{%s\\}", env), strip_latex_comments(doc), perl = TRUE)
}

has_definecolor <- function(doc, color) {
  grepl(sprintf("\\\\definecolor\\{%s\\}", color), strip_latex_comments(doc), perl = TRUE)
}

has_defineverbatimenvironment <- function(doc, env) {
  grepl(sprintf("\\\\DefineVerbatimEnvironment\\{%s\\}", env), strip_latex_comments(doc), perl = TRUE)
}

strip_latex_comments <- function(doc) {
  lines <- split_lines(doc)
  for (i in seq_along(lines)) {
    line <- lines[[i]]
    chars <- strsplit(line, "")[[1]]
    j <- 1
    n <- length(chars)
    while (j <= n) {
      if (chars[j] == "\\" && j < n) {
        j <- j + 2
      } else if (chars[j] == "%") {
        lines[[i]] <- substr(line, 1, j - 1)
        break
      } else {
        j <- j + 1
      }
    }
  }
  paste(lines, collapse = "\n")
}
