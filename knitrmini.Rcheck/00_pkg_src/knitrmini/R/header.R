set_header = function(...) {
  opts_knit$set(header = merge_list(opts_knit$get('header'), c(...)))
}

.header.hi.tex =
'\\newcommand{\\hlnum}[1]{\\textcolor[rgb]{0.000,0.000,0.812}{#1}}
\\newcommand{\\hlstr}[1]{\\textcolor[rgb]{0.306,0.604,0.024}{#1}}
\\newcommand{\\hlsng}[1]{\\textcolor[rgb]{0.306,0.604,0.024}{#1}}
\\newcommand{\\hlcom}[1]{\\textcolor[rgb]{0.561,0.349,0.008}{\\textit{#1}}}
\\newcommand{\\hlopt}[1]{\\textcolor[rgb]{0.808,0.361,0.000}{\\textbf{#1}}}
\\newcommand{\\hlstd}[1]{\\textcolor[rgb]{0.000,0.000,0.000}{#1}}
\\newcommand{\\hldef}[1]{\\textcolor[rgb]{0.000,0.000,0.000}{#1}}
\\newcommand{\\hlkwd}[1]{\\textcolor[rgb]{0.125,0.290,0.529}{\\textbf{#1}}}
\\newcommand{\\hlkwc}[1]{\\textcolor[rgb]{0.561,0.349,0.008}{#1}}
\\newcommand{\\hlkwa}[1]{\\textcolor[rgb]{0.125,0.290,0.529}{\\textbf{#1}}}
\\newcommand{\\hlkwb}[1]{\\textcolor[rgb]{0.125,0.290,0.529}{#1}}
\\newcommand{\\hlkwr}[1]{\\textcolor[rgb]{0.647,0.000,0.000}{\\textbf{#1}}}
'

.header.framed =
'\\definecolor{shadecolor}{rgb}{0.969, 0.969, 0.969}
\\definecolor{messagecolor}{rgb}{0, 0, 0}
\\definecolor{warningcolor}{rgb}{1, 0, 1}
\\definecolor{errorcolor}{rgb}{1, 0, 0}
\\newenvironment{knitrout}{}{}
\\newenvironment{kframe}{}{}
'

.header.maxwidth =
'\\makeatletter
\\def\\maxwidth{ %
  \\ifdim\\Gin@nat@width>\\linewidth
    \\linewidth
  \\else
    \\Gin@nat@width
  \\fi
}
\\makeatother
'

insert_header = function(doc) {
  if (is.null(b <- knit_patterns$get('header.begin'))) return(doc)
  if (out_format(c('latex', 'sweave')))
    return(insert_header_latex(doc, b))
  doc
}

insert_header_latex = function(doc, b) {
  i = grep(b, doc)
  if (length(i) >= 1L) {
    header = make_header_latex(doc)
    i = i[1L]
    l = str_locate(doc[i], b, FALSE)
    doc[i] = str_insert(doc[i], l[, 2], header)
  }
  doc
}

str_insert = function(x, i, value) {
  if (i <= 0) return(paste0(value, x))
  n = nchar(x)
  if (n == 0 || i >= n) return(paste0(x, value))
  paste0(substr(x, 1, i), value, substr(x, i + 1, n))
}

make_header_latex = function(doc) {
  h = opts_knit$get('header')
  one_string(c(
    header_latex_packages(doc),
    .header.maxwidth,
    h[['framed']],
    h[['highlight']],
    '\\usepackage{alltt}'
  ))
}

header_latex_packages = function(doc) {
  use_package(c('graphicx', if (!grepl('xcolor', doc)) 'xcolor'), doc)
}

use_package = function(pkgs, doc) {
  paste(sapply(pkgs, function(p) {
    if (!nzchar(p)) return('')
    r = sprintf('.*?\\\\usepackage\\[(.+?)]\\{%s}.*', p)
    o = xfun::grep_sub(r, '\\1', doc)
    if (length(o)) return(sprintf('\\usepackage[%s]{%s}', o[1], p))
    sprintf('\\usepackage{%s}', p)
  }), collapse = '')
}

build_preamble = function(processed) {
  preamble_lines = character()
  needed = list()

  if (!has_package(processed, 'xcolor'))
    needed$xcolor = '\\usepackage{xcolor}'

  if (!has_package(processed, 'alltt'))
    needed$alltt = '\\usepackage{alltt}'

  if (!has_definecolor(processed, 'shadecolor'))
    needed$shadecolor = .header.framed

  if (grepl('\\\\hlstd', processed)) {
    hl_cmds = c('hlnum', 'hlstr', 'hlsng', 'hlcom', 'hlopt', 'hlstd',
                'hldef', 'hlkwd', 'hlkwc', 'hlkwa', 'hlkwb', 'hlkwr')
    for (cmd in hl_cmds) {
      if (!has_newcommand(processed, cmd))
        needed[[cmd]] = .header.hi.tex
    }
  }

  if (grepl('\\\\includegraphics', processed)) {
    if (!has_package(processed, 'graphicx'))
      needed$graphicx = '\\usepackage{graphicx}'
  }

  needed
}

has_package = function(doc, pkg) {
  grepl(sprintf('\\\\usepackage(\\[.*?\\])?\\{%s\\}', pkg), doc, perl = TRUE)
}

has_newcommand = function(doc, cmd) {
  grepl(sprintf('\\\\newcommand(\\*)?\\{%s\\}', cmd), doc, perl = TRUE)
}

has_definecolor = function(doc, color) {
  grepl(sprintf('\\\\definecolor\\{%s\\}', color), doc, perl = TRUE)
}
