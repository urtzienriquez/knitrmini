# knitrmini

[![CI](https://github.com/urtzienriquez/knitrmini/actions/workflows/CI.yml/badge.svg)](https://github.com/urtzienriquez/knitrmini/actions/workflows/CI.yml)
[![codecov](https://codecov.io/github/urtzienriquez/knitrmini/graph/badge.svg?token=B5L9VSWW7D)](https://codecov.io/github/urtzienriquez/knitrmini)

> Minimal knitr for Rnw → TeX → PDF conversion

## Overview

Heavily inspired by and based on [knitr](https://yihui.org/knitr/) — Yihui Xie's excellent and comprehensive literate programming package — **knitrmini** is a stripped-down reimplementation focused exclusively on one workflow: R Noweb (.Rnw) to LaTeX to PDF.

## Philosophy: Least obtrusive, maximum control

knitrmini is designed to be the **least obtrusive** literate programming engine possible. Unlike knitr, which wraps code output in `\begin{knitrout}` and `\begin{kframe}` environments and injects significant preamble boilerplate, knitrmini generates clean LaTeX using only standard environments and injects **nothing into your preamble unless the output actually needs it** — and even then, only if you haven't already defined it yourself.

| What triggers it | What gets injected |
|---|---|
| Echoed code with syntax highlighting (`echo=TRUE`, `highlight=TRUE`) | `\usepackage{xcolor}`, `\usepackage{fancyvrb}`, `\usepackage{framed}`, `\DefineVerbatimEnvironment{Highlighting}`, `\newenvironment{Shaded}`, `\hlnum`/`\hlstr`/etc. color commands |
| Echoed code with minted (`echo=TRUE`, `minted=TRUE`) | `\usepackage{minted}`, `\usemintedstyle{...}`, `\definecolor{knitbg}` |
| Plots produced by a chunk | `\usepackage{graphicx}`, `\maxwidth` macro |
| Warnings (`warning=TRUE`) | `\definecolor{warningcolor}` (magenta) |
| Messages (`message=TRUE`) | `\definecolor{messagecolor}` (black) |
| Errors (`error=TRUE`) | `\definecolor{errorcolor}` (red) |

Each injection is **conditional**: if you already have `\usepackage{graphicx}` in your .Rnw preamble, it won't be added. If you already defined `\maxwidth`, it won't be duplicated. The result is a TeX file that looks like you wrote it yourself — because knitrmini stays out of your way.

This means **you are responsible for writing proper LaTeX**. But in exchange, you have full control over the final document — no unexpected wrappers, no hidden boilerplate.

## Usage

```r
library(knitrmini)

# One step: knit and compile to PDF
knit("document.Rnw")

# Two steps: knit first, then compile separately
knit("document.Rnw", compile = FALSE)
knit2pdf("document.Rnw")

# With minted syntax highlighting
knit("document.Rnw", minted = TRUE, minted_style = "tango")
```

The default LaTeX engine is `pdflatex`. Change it globally:

```r
opts_knit$set(engine = "xelatex")
# or per-call:
knit("document.Rnw", engine = "xelatex")
```

## Chunk options

Set via `opts_chunk$set(...)` or per-chunk in `<<...>>=` headers:

| Option | Default | Description |
|---|---|---|
| `eval` | `TRUE` | Evaluate code |
| `echo` | `TRUE` | Display source code |
| `results` | `"markup"` | `"markup"`, `"asis"`, `"hold"`, or `"hide"` |
| `highlight` | `TRUE` | Syntax highlighting for source |
| `minted` | `FALSE` | Use minted instead of Shaded for highlighting |
| `minted_style` | `NULL` | Minted style name (e.g. `"tango"`) |
| `comment` | `"##"` | Comment prefix for text output |
| `size` | `"normalsize"` | Font size for chunk output |
| `cache` | `FALSE` | Cache chunk results |
| `cache.path` | `"cache/"` | Cache directory |
| `fig.width` / `fig.height` | `7` / `7` | Figure dimensions (inches) |
| `fig.cap` | `NULL` | Figure caption |
| `fig.env` | `NULL` | Figure environment (auto `"figure"` if caption set) |
| `fig.pos` | `""` | Figure position (e.g. `"!htbp"`) |
| `fig.lp` | `"fig:"` | Figure label prefix |
| `out.width` / `out.height` | `NULL` | Output dimensions in LaTeX (e.g. `\maxwidth`, `0.5\textwidth`) |
| `dev` | `"pdf"` | Graphics device (`"pdf"`, `"png"`, `"svg"`) |
| `dpi` | `72` | Resolution for raster devices |
| `warning` | `TRUE` | Show warnings (`TRUE`, `FALSE`, or max count) |
| `message` | `TRUE` | Show messages (`TRUE`, `FALSE`, or max count) |
| `error` | `TRUE` | Show errors (`TRUE` = continue, `FALSE` = stop) |
| `include` | `TRUE` | Include chunk output in document |
| `child` | `NULL` | Path to child .Rnw document |
| `engine` | `"R"` | Code engine (only `"R"` currently) |
| `ref.label` | `NULL` | Reference label of another chunk to reuse code |

## Global options

Set via `opts_knit$set(...)`:

| Option | Default | Description |
|---|---|---|
| `engine` | `"pdflatex"` | LaTeX compiler (`"pdflatex"`, `"xelatex"`, `"lualatex"`) |
| `progress` | `TRUE` | Show progress messages |
| `root.dir` | `NULL` | Root directory for chunk execution |
| `unnamed.chunk.label` | `"unnamed-chunk"` | Prefix for unnamed chunks |
| `minted_style` | `NULL` | Default minted style |
| `resolve_input` | `TRUE` | Inline `\input`/`\include` files |

## Editor Support

`.Rnw` (and `.rnoweb`) files use the **R NoWeb** format — a literate programming format where LaTeX prose is interleaved with R code chunks delimited by `<<...>>= ... @`. For syntax highlighting and proper editing support, use the [tree-sitter-rnoweb](https://github.com/urtzienriquez/tree-sitter-rnoweb) grammar.

### Quick setup (Neovim)

1. Register the filetype:
   ```lua
   vim.filetype.add({ extension = { Rnw = "rnoweb", rnoweb = "rnoweb" } })
   ```

2. Compile and register the parser:
   ```bash
   gcc -O2 -shared -I src src/parser.c src/scanner.c \
     -o ~/.local/share/nvim/site/parser/rnoweb.so
   ```

3. Symlink the query files for highlighting and injections:
   ```bash
   ln -sf /path/to/tree-sitter-rnoweb/queries/highlights.scm ~/.config/nvim/queries/rnoweb/
   ln -sf /path/to/tree-sitter-rnoweb/queries/injections.scm ~/.config/nvim/queries/rnoweb/
   ```

The tree-sitter-rnoweb repo also ships [`literateR-fmt`](https://github.com/urtzienriquez/tree-sitter-rnoweb#formatting), a formatter that formats R chunks with styler and LaTeX prose with latexindent.

## Dependencies

- **evaluate** — parse and evaluate R code chunks
- **highr** — syntax highlighting for R code
- **xfun** — miscellaneous utilities
- **digest** — MD5 hashing for cache keys
- **tinytex** (suggested) — automatic LaTeX installation

## License

GPL
