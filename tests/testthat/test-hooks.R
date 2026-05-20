test_that(".inline.hook handles numeric and character", {
  expect_equal(knitrmini:::.inline.hook(1.234), "1.234")
  expect_equal(knitrmini:::.inline.hook(c(1, 2, 3)), "1, 2, 3")
  expect_equal(knitrmini:::.inline.hook("text"), "text")
})

test_that(".inline.hook.tex formats scientific notation", {
  result <- knitrmini:::.inline.hook.tex(1000000)
  expect_match(result, "10\\^\\{6\\}")
  expect_true(is.character(result))
})

test_that(".verb.hook wraps in verbatim", {
  result <- knitrmini:::.verb.hook("code")
  expect_match(result, "\\\\begin\\{verbatim\\}")
  expect_match(result, "\\\\end\\{verbatim\\}")
})

test_that(".chunk.hook.tex handles asis output", {
  result <- knitrmini:::.chunk.hook.tex("text", list(results = "asis", size = "normalsize"))
  expect_equal(result, "text")
})

test_that(".chunk.hook.tex applies size", {
  result <- knitrmini:::.chunk.hook.tex("text", list(results = "markup", size = "footnotesize"))
  expect_match(result, "\\\\footnotesize")
})

test_that("render_figures without caption, no figure env", {
  result <- knitrmini:::render_figures("figure/test-1.pdf", list(fig.pos = "", fig.env = NULL, fig.lp = "fig:", fig.cap = NULL, label = "test", out.width = NULL, out.height = NULL))
  expect_match(result, "\\\\includegraphics\\{figure/test-1\\}")
})

test_that("render_figures with caption adds figure env", {
  result <- knitrmini:::render_figures("figure/test-1.pdf", list(fig.pos = "", fig.env = NULL, fig.lp = "fig:", fig.cap = "A caption", label = "test", out.width = NULL, out.height = NULL))
  expect_match(result, "\\\\begin\\{figure\\}")
  expect_match(result, "\\\\caption\\{A caption\\}")
  expect_match(result, "\\\\end\\{figure\\}")
})

test_that("render_figures with out.width adds attributes", {
  result <- knitrmini:::render_figures("figure/test-1.pdf", list(fig.pos = "", fig.env = NULL, fig.lp = "fig:", fig.cap = NULL, label = "test", out.width = "0.5\\textwidth", out.height = NULL))
  expect_match(result, "\\\\includegraphics\\[width=0.5\\\\textwidth\\]")
})

test_that("render_figures handles tikz .tex files with \\input", {
  result <- knitrmini:::render_figures("figure/test-1.tex", list(fig.pos = "", fig.env = NULL, fig.lp = "fig:", fig.cap = NULL, label = "test", out.width = NULL, out.height = NULL))
  expect_match(result, "\\\\input\\{figure/test-1\\}")
})

test_that("render_figures with fig.env set", {
  result <- knitrmini:::render_figures("figure/test-1.pdf", list(fig.pos = "H", fig.env = "figure", fig.lp = "fig:", fig.cap = NULL, label = "test", out.width = NULL, out.height = NULL))
  expect_match(result, "\\\\begin\\{figure\\}\\[H\\]")
})

test_that("render_figures adds label when label and f_env present", {
  result <- knitrmini:::render_figures("figure/test-1.pdf", list(fig.pos = "", fig.env = "figure", fig.lp = "fig:", fig.cap = NULL, label = "mychunk", out.width = NULL, out.height = NULL))
  expect_match(result, "\\\\label\\{fig:mychunk\\}")
})

test_that("render_figures multiple figures", {
  result <- knitrmini:::render_figures(c("fig1.pdf", "fig2.pdf"), list(fig.pos = "", fig.env = NULL, fig.lp = "fig:", fig.cap = NULL, label = "test", out.width = NULL, out.height = NULL))
  expect_match(result, "\\\\includegraphics\\{fig1\\}")
  expect_match(result, "\\\\includegraphics\\{fig2\\}")
})

test_that("output_asis identifies blank and asis options", {
  expect_true(knitrmini:::output_asis("", list(results = "markup")))
  expect_true(knitrmini:::output_asis("text", list(results = "asis")))
  expect_false(knitrmini:::output_asis("text", list(results = "markup")))
})

test_that("escape_latex escapes special characters", {
  expect_equal(knitrmini:::escape_latex("\\"), "\\textbackslash{}")
  expect_equal(knitrmini:::escape_latex("{"), "\\{")
  expect_equal(knitrmini:::escape_latex("}"), "\\}")
  expect_equal(knitrmini:::escape_latex("$%&_#^~"),
    "\\$\\%\\&\\_\\#\\^{}\\textasciitilde{}")
})

test_that("escape_latex leaves normal text unchanged", {
  expect_equal(knitrmini:::escape_latex("hello world"), "hello world")
  expect_equal(knitrmini:::escape_latex("alpha beta"), "alpha beta")
})

test_that("sans_ext strips extension", {
  expect_equal(knitrmini:::sans_ext("file.pdf"), "file")
  expect_equal(knitrmini:::sans_ext("path/to/file.ext"), "path/to/file")
  expect_equal(knitrmini:::sans_ext("noext"), "noext")
  expect_equal(knitrmini:::sans_ext("file.name.ext"), "file.name")
})

test_that(".color.block wraps with color and ttfamily", {
  hook <- knitrmini:::.color.block("\\color{red}{", "}")
  result <- hook("test message", list())
  expect_match(result, "\\\\ttfamily")
  expect_match(result, "\\\\color\\{red\\}")
  expect_match(result, "test message")
})

test_that("format_sci_one handles special values", {
  expect_equal(knitrmini:::format_sci_one(0), "0")
  expect_equal(knitrmini:::format_sci_one(NA_real_), "NA")
  expect_equal(knitrmini:::format_sci_one(Inf), "\\infty{}")
  expect_equal(knitrmini:::format_sci_one(-Inf), "-\\infty{}")
})

test_that("format_sci_one formats small numbers", {
  result <- knitrmini:::format_sci_one(0.0001)
  expect_match(result, "10\\^\\{-4\\}")
})

test_that("format_sci handles roman", {
  x <- structure(1, class = "roman")
  expect_equal(knitrmini:::format_sci(x), "I")
})

test_that("hooks_latex returns list with expected names", {
  h <- knitrmini:::hooks_latex()
  expect_named(h, c("source", "output", "warning", "message", "error",
    "inline", "chunk", "plot", "text", "document"))
})

test_that("render_latex sets expected options", {
  old_fmt <- opts_knit$get("out.format")
  old_width <- opts_chunk$get("out.width")
  old_dev <- opts_chunk$get("dev")
  on.exit({
    opts_knit$set(out.format = old_fmt)
    opts_chunk$set(out.width = old_width, dev = old_dev)
  })
  render_latex()
  expect_equal(opts_knit$get("out.format"), "latex")
  expect_equal(opts_chunk$get("out.width"), "\\maxwidth")
  expect_equal(opts_chunk$get("dev"), "pdf")
})

test_that("sans_ext handles edge cases", {
  expect_equal(knitrmini:::sans_ext("a.b.c"), "a.b")
  expect_equal(knitrmini:::sans_ext("."), ".")
  expect_equal(knitrmini:::sans_ext(".hidden"), ".hidden")
})
