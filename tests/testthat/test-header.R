library(knitrmini)

test_that("build_preamble empty doc returns empty", {
  result <- knitrmini:::build_preamble("plain text no special commands")
  expect_equal(result, character(0))
})

test_that("build_preamble with Shaded adds LaTeX packages", {
  doc <- "\\begin{Shaded}\ncode\n\\end{Shaded}"
  result <- knitrmini:::build_preamble(doc)
  expect_true(any(grepl("xcolor", result)))
  expect_true(any(grepl("fancyvrb", result)))
  expect_true(any(grepl("framed", result)))
  expect_true(any(grepl("shadecolor", result)))
  expect_true(any(grepl("Highlighting", result)))
  expect_true(any(grepl("Shaded", result)))
})

test_that("build_preamble with highlighting commands", {
  doc <- "\\hlkwd{fun}"
  result <- knitrmini:::build_preamble(doc)
  expect_true(any(grepl("hlnum", result)))
})

test_that("build_preamble with includegraphics adds graphicx", {
  doc <- "\\includegraphics{file.pdf}"
  result <- knitrmini:::build_preamble(doc)
  expect_true(any(grepl("graphicx", result)))
})

test_that("build_preamble with maxwidth adds macro", {
  doc <- "\\maxwidth"
  result <- knitrmini:::build_preamble(doc)
  expect_true(any(grepl("maxwidth", result)))
})

test_that("build_preamble with tikz input adds tikz package", {
  doc <- "\\input{figure/plot-1}"
  result <- knitrmini:::build_preamble(doc)
  expect_true(any(grepl("tikz", result)))
})

test_that("build_preamble adds message/warning/error colors", {
  doc <- "messagecolor warningcolor errorcolor"
  result <- knitrmini:::build_preamble(doc)
  expect_true(any(grepl("messagecolor", result)))
  expect_true(any(grepl("warningcolor", result)))
  expect_true(any(grepl("errorcolor", result)))
})

test_that("build_preamble adds shadecolor only when !has_shaded", {
  doc <- "shadecolor used in doc"
  result <- knitrmini:::build_preamble(doc)
  expect_true(any(grepl("shadecolor", result)))
})

test_that("build_preamble with beamer adds fontsize option", {
  doc <- "\\documentclass{beamer}\n\\begin{Shaded}\ncode\n\\end{Shaded}"
  result <- knitrmini:::build_preamble(doc)
  hl_line <- grep("Highlighting", result, value = TRUE)
  expect_true(any(grepl("fontsize", hl_line)))
})

test_that("build_preamble with minted adds minted package", {
  doc <- "\\begin{minted}{r}\ncode\n\\end{minted}"
  result <- knitrmini:::build_preamble(doc)
  expect_true(any(grepl("minted", result)))
})

test_that("build_preamble does not add redundant xcolor", {
  doc <- "\\usepackage{xcolor}\n\\begin{Shaded}\ncode\n\\end{Shaded}"
  result <- knitrmini:::build_preamble(doc)
  expect_false(any(grepl("xcolor", result)))
})

test_that("has_package detects usepackage", {
  doc <- "\\usepackage{graphicx}"
  expect_true(knitrmini:::has_package(doc, "graphicx"))
  expect_false(knitrmini:::has_package(doc, "nonexistent"))

  doc2 <- "\\usepackage[opts]{graphicx}"
  expect_true(knitrmini:::has_package(doc2, "graphicx"))
})

test_that("has_newcommand detects newcommand", {
  doc <- "\\newcommand{foo}{bar}"
  expect_true(knitrmini:::has_newcommand(doc, "foo"))
  expect_false(knitrmini:::has_newcommand(doc, "nonexistent"))

  doc2 <- "\\newcommand*{foo}{bar}"
  expect_true(knitrmini:::has_newcommand(doc2, "foo"))
})

test_that("has_newenvironment detects newenvironment", {
  doc <- "\\newenvironment{myenv}{begin}{end}"
  expect_true(knitrmini:::has_newenvironment(doc, "myenv"))
  expect_false(knitrmini:::has_newenvironment(doc, "nonexistent"))
})

test_that("has_definecolor detects definecolor", {
  doc <- "\\definecolor{mycolor}{rgb}{1,0,0}"
  expect_true(knitrmini:::has_definecolor(doc, "mycolor"))
  expect_false(knitrmini:::has_definecolor(doc, "nonexistent"))
})

test_that("has_defineverbatimenvironment detects DefineVerbatimEnvironment", {
  doc <- "\\DefineVerbatimEnvironment{MyVerbatim}{Verbatim}{}"
  expect_true(knitrmini:::has_defineverbatimenvironment(doc, "MyVerbatim"))
  expect_false(knitrmini:::has_defineverbatimenvironment(doc, "nonexistent"))
})

test_that("strip_latex_comments handles comments", {
  expect_equal(knitrmini:::strip_latex_comments("text % comment"), "text ")
  expect_equal(knitrmini:::strip_latex_comments("\\% not a comment"), "\\% not a comment")
  expect_equal(knitrmini:::strip_latex_comments("no comment"), "no comment")
  expect_equal(knitrmini:::strip_latex_comments("% full line comment"), "")
})

test_that("insert_header adds header before document begin", {
  old_fmt <- opts_knit$get("out.format")
  on.exit(opts_knit$set(out.format = old_fmt))
  opts_knit$set(out.format = "latex")
  doc <- "\\documentclass{article}\n\\includegraphics{file.pdf}\n\\begin{document}\ntext"
  result <- knitrmini:::insert_header(doc)
  expect_true(grepl("graphicx", result))
})

test_that("insert_header returns unchanged for non-latex format", {
  old <- opts_knit$get("out.format")
  on.exit(opts_knit$set(out.format = old))
  opts_knit$set(out.format = "html")
  doc <- "plain text"
  expect_equal(knitrmini:::insert_header(doc), doc)
})

test_that("set_header merges with existing headers", {
  old <- opts_knit$get("header")
  on.exit(opts_knit$set(header = old))
  set_header(highlight = "test")
  expect_equal(opts_knit$get("header")[["highlight"]], "test")
})
