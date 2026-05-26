test_that("guess_output handles extensions", {
  expect_equal(knitrmini:::guess_output("file.Rnw"), "file.tex")
  expect_equal(knitrmini:::guess_output("file.snw"), "file.tex")
  expect_equal(knitrmini:::guess_output("file.stex"), "file.tex")
  expect_equal(knitrmini:::guess_output("file.Rmd"), "file.Rmd-out.tex")
  expect_equal(knitrmini:::guess_output("file.tex"), "file.tex-out.tex")
})

test_that("is_abs_path detects absolute paths", {
  expect_true(knitrmini:::is_abs_path("/absolute/path"))
  expect_true(knitrmini:::is_abs_path("/"))
  expect_true(knitrmini:::is_abs_path("C:/path"))
  expect_false(knitrmini:::is_abs_path("relative/path"))
  expect_false(knitrmini:::is_abs_path("./relative"))
  expect_false(knitrmini:::is_abs_path("justfile"))
})

test_that("normalize_path resolves existing files", {
  tf <- tempfile()
  writeLines("test", tf)
  on.exit(unlink(tf))
  result <- knitrmini:::normalize_path(tf)
  expect_true(grepl("^/", result))
  expect_true(grepl(basename(tf), result))
})

test_that("normalize_path resolves non-existing paths", {
  result <- knitrmini:::normalize_path("/home/user/../project/./figure/plot.pdf")
  expect_equal(result, "/home/project/figure/plot.pdf")
  result2 <- knitrmini:::normalize_path("relative/../path/./file")
  expect_equal(result2, "path/file")
})

test_that("normalize_path handles already absolute paths", {
  result <- knitrmini:::normalize_path("/already/absolute")
  expect_equal(result, "/already/absolute")
})

test_that("resolve_includegraphics makes relative paths absolute", {
  doc <- "\\includegraphics{figure/plot.pdf}"
  result <- knitrmini:::resolve_includegraphics(doc, "/home/user")
  expect_match(result, "/home/user/figure/plot.pdf")
})

test_that("resolve_includegraphics leaves absolute paths unchanged", {
  doc <- "\\includegraphics{/abs/path/plot.pdf}"
  result <- knitrmini:::resolve_includegraphics(doc, "/home/user")
  expect_equal(result, doc)
})

test_that("resolve_includegraphics handles optional args", {
  doc <- "\\includegraphics[width=0.5\\textwidth]{figure/plot.pdf}"
  result <- knitrmini:::resolve_includegraphics(doc, "/home/user")
  expect_match(result, "/home/user/figure/plot.pdf")
  expect_match(result, "width=0.5\\\\textwidth")
})

test_that("resolve_includegraphics handles nested braces in opts", {
  doc <- "\\includegraphics[trim={0 0 0 0},clip]{figure/plot.pdf}"
  result <- knitrmini:::resolve_includegraphics(doc, "/home/user")
  expect_match(result, "/home/user/figure/plot.pdf")
})

test_that("resolve_includegraphics handles starred variant", {
  doc <- "\\includegraphics*{figure/plot.pdf}"
  result <- knitrmini:::resolve_includegraphics(doc, "/home/user")
  expect_match(result, "/home/user/figure/plot.pdf")
})

test_that("resolve_includegraphics handles multiple on same line", {
  doc <- "a \\includegraphics{a.pdf} b \\includegraphics{b.pdf}"
  result <- knitrmini:::resolve_includegraphics(doc, "/home/user")
  expect_match(result, "/home/user/a.pdf")
  expect_match(result, "/home/user/b.pdf")
})

test_that("resolve_includegraphics no matches returns unchanged", {
  doc <- "plain text no graphics"
  result <- knitrmini:::resolve_includegraphics(doc, "/home/user")
  expect_equal(result, doc)
})

test_that("resolve_inputs inlines simple file", {
  .knitEnv <- knitrmini:::.knitEnv
  tmp_dir <- tempdir()
  child <- file.path(tmp_dir, "child.tex")
  writeLines("child content", child)
  on.exit(unlink(child))

  doc <- paste0("before \\input{child} after")
  old_dir <- .knitEnv$input.dir
  .knitEnv$input.dir <- tmp_dir
  on.exit(.knitEnv$input.dir <- old_dir, add = TRUE)

  result <- knitrmini:::resolve_inputs(doc)
  expect_match(result, "child content")
})

test_that("resolve_inputs handles non-existent file gracefully", {
  .knitEnv <- knitrmini:::.knitEnv
  doc <- "\\input{nonexistent}"
  old_dir <- .knitEnv$input.dir
  .knitEnv$input.dir <- tempdir()
  on.exit(.knitEnv$input.dir <- old_dir)

  result <- knitrmini:::resolve_inputs(doc)
  expect_equal(result, doc)
})

test_that("clean_output removes texlab markers", {
  doc <- "before\n\\newif\\iftexlab\nsome\nstuff\n\\fi\nafter"
  result <- knitrmini:::clean_output(doc)
  expect_match(result, "before")
  expect_match(result, "after")
  expect_false(grepl("texlab", result))
})

test_that("clean_output removes root comments", {
  doc <- "text\n% !root = main.tex\nmore"
  result <- knitrmini:::clean_output(doc)
  expect_match(result, "text")
  expect_match(result, "more")
  expect_false(grepl("!root", result))
})

test_that("check_color_definition errors on definecolor without xcolor", {
  expect_error(knitrmini:::check_color_definition("\\definecolor{foo}{rgb}{1,0,0}"),
    "xcolor")
})

test_that("check_color_definition passes with xcolor", {
  expect_silent(knitrmini:::check_color_definition("\\usepackage{xcolor}\n\\definecolor{foo}{rgb}{1,0,0}"))
})

test_that("shorten_error extracts line number and message", {
  err <- "! Undefined control sequence.\nl.42 \\somecommand"
  result <- knitrmini:::shorten_error(err)
  expect_match(result, "l\\.42")
})

test_that("parse_latex_log returns empty for missing file", {
  result <- knitrmini:::parse_latex_log("nonexistent.log")
  expect_named(result, c("errors", "warnings", "badboxes",
    "undefined_refs", "undefined_citations"))
  expect_length(result$errors, 0)
})

test_that("parse_latex_log extracts errors", {
  log_content <- c("! Undefined control sequence.", "l.1 \\foo", "", "")
  lf <- tempfile(fileext = ".log")
  writeLines(log_content, lf)
  on.exit(unlink(lf))
  result <- knitrmini:::parse_latex_log(lf)
  expect_length(result$errors, 1)
  expect_match(result$errors[1], "Undefined control sequence")
})

test_that("parse_latex_log extracts LaTeX warnings", {
  log_content <- c("LaTeX Warning: Citation `foo' undefined on page 1.", "")
  lf <- tempfile(fileext = ".log")
  writeLines(log_content, lf)
  on.exit(unlink(lf))
  result <- knitrmini:::parse_latex_log(lf)
  expect_length(result$warnings, 1)
  expect_length(result$undefined_citations, 1)
})

test_that("parse_latex_log extracts Package warnings", {
  log_content <- c("Package xcolor Warning: Some warning text.", "")
  lf <- tempfile(fileext = ".log")
  writeLines(log_content, lf)
  on.exit(unlink(lf))
  result <- knitrmini:::parse_latex_log(lf)
  expect_length(result$warnings, 1)
})

test_that("parse_latex_log extracts badboxes", {
  log_content <- c("Overfull \\hbox (12.0pt too wide) in paragraph", "")
  lf <- tempfile(fileext = ".log")
  writeLines(log_content, lf)
  on.exit(unlink(lf))
  result <- knitrmini:::parse_latex_log(lf)
  expect_length(result$badboxes, 1)
})

test_that("parse_latex_log extracts undefined refs from warnings", {
  log_content <- c("LaTeX Warning: Reference `foo' undefined on page 1.", "")
  lf <- tempfile(fileext = ".log")
  writeLines(log_content, lf)
  on.exit(unlink(lf))
  result <- knitrmini:::parse_latex_log(lf)
  expect_length(result$undefined_refs, 1)
})

test_that("knit without input file returns tex string", {
  tf <- tempfile(fileext = ".Rnw")
  writeLines(c(
    "\\documentclass{article}",
    "\\begin{document}",
    "<<a>>=",
    "1+1",
    "@",
    "\\end{document}"
  ), tf)
  tex_file <- sub("Rnw$", "tex", tf)
  on.exit(unlink(c(tf, tex_file)))
  result <- knit(tf, compile = FALSE, quiet = TRUE)
  expect_true(file.exists(tex_file))
  tex_content <- readLines(tex_file, warn = FALSE)
  expect_true(any(grepl("end{document}", tex_content, fixed = TRUE)))
})

test_that("knit2pdf runs full knit + compile pipeline", {
  tf <- make_rnw("<<a>>=\n1+1\n@")
  on.exit(unlink(c(tf, sub("Rnw$", "tex", tf), sub("Rnw$", "pdf", tf))))
  result <- knit2pdf(tf, quiet = TRUE)
  expect_true(file.exists(result))
  expect_match(result, "\\.pdf$")
})
