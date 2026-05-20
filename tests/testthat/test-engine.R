library(knitrmini)

test_that("dev2ext maps device names to extensions", {
  expect_equal(knitrmini:::dev2ext(list(dev = "pdf")), "pdf")
  expect_equal(knitrmini:::dev2ext(list(dev = "png")), "png")
  expect_equal(knitrmini:::dev2ext(list(dev = "svg")), "svg")
  expect_equal(knitrmini:::dev2ext(list(dev = "jpeg")), "jpg")
  expect_equal(knitrmini:::dev2ext(list(dev = "tikz")), "tex")
  expect_equal(knitrmini:::dev2ext(list(dev = "unknown")), "pdf")
  expect_equal(knitrmini:::dev2ext(list()), "pdf")
})

test_that("is_plot_output detects recorded plots", {
  expect_false(knitrmini:::is_plot_output("character"))
  expect_false(knitrmini:::is_plot_output(42))
  expect_false(knitrmini:::is_plot_output(list()))
})

test_that("as.source creates source structure", {
  result <- knitrmini:::as.source("1+1")
  expect_s3_class(result[[1]], "source")
  expect_equal(result[[1]]$src, "1+1")
})

test_that("merge_class merges adjacent source blocks", {
  s1 <- structure(list(src = "a"), class = "source")
  s2 <- structure(list(src = "b"), class = "source")
  res <- list(s1, s2)
  merged <- knitrmini:::merge_class(res)
  expect_length(merged, 1)
  expect_equal(merged[[1]]$src, c("a", "b"))
})

test_that("merge_class leaves non-adjacent blocks separate", {
  s1 <- structure(list(src = "a"), class = "source")
  s2 <- structure(list(src = "b"), class = "source")
  res <- list(s1, "text", s2)
  merged <- knitrmini:::merge_class(res)
  expect_length(merged, 3)
})

test_that("merge_plots 'none' removes all plots", {
  res <- list("a", structure(1, class = "recordedplot"), "b",
    structure(2, class = "recordedplot"))
  result <- knitrmini:::merge_plots(res, "none")
  expect_equal(result, list("a", "b"))
})

test_that("merge_plots 'first' keeps first plot only", {
  res <- list("a", structure(1, class = "recordedplot"),
    structure(2, class = "recordedplot"), "b")
  result <- knitrmini:::merge_plots(res, "first")
  expect_true("b" %in% result)
  expect_length(result, 3)
})

test_that("merge_plots 'last' keeps last plot", {
  res <- list("a", structure(1, class = "recordedplot"),
    structure(2, class = "recordedplot"), "b")
  result <- knitrmini:::merge_plots(res, "last")
  expect_true("b" %in% result)
})

test_that("merge_plots 'high' keeps last in each contiguous run", {
  res <- list("a", structure(1, class = "recordedplot"),
    structure(2, class = "recordedplot"), "b",
    structure(3, class = "recordedplot"), "c")
  result <- knitrmini:::merge_plots(res, "high")
  expect_true("a" %in% result)
  expect_true("b" %in% result)
  expect_true("c" %in% result)
})

test_that("merge_plots numeric keeps specified indices", {
  res <- list("a", structure(1, class = "recordedplot"),
    structure(2, class = "recordedplot"), "b")
  result <- knitrmini:::merge_plots(res, 1)
  expect_length(result, 4)
})

test_that("merge_plots returns unchanged for no plots", {
  res <- list("a", "b", "c")
  result <- knitrmini:::merge_plots(res, "none")
  expect_equal(result, res)
})

test_that("merge_character merges adjacent same-class elements", {
  result <- knitrmini:::merge_character(list("a", "b", "c"))
  expect_length(result, 1)
  expect_equal(result[[1]], "abc")
})

test_that("merge_character single element unchanged", {
  result <- knitrmini:::merge_character(list("a"))
  expect_equal(result, list("a"))
})

test_that("filter_evaluate filters by numeric opt", {
  w1 <- structure("warning1", class = "warning")
  w2 <- structure("warning2", class = "warning")
  res <- list("text", w1, w2)
  result <- knitrmini:::filter_evaluate(res, 1, evaluate::is.warning)
  expect_length(result, 2)
})

test_that("filter_evaluate returns unchanged when opt not numeric", {
  res <- list("text")
  result <- knitrmini:::filter_evaluate(res, TRUE, evaluate::is.warning)
  expect_equal(result, res)
})

test_that("sew dispatches to correct hooks", {
  old_hooks <- knit_hooks$get()
  on.exit(knit_hooks$restore(old_hooks))

  knit_hooks$set(
    source = function(x, opts) "SOURCE",
    output = function(x, opts) "OUTPUT",
    warning = knitrmini:::.color.block("", ""),
    message = knitrmini:::.color.block("", ""),
    error = knitrmini:::.color.block("", ""),
    plot = function(x, opts) "PLOT"
  )

  src <- structure(list(src = "code"), class = "source")
  out <- "text output"
  result <- knitrmini:::sew(list(src, out), list())
  expect_equal(result[[1]], "SOURCE")
  expect_equal(result[[2]], "OUTPUT")
})

test_that("eval_lang evaluates calls", {
  env <- list2env(list(x = 42))
  expect_equal(knitrmini:::eval_lang(quote(x), env), 42)
  expect_equal(knitrmini:::eval_lang(quote(1 + 1)), 2)
  expect_equal(knitrmini:::eval_lang("string"), "string")
  expect_equal(knitrmini:::eval_lang(42), 42)
})

test_that("knit_print.default handles character and non-character", {
  expect_equal(knitrmini:::knit_print.default("hello", inline = FALSE), "hello")
  expect_equal(knitrmini:::knit_print.default(c("a", "b"), inline = FALSE), "a\nb")
  expect_equal(knitrmini:::knit_print.default("hello", inline = TRUE), "hello")
})

test_that("input_dir uses root.dir first, then input.dir, then '.'", {
  .knitEnv <- knitrmini:::.knitEnv
  old_root <- opts_knit$get("root.dir")
  old_input <- .knitEnv$input.dir
  on.exit({
    opts_knit$set(root.dir = old_root)
    .knitEnv$input.dir <- old_input
  })

  opts_knit$set(root.dir = "/root/path")
  .knitEnv$input.dir <- "/input/path"
  expect_equal(knitrmini:::input_dir(), "/root/path")

  opts_knit$set(root.dir = NULL)
  expect_equal(knitrmini:::input_dir(), "/input/path")

  .knitEnv$input.dir <- NULL
  expect_equal(knitrmini:::input_dir(), ".")
})

test_that("child_mode returns correct status", {
  old <- opts_knit$get("child")
  on.exit(opts_knit$set(child = old))
  opts_knit$set(child = TRUE)
  expect_true(knitrmini:::child_mode())
  opts_knit$set(child = FALSE)
  expect_false(knitrmini:::child_mode())
})

test_that("in_dir changes and restores directory", {
  old <- getwd()
  tmp <- tempdir()
  knitrmini:::in_dir(tmp, {
    expect_equal(getwd(), normalizePath(tmp))
  })
  expect_equal(getwd(), old)
})

test_that("get_code returns code from params", {
  params <- list(code = "1+1", label = "x", file = NULL)
  result <- knitrmini:::get_code(params, "x", "x")
  expect_equal(result, "1+1")
})

test_that("set_code stores code in knit_code", {
  knitrmini:::set_code("testchunk", "1+1")
  expect_equal(knit_code$get("testchunk"), "1+1")
  knit_code$delete("testchunk")
})

test_that("inline_exec replaces inline code", {
  block <- list(
    code = "1+1",
    input = "result: \\Sexpr{1+1}",
    location = matrix(c(9, 19), nrow = 1)
  )
  result <- knitrmini:::inline_exec(block)
  expect_equal(result, "result: 2")
})

test_that("inline_exec with no code returns input unchanged", {
  block <- list(code = character(0), input = "plain text", location = matrix(0, 0, 2))
  expect_equal(knitrmini:::inline_exec(block), "plain text")
})

test_that("hilight_source returns highlighted code", {
  result <- knitrmini:::hilight_source("1+1")
  expect_true(is.character(result))
  expect_true(length(result) > 0)
})

test_that("as.source creates proper source structure", {
  result <- knitrmini:::as.source("test code")
  expect_length(result, 1)
  expect_s3_class(result[[1]], "source")
  expect_equal(result[[1]]$src, "test code")
})
