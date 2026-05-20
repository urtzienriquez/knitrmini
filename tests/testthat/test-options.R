test_that("new_defaults get/set/delete", {
  d <- knitrmini:::new_defaults(list(a = 1, b = 2))
  expect_equal(d$get("a"), 1)
  expect_equal(d$get("b"), 2)
  expect_equal(d$get(), list(a = 1, b = 2))

  d$set(a = 10)
  expect_equal(d$get("a"), 10)

  d$delete("a")
  expect_null(d$get("a"))
})

test_that("new_defaults get with default fallback", {
  d <- knitrmini:::new_defaults(list(x = 1))
  expect_equal(d$get("x"), 1)
  expect_null(d$get("y"))
})

test_that("new_defaults restore", {
  d <- knitrmini:::new_defaults(list(a = 1))
  d$set(a = 99)
  d$restore()
  expect_equal(d$get("a"), 1)

  d$set(a = 99)
  d$restore(list(a = 42))
  expect_equal(d$get("a"), 42)
})

test_that("new_defaults merge", {
  d <- knitrmini:::new_defaults(list(a = 1, b = 2))
  merged <- d$merge(list(b = 3, c = 4))
  expect_equal(merged, list(a = 1, b = 3, c = 4))
  expect_equal(d$get("a"), 1)
})

test_that("new_defaults append", {
  d <- knitrmini:::new_defaults(list(x = "a"))
  d$append(list(x = "b"))
  expect_equal(d$get("x"), c("a", "b"))
})

test_that("%n% operator", {
  `%n%` <- knitrmini:::`%n%`
  expect_equal(NULL %n% "fallback", "fallback")
  expect_equal("value" %n% "fallback", "value")
  expect_equal(0 %n% 1, 0)
  expect_equal(character(0) %n% "fallback", character(0))
})

test_that("merge_list merges correctly", {
  x <- list(a = 1, b = 2)
  y <- list(b = 3, c = 4)
  result <- knitrmini:::merge_list(x, y)
  expect_equal(result, list(a = 1, b = 3, c = 4))
  expect_equal(result$b, 3)
})

test_that("one_string collapses with newline", {
  expect_equal(knitrmini:::one_string(c("a", "b", "c")), "a\nb\nc")
  expect_equal(knitrmini:::one_string("single"), "single")
  expect_equal(knitrmini:::one_string(character(0)), "")
})

test_that("split_lines handles line endings", {
  expect_equal(knitrmini:::split_lines("a\nb\nc"), c("a", "b", "c"))
  expect_equal(knitrmini:::split_lines("a\r\nb\r\nc"), c("a", "b", "c"))
  expect_equal(knitrmini:::split_lines("a\rb\rc"), c("a", "b", "c"))
  expect_equal(knitrmini:::split_lines(""), "")
  expect_equal(knitrmini:::split_lines("single"), "single")
  expect_equal(knitrmini:::split_lines(""), "")
})

test_that("is_blank", {
  expect_true(knitrmini:::is_blank(""))
  expect_true(knitrmini:::is_blank("   "))
  expect_true(knitrmini:::is_blank("\t\n"))
  expect_true(knitrmini:::is_blank(character(0)))
  expect_false(knitrmini:::is_blank("a"))
  expect_false(knitrmini:::is_blank(" a "))
})

test_that("sc_split splits by comma and semicolon", {
  expect_equal(knitrmini:::sc_split("a,b,c"), c("a", "b", "c"))
  expect_equal(knitrmini:::sc_split("a;b;c"), c("a", "b", "c"))
  expect_equal(knitrmini:::sc_split("a, b; c"), c("a", "b", "c"))
  expect_equal(knitrmini:::sc_split("single"), "single")
  expect_equal(knitrmini:::sc_split(numeric()), numeric())
  expect_equal(knitrmini:::sc_split(1:3), 1:3)
})

test_that("valid_path concatenates prefix and label", {
  expect_equal(knitrmini:::valid_path("figure/", "chunk1"), "figure/chunk1")
  expect_equal(knitrmini:::valid_path("", "chunk1"), "chunk1")
  expect_equal(knitrmini:::valid_path(NULL, "chunk1"), "chunk1")
  expect_equal(knitrmini:::valid_path(NA, "chunk1"), "chunk1")
  expect_equal(knitrmini:::valid_path("NA", "chunk1"), "chunk1")
})

test_that("opts_chunk has expected defaults", {
  expect_true(opts_chunk$get("eval"))
  expect_true(opts_chunk$get("echo"))
  expect_equal(opts_chunk$get("fig.path"), "figure/")
  expect_equal(opts_chunk$get("engine"), "R")
})

test_that("opts_knit has expected defaults", {
  expect_true(opts_knit$get("resolve_input"))
  expect_true(opts_knit$get("normalize_paths"))
  expect_equal(opts_knit$get("unnamed.chunk.label"), "unnamed-chunk")
  expect_null(opts_knit$get("root.dir"))
})

test_that("opts_knit get preserves names with drop=FALSE", {
  d <- knitrmini:::new_defaults(list(a = 1, b = 2))
  result <- d$get("a", drop = FALSE)
  expect_type(result, "list")
  expect_named(result, "a")
})
