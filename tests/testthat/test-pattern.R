test_that("detect_pattern returns rnw for .Rnw extension", {
  expect_equal(knitrmini:::detect_pattern("<<foo>>=\n1+1\n@", "rnw"), "rnw")
  expect_equal(knitrmini:::detect_pattern("<<foo>>=\n1+1\n@", "snw"), "rnw")
  expect_equal(knitrmini:::detect_pattern("<<foo>>=\n1+1\n@", "stex"), "rnw")
})

test_that("detect_pattern returns NULL for unknown extensions without chunk markers", {
  expect_null(knitrmini:::detect_pattern("plain text", "txt"))
  expect_null(knitrmini:::detect_pattern("no chunks here", "Rmd"))
})

test_that("out_format without argument returns current format", {
  old <- opts_knit$get("out.format")
  on.exit(opts_knit$set(out.format = old))
  opts_knit$set(out.format = "latex")
  expect_true(knitrmini:::out_format("latex"))
  expect_false(knitrmini:::out_format("html"))
  expect_equal(knitrmini:::out_format(NULL), "latex")
})

test_that("group_pattern validates single non-NA non-empty strings", {
  expect_true(knitrmini:::group_pattern("^\\Sexpr\\{([^}]+)\\}$"))
  expect_false(knitrmini:::group_pattern(NA_character_))
  expect_false(knitrmini:::group_pattern(""))
  expect_false(knitrmini:::group_pattern(character(0)))
})

test_that("knit_patterns can be restored", {
  old <- knit_patterns$get()
  knit_patterns$set(list(chunk.begin = "CUSTOM"))
  expect_equal(knit_patterns$get("chunk.begin"), "CUSTOM")
  knit_patterns$restore(old)
  expect_equal(knit_patterns$get("chunk.begin"), all_patterns$rnw$chunk.begin)
})
