test_that("group_indices marks chunks correctly", {
  begin <- c(FALSE, TRUE, FALSE, FALSE, FALSE, TRUE, FALSE, FALSE)
  end   <- c(FALSE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, TRUE)
  idx <- knitrmini:::group_indices(begin, end)
  expect_equal(idx, c(0L, 1L, 1L, 1L, 2L, 3L, 3L, 3L))
})

test_that("group_indices handles no chunks", {
  begin <- rep(FALSE, 5)
  end <- rep(FALSE, 5)
  idx <- knitrmini:::group_indices(begin, end)
  expect_equal(idx, rep(0L, 5))
})

test_that("group_indices handles consecutive chunks", {
  begin <- c(TRUE, FALSE, TRUE, FALSE)
  end   <- c(FALSE, TRUE, FALSE, TRUE)
  idx <- knitrmini:::group_indices(begin, end)
  expect_equal(idx, c(1L, 1L, 3L, 3L))
})

test_that("divide_chunks splits into groups", {
  lines <- c("before", "<<chunk>>=", "1+1", "@", "middle", "<<chunk2>>=", "2+2", "@", "after")
  pat <- all_patterns$rnw
  groups <- knitrmini:::divide_chunks(lines, pat$chunk.begin, pat$chunk.end)
  expect_length(groups, 5)
  expect_equal(groups[[1]], "before")
  expect_equal(groups[[2]][1], "<<chunk>>=")
  expect_equal(groups[[3]], "middle")
})

test_that("strip_block removes prefix from code lines", {
  x <- c("<<chunk>>=", "  1+1", "  2+2")
  result <- knitrmini:::strip_block(x, "^ *")
  expect_equal(result[1], "<<chunk>>=")
  expect_true(all(nchar(result[-1]) > 0))
})

test_that("parse_block extracts label and creates block object", {
  block <- knitrmini:::parse_block("1+1", "label = mychunk", "label = mychunk")
  expect_s3_class(block, "block")
  expect_equal(block$params$label, "mychunk")
})

test_that("parse_block assigns auto-label when missing", {
  knitrmini:::chunk_counter(reset = TRUE)
  block <- knitrmini:::parse_block("1+1", "", "")
  expect_true(grepl("unnamed-chunk", block$params$label))
})

test_that("parse_block handles dependson", {
  knitrmini:::dep_list$restore()
  block <- knitrmini:::parse_block("1+1", "dependson = otherchunk", "dependson = otherchunk")
  expect_equal(knitrmini:::dep_list$get("otherchunk"), block$params$label)
})

test_that("parse_inline extracts Sexpr code", {
  result <- knitrmini:::parse_inline("text \\Sexpr{1+1} more", all_patterns$rnw)
  expect_s3_class(result, "inline")
  expect_equal(result$code, "1+1")
})

test_that("parse_inline returns empty code when no inline expression", {
  result <- knitrmini:::parse_inline("plain text no inline code", all_patterns$rnw)
  expect_equal(result$code, character(0))
})

test_that("parse_inline handles inline comments", {
  patterns <- all_patterns$rnw
  patterns$inline.comment <- "^\\s*%.*"
  result <- knitrmini:::parse_inline("% \\Sexpr{1+1}", patterns)
  expect_s3_class(result, "inline")
})

test_that("split_file returns mix of blocks and inline", {
  lines <- c("before", "<<a>>=", "1+1", "@", "after")
  groups <- knitrmini:::split_file(lines, patterns = all_patterns$rnw)
  expect_length(groups, 3)
  expect_s3_class(groups[[2]], "block")
})

test_that("unnamed_chunk uses default prefix", {
  knitrmini:::chunk_counter(reset = TRUE)
  expect_equal(knitrmini:::unnamed_chunk(), "unnamed-chunk-1")
  expect_equal(knitrmini:::unnamed_chunk(), "unnamed-chunk-2")
})

test_that("chunk_counter resets", {
  knitrmini:::chunk_counter(reset = TRUE)
  expect_equal(knitrmini:::chunk_counter(), 1L)
  expect_equal(knitrmini:::chunk_counter(), 2L)
  knitrmini:::chunk_counter(reset = TRUE)
  expect_equal(knitrmini:::chunk_counter(), 1L)
})

test_that("plot_counter resets", {
  knitrmini:::plot_counter(reset = TRUE)
  expect_equal(knitrmini:::plot_counter(), 1L)
  knitrmini:::plot_counter(reset = TRUE)
  expect_equal(knitrmini:::plot_counter(), 1L)
})

test_that("str_locate finds pattern positions", {
  loc <- knitrmini:::str_locate("abc abc", "a")
  expect_true(is.matrix(loc[[1]]))
  expect_equal(unname(loc[[1]][1, "start"]), 1)
})

test_that("str_locate returns empty for no match", {
  loc <- knitrmini:::str_locate("abc", "z")
  expect_true(is.matrix(loc[[1]]))
  expect_equal(nrow(loc[[1]]), 0)
})

test_that("str_replace replaces at given positions", {
  loc <- matrix(c(7, 11), nrow = 1, dimnames = list(NULL, c("start", "end")))
  result <- knitrmini:::str_replace("hello world", loc, "there")
  expect_equal(result, "hello there")
})
