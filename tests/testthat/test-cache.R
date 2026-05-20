test_that("new_cache save/load roundtrip", {
  c <- knitrmini:::new_cache()
  hash <- file.path(tempdir(), "testcache")
  c$save(list(x = 1), "output text", hash)
  on.exit(unlink(paste0(hash, c("_objects.rds", "_output.rds"))))

  expect_true(c$exists(hash))
  data <- c$load(hash)
  expect_equal(data$objects, list(x = 1))
  expect_equal(data$output, "output text")
})

test_that("new_cache exists returns FALSE for missing hash", {
  c <- knitrmini:::new_cache()
  expect_false(c$exists("nonexistent/hash"))
})

test_that("new_cache load returns NULL for missing hash", {
  c <- knitrmini:::new_cache()
  expect_null(c$load("nonexistent/hash"))
})

test_that("new_cache purge removes files", {
  c <- knitrmini:::new_cache()
  hash <- file.path(tempdir(), "purgetest")
  c$save(list(), "", hash)
  expect_true(c$exists(hash))
  c$purge(hash)
  expect_false(c$exists(hash))
})

test_that("new_cache output returns saved output", {
  c <- knitrmini:::new_cache()
  hash <- file.path(tempdir(), "outputtest")
  c$save(list(), "expected output", hash)
  on.exit(unlink(paste0(hash, c("_objects.rds", "_output.rds"))))
  expect_equal(c$output(hash), "expected output")
})

test_that("cache_content includes expected fields", {
  params <- list(
    code = "1+1", eval = TRUE, warning = TRUE, message = TRUE,
    error = TRUE, cache = TRUE, fig.keep = "high",
    fig.path = "figure/", dev = "pdf", dpi = 72,
    fig.width = 7, fig.height = 7
  )
  content <- knitrmini:::cache_content(params)
  expect_equal(content$code, "1+1")
  expect_true(content$eval)
})

test_that("cache_name produces deterministic hash", {
  params <- list(
    code = "1+1", cache.path = "cache/", label = "testchunk",
    cache.comments = TRUE, engine = "R"
  )
  h1 <- knitrmini:::cache_name(params)
  h2 <- knitrmini:::cache_name(params)
  expect_equal(h1, h2)
})

test_that("knit_global returns globalenv when not set", {
  expect_equal(knitrmini:::knit_global(), globalenv())
})

test_that("knit_global returns custom env after knit_global_set", {
  .knitEnv <- knitrmini:::.knitEnv
  env <- new.env()
  old <- .knitEnv$.knitGlobal
  on.exit(rm(list = ".knitGlobal", envir = .knitEnv))
  knitrmini:::knit_global_set(env)
  expect_equal(knitrmini:::knit_global(), env)
})
