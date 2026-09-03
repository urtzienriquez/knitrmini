library(knitrmini)

test_that("kable produces basic LaTeX tabular", {
  res <- kable(head(iris, 2))
  expect_true(grepl("\\\\begin\\{tabular\\}", res))
  expect_true(grepl("\\\\end\\{tabular\\}", res))
  expect_true(grepl("Sepal.Length", res))
  expect_true(grepl("setosa", res))
})

test_that("kable with caption wraps in table environment", {
  res <- kable(head(iris, 2), caption = "Test caption", label = NA)
  expect_true(grepl("\\\\begin\\{table\\}", res))
  expect_true(grepl("\\\\end\\{table\\}", res))
  expect_true(grepl("\\\\caption\\{Test caption\\}", res))
  expect_true(grepl("\\\\centering", res))
})

test_that("kable with booktabs", {
  res <- kable(mtcars[1:2, 1:3], booktabs = TRUE, label = NA)
  expect_true(grepl("\\\\toprule", res))
  expect_true(grepl("\\\\midrule", res))
  expect_true(grepl("\\\\bottomrule", res))
  expect_false(grepl("\\\\hline", res))
})

test_that("kable with longtable", {
  res <- kable(mtcars[1:2, 1:3], longtable = TRUE)
  expect_true(grepl("\\\\begin\\{longtable\\}", res))
  expect_false(grepl("\\\\begin\\{table\\}", res))
  expect_false(grepl("\\\\end\\{table\\}", res))
})

test_that("kable with row.names = FALSE", {
  res <- kable(mtcars[1:2, 1:3], row.names = FALSE)
  expect_false(grepl("Mazda", res))
  expect_true(grepl("mpg", res))
})

test_that("kable with custom col.names", {
  res <- kable(mtcars[1:2, 1:3], col.names = c("A", "B", "C"))
  expect_true(grepl("A & B & C", res, fixed = TRUE))
})

test_that("kable with digits", {
  df <- data.frame(a = 1.2345, b = 2.6789)
  res <- kable(df, digits = c(2, 1), row.names = FALSE)
  expect_true(grepl("1\\.23", res))
  expect_true(grepl("2\\.7", res))
})

test_that("kable with alignment string", {
  res <- kable(mtcars[1:2, 1:3], align = "clr")
  expect_true(grepl("\\{l\\|c\\|l\\|r\\}", res))
})

test_that("kable aligns numeric columns right, others left", {
  res <- kable(mtcars[1:2, 1:3])
  expect_true(grepl("\\{l\\|r\\|r\\|r\\}", res))
})

test_that("kable with no caption = no table env", {
  res <- kable(mtcars[1:2, 1:3])
  expect_false(grepl("\\\\begin\\{table\\}", res))
})

test_that("kable returns knitr_kable object", {
  res <- kable(mtcars[1:2, 1:3])
  expect_s3_class(res, "knitr_kable")
  expect_equal(attr(res, "format"), "latex")
})

test_that("kable with empty data frame", {
  res <- kable(data.frame())
  expect_true(grepl("\\\\begin\\{tabular\\}", res))
  expect_true(grepl("\\\\end\\{tabular\\}", res))
})

test_that("kable with varying digit lengths per column", {
  df <- data.frame(a = 1.2345, b = 2.6789, c = 3.1111)
  res <- kable(df, digits = c(1, 2, 3))
  expect_true(grepl("1\\.2", res))
  expect_true(grepl("2\\.68", res))
  expect_true(grepl("3\\.111", res))
})

test_that("kable with format.args", {
  df <- data.frame(x = 1000.5, y = 2000.5)
  res <- kable(df, format.args = list(big.mark = ",", decimal.mark = "."))
  expect_true(grepl("1,000.5", res, fixed = TRUE))
  expect_true(grepl("2,000.5", res, fixed = TRUE))
})

test_that("kable creates label when label is provided", {
  res <- kable(mtcars[1:2, 1:3], caption = "Labelled", label = "mytab")
  expect_true(grepl("\\\\label\\{tab:mytab\\}", res))
  expect_true(grepl("Labelled", res, fixed = TRUE))
})

test_that("kable without caption produces no caption LaTeX", {
  res <- kable(mtcars[1:2, 1:3])
  expect_false(grepl("\\\\caption", res))
})

test_that("kable with explicit label", {
  res <- kable(mtcars[1:2, 1:3], caption = "Labelled", label = "testtab")
  expect_true(grepl("\\\\label\\{tab:testtab\\}", res))
})
