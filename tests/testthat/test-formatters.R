test_that("formatters produce their documented labels", {
  expect_equal(
    num_format(0, 3e6, 1e6, suffix = "m", currency = "$"),
    c("$0m", "$1m", "$2m", "$3m")
  )
  expect_equal(
    percent_format(0, 0.1, 0.02),
    c("0%", "2%", "4%", "6%", "8%", "10%")
  )
  expect_equal(
    year_format(2020, 2024, 2),
    paste0("\u2019", c("20", "22", "24"))
  )
})

test_that("legacy output helpers select their first default aspect", {
  expect_equal(eval(formals(badger_publish)$aspect, baseenv())[[1L]], "1col")
  expect_equal(eval(formals(badger_finisher)$aspect, baseenv())[[1L]], "default")
})
