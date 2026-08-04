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
  expect_equal(num_format(), "0.001k")
  expect_equal(
    lab_kmb(c(NA, Inf, -Inf, 0, 1500)),
    c(NA, "Inf", "-Inf", "0", "1.5k")
  )
})

test_that("formatters reject invalid arguments clearly", {
  expect_error(num_format(suffix = "trillion"), "arg")
  expect_error(num_format(by = 0), "must not be zero")
  expect_error(lab_kmb("1000"), "must be numeric")
})

test_that("endpoint helpers share missing-value and singleton behavior", {
  singleton <- data.frame(x = 1, y = 2)
  missing_tail <- data.frame(x = 1:3, y = c(1, 2, NA))

  expect_equal(nrow(find_endpoints(singleton, x, y)), 1L)
  expect_equal(find_endpoints(missing_tail, x, y)$x, c(2L, 1L))

  layer <- geom_endpoint(missing_tail, x, y, color = badblue)
  expect_equal(layer$data$x, c(2L, 1L))
})

test_that("legacy output helpers select their first default aspect", {
  expect_equal(eval(formals(badger_publish)$aspect, baseenv())[[1L]], "1col")
  expect_equal(eval(formals(badger_finisher)$aspect, baseenv())[[1L]], "default")
})
