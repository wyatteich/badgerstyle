test_that("compact labels trim zero decimals and use lowercase suffixes", {
  f <- label_number_trim()
  expect_equal(f(c(0, 60, 1e3, 1200, 6e7, 2e9, 3e12, -1200, NA_real_)),
    c("0", "60", "1k", "1.2k", "60m", "2b", "3t", "-1.2k", NA_character_))
  expect_equal(f(numeric()), character())
  expect_equal(label_number_trim(accuracy = .01)(c(1e6, 1.2e6, 1.23e6)),
    c("1m", "1.20m", "1.23m"))
})

test_that("decimal marks, affixes, and caller scale choices are preserved", {
  expect_equal(label_number_trim(decimal.mark = ",", prefix = "$")(c(6e7, 1.2e6)),
    c("$60m", "$1,2m"))
  expect_equal(label_number_trim(decimal.mark = "|")(c(1e6, 1.2e6)),
    c("1m", "1|2m"))
  expect_equal(label_number_trim(scale_cut = c(0, K = 1e3))(c(1000, 1200)),
    c("1K", "1.2K"))
  expect_equal(label_number_trim(scale_cut = stats::setNames(0, ""), scale = 1e-6, suffix = "m")(c(0, 6e7, 1.2e6)),
    c("0m", "60m", "1.2m"))
  expect_equal(label_number_trim(scale_cut = stats::setNames(0, ""), suffix = "%")(c(10, 12.5)),
    c("10%", "12.5%"))
  expect_equal(label_number_trim(scale_cut = stats::setNames(0, ""), accuracy = 1, big.mark = ",")(60000), "60,000")
})
