library(testthat)


# TESTS FOR double_mad() =======================================================


test_that("double_mad() returns correct MADs for typical asymmetric distribution", {
  x <- c(1, 2, 3, 3, 4, 4, 4, 5, 5.5, 6, 6, 6.5, 7, 7, 7.5, 8, 9, 12, 52, 90)
  result <- double_mad(x)

  expect_length(result, 2) # check that output length is 2
  expect_true(is.numeric(result)) # check that output is numeric
  expect_equal(result[1], 2) # check left value
  expect_equal(result[2], 1.5) # check right value
})


test_that("double_mad() handles empty input gracefully", {
  x <- numeric(0)
  result <- double_mad(x)

  expect_equal(result, c(NA_real_, NA_real_)) # both MADs should be NA
})


test_that("double_mad() handles NA values in input vector", {
  x <- c(1, 2, NA, 3, 4, NA, 5)
  result <- double_mad(x)

  expect_length(result, 2)
  expect_true(!any(is.na(result))) # ensure NA values are removed before calculating MAD
})


test_that("double_mad() throws error for non-numeric input", {
  expect_error(double_mad("a"), "Your input must be numeric.")
  expect_error(double_mad(list(1, 2, 3)), "Your input must be numeric.")
})


test_that("double_mad() handles zero MAD with different zero_mad_action options", {
  x <- rep(5, 10) # input with constant values will result in MAD = 0

  expect_equal(double_mad(x), c(0, 0)) # default behavior (no warning or stop)
  expect_warning(double_mad(x, zero_mad_action = "warn"), "MAD is 0") # warning option
  expect_error(double_mad(x, zero_mad_action = "stop"), "MAD is 0") # stop option
})


test_that("double_mad() works for symmetric distribution", {
  x <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
  result <- double_mad(x)

  expect_equal(result[1], result[2]) # left and right MADs should be equal
})


test_that("double_mad() returns reasonable MADs for small asymmetric dataset", {
  x <- c(1, 1, 2, 3) # small asymmetric dataset (does not per se return this outcome, could also return a 0/0 outcome (e. g. with c(1, 1, 1, 3)))
  result <- double_mad(x)

  expect_length(result, 2)
  expect_true(is.numeric(result))
  expect_gt(result[1], 0)
  expect_gt(result[2], 0)
  expect_true(result[1] != result[2]) # left and right MADs should differ for asymmetry
})


# TEST FOR double_mad_from_median() ============================================


test_that("double_mad_from_median() calculates correct MAD distances", {
  x <- c(1, 2, 3, 5, 8, 10, 12, 15, 16)
  result <- double_mad_from_median(x)

  expect_length(result, length(x))
  expect_true(is.numeric(result))
})


test_that("double_mad_from_median() handles symmetric dataset with equal distances", {
  x <- c(2, 4, 6, 8, 10)
  result <- double_mad_from_median(x)

  expect_true(all(result[x <= 6] == rev(result[x >= 6]))) # left side MADs is equal to reversed right side MADs
})


test_that("double_mad_from_median() returns 0 for identical values", {
  x <- c(5, 5, 5, 5)
  result <- double_mad_from_median(x)

  expect_equal(result, rep(0, length(x)))
})


test_that("double_mad_from_median() errors on non-numeric input", {
  x <- c("a", "b", "c")

  expect_error(double_mad_from_median(x), "must be numeric")
})


# TESTS FOR rmse() =============================================================


testthat::test_that("rmse() throws an error if args have not the same length", {

  testthat::expect_error(
    rmse(c(1, 1, 1, 2), c(2, 2, 4))
  )

})


# TESTS FOR rmse_cutoff() ======================================================


testthat::test_that("rmse_cutoff() throws an error for too big absolute cutoff", {
  testthat::expect_error(
    rmse_cutoff(c(1, 1, 1, 2), c(1, 2, 2, 4), cutoff_min = 4, cutoff_perc = 10)
  )

  testthat::expect_error(
    rmse_cutoff(c(1, 1, 1, 2), c(1, 2, 2, 4), cutoff_min = 2, cutoff_perc = 105)
  )
})


# TESTS FOR is_outlier_double_mad() ============================================


test_that("is_outlier_double_mad() detects outliers based on MAD distances", {
  x <- c(1, 2, 3, 5, 8, 10, 12, 50)
  result <- is_outlier_double_mad(x)

  expect_length(result, length(x))
  expect_true(is.logical(result))
  expect_true(result[8]) # Extreme value
  expect_false(any(result[1:7]))
})


test_that("is_outlier_double_mad() respects threshold parameter", {
  x <- c(1, 2, 3, 5, 8, 10, 12, 50)

  expect_false(any(is_outlier_double_mad(x, threshold= 10))) # higher threshold, no outliers
})


test_that("is_outlier_double_mad() handles identical values correctly", {
  x <- rep(5, 10)
  result <- is_outlier_double_mad(x)

  expect_equal(result, rep(FALSE, length(x)))
})


test_that("is_outlier_double_mad() errors on non-numeric input", {
  x <- c("a", "b", "c")

  expect_error(is_outlier_double_mad(x), "must be numeric")
})


# TESTS FOR outlier_range() ====================================================


test_that("outlier_range() calculates correct bounds", {
  x <- c(1, 2, 3, 5, 8, 10, 12, 50)
  result <- outlier_range(x)

  expect_equal(names(result), c("median", "iqr", "lower", "upper", "label"))
  expect_true(is.numeric(result$lower) && is.numeric(result$upper))
  expect_true(result$lower == 0 && result$upper == 20.5)
})


test_that("outlier_range() respects percent bounds", {
  x <- c(1, 2, 3, 5, 8, 10, 12, 150, 1502, 1502, 1502, 1502, 1502, 1502, 1502, 1502, 15669)
  result <- outlier_range(x, percent = TRUE)

  expect_true(all(result$lower >= 0))
  expect_true(all(result$upper <= 100))
})


test_that("outlier_range() errors on non-numeric input", {
  x <- c("a", "b", "c")

  expect_error(outlier_range(x), "must be numeric")
})


# TESTS FOR is_outtlier_single_mad() ===========================================


test_that("is_outlier_single_mad() detects symmetric outliers", {
  x <- c(1, 2, 3, 4, 100)
  result <- is_outlier_single_mad(x)

  expect_length(result, length(x))
  expect_true(result[5]) # extreme value is outlier
  expect_false(any(result[1:4])) # all other values are not outliers
})


test_that("is_outlier_single_mad() adjusts outlier detection based on threshold", {
  x <- c(1, 2, 3, 4, 100)

  expect_false(any(is_outlier_single_mad(x, threshold= 80))) # high threshold
})


test_that("is_outlier_single_mad() handles NA values appropriately", {
  x <- c(1, 2, 3, NA, 100)
  result <- is_outlier_single_mad(x, na.rm = TRUE)

  expect_length(result, length(x))
  expect_true(is.na(result[4]))
  expect_true(all(!is.na(result[-4])))
})


# TESTS FOR is_outlier_z() =====================================================


test_that("is_outlier_z() detects outliers based on Z-score", {
  x <- c(1, 2, 3, 4, 5, 6, 7, 8, 8, 8, 9, 12, 250)
  result <- is_outlier_z(x)

  expect_length(result, length(x))
  expect_true(result[length(x)]) # extreme value is outlier
  expect_false(any(result[1:(length(x) - 1)]))
})


test_that("is_outlier_z() adjusts outlier detection based on threshold", {
  x <- c(1, 2, 3, 4, 5, 6, 7, 8, 8, 8, 9, 12, 250)

  expect_false(any(is_outlier_z(x, threshold= 10))) # high threshold, no outliers
})


test_that("is_outlier_z() handles NA values appropriately", {
  x <- c(1, 2, 3, NA, 100)
  result <- is_outlier_z(x, na.rm = TRUE)

  expect_length(result, length(x))
  expect_true(is.na(result[4]))
  expect_true(all(!is.na(result[-4])))
})


# TESTS FOR is_outlier_turkey() ================================================


test_that("is_outlier_turkey() detects Tukey outliers", {
  x <- c(1, 2, 3, 4, 20)
  result <- is_outlier_turkey(x)

  expect_length(result, length(x))
  expect_true(result[5]) # outlier
  expect_false(any(result[1:4]))
})


test_that("is_outlier_turkey() adjusts outlier detection based on k threshold", {
  x <- c(1, 2, 3, 4, 20)
  result <- is_outlier_turkey(x, threshold= 10) # higher threshold, no more

  expect_false(result[5])
})


test_that("is_outlier_turkey() handles identical values correctly", {
  x <- rep(5, 10)
  result <- is_outlier_turkey(x)

  expect_false(any(result)) # no outliers in identical data
})

