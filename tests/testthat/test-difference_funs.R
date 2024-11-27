library(testthat)


# TESTS FOR cross_fun() ========================================================


test_that("cross_fun() returns the expected table", {
  output <- cross_fun(vote_data[, 1:4], "Eidg1", "Kant1", geo_cols = c("gemeinde", "v_gemwkid"))

  rds_filepath <- testthat::test_path("testdata", "expected_outcome_cross_fun.rds")

  if (file.exists(rds_filepath)) {
    expected_output <- readRDS(rds_filepath)

    # Check if the output matches the expected RDS content
    testthat::expect_equal(output, expected_output)

  } else {

    # return a warning message if the corresponding RDS file was not found
    testthat::fail("No corresponding RDS file found to test cross_fun().")

  }
})


test_that("cross_fun() returns errors", {
  # Check error for non-existing columns
  expect_error(cross_fun(vote_data[, 1:4], "Eidg1", "Kant1", "non-existent-column"))
})


# TESTS FOR get_differences() ==================================================


test_that("get_differences() returns the expected table", {
  input <- vote_data[, 1:5]

  combinations <- as.data.frame(t(combn(c(names(input[-(1:2)])), 2)))

  output <- get_differences(vote_data[, 1:5], combinations$V1, combinations$V2, geo_cols = c("gemeinde", "v_gemwkid"))

  rds_filepath <- testthat::test_path("testdata", "expected_outcome_get_differences.rds")

  if (file.exists(rds_filepath)) {
    expected_output <- readRDS(rds_filepath)

    # Check if the output matches the expected RDS content
    testthat::expect_equal(output, expected_output)

  } else {

    # return a warning message if the corresponding RDS file was not found
    testthat::fail("No corresponding RDS file found to test get_differences().")

  }
})


test_that("get_differences() returns error if combination vectors do not have the same length", {
  combinations <- as.data.frame(t(combn(c(names(vote_data[, 3:5])), 2)))

  expect_error(get_differences(vote_data[, 1:5], combinations$V1, combinations$V2[-1], geo_cols = c("gemeinde", "v_gemwkid")))
})


