library(testthat)


# TESTS FOR cross_fun() ========================================================


test_that("cross_fun() returns the expected table", {
  output <- cross_fun(votedata[, 1:4], "Eidg1", "Kant1", geo_cols = c("gemeinde", "v_gemwkid"))

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
  expect_error(cross_fun(votedata[, 1:4], "Eidg1", "Kant1", "non-existent-column"))
})


# TESTS FOR get_differences() ==================================================













































