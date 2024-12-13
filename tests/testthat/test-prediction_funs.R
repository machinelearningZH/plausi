library(testthat)


# TESTS FOR predict_votes() ===========================================


# We only test the outcome of predict_multiple_votes() since this function
# is based on both train_prediction_model() and predict_single_vote(). If
# something fails somewhere, it will be finallynoticed in
# predict_multiple_votes(). To do this, we run the entire process using a
# couple of different completion states of vote_data. Error handling
# is tested in the basic functions.

testthat::test_that("predict_votes() returns the expected output", {

  # set seed for reproducibility of sample()
  set.seed(1879)

  # create predictions for different counting states and compare them to an expected outcome
  test_prediction_list <- lapply(
    1:6,
    function(index) {
      # create input file
      test_input <- vote_data
      replacement_sequence <- sample(nrow(test_input), seq(0, 100, 20)[index])
      test_input[c("Eidg1", "Kant1")] <- lapply(c("Eidg1", "Kant1"), function(col) replace(test_input[[col]], replacement_sequence, NA))

      # train model
      test_model <- train_prediction_model("Eidg1", test_input, to_exclude_vars = "Kant1", geovars = c("gemeinde", "v_gemwkid"))

      # predict multiple votes
      test_prediction <- predict_votes(c("Eidg1", "Kant1"), traindata = test_input, geovars = c("gemeinde", "v_gemwkid"))
    })

  # compare data to expected result
  expect_equal(test_prediction_list, readRDS(testthat::test_path("testdata", "expected_outcome_predict_votes.rds")))

})


# TESTS FOR train_prediction_model() ===========================================


testthat::test_that("train_prediction_model() returns warnings and errors", {

  testthat::expect_warning(
    train_prediction_model(
      "Eidg1",
      vote_data,
      to_exclude_vars = NULL, # throws a warning message for NAs in data (the columns are then excluded)
      geovars = c("gemeinde", "v_gemwkid")
    )
  )

  testthat::expect_error(
    train_prediction_model(
      "Eidg1",
      vote_data,
      method = "undefined", # throws an error for model not being in caret's built-in library
      to_exclude_vars = "Kant1",
      geovars = c("gemeinde", "v_gemwkid")
    )
  )

  testthat::expect_error(
    train_prediction_model(
      "undefined", # throws an error for variables that are not found in the data
      vote_data,
      method = "svmRadial",
      trControl = NULL,
      to_exclude_vars = c("Kant1"),
      geovars = c("gemeinde", "v_gemwkid")
    )
  )

})



# TESTS FOR predict_single_vote() ==============================================


testthat::test_that("predict_single_vote() throws an error if something other than a model of class train is inserted into function", {

  testthat::expect_error(
    predict_single_vote("undefind", vote_data)
  )

})


# TESTS FOR rmse() =============================================================


testthat::test_that("rmse() throws an error if args have not the same length", {

  testthat::expect_error(
    rmse(c(1, 1, 1, 2), c(2, 2, 4))
  )

})














#
#
#
#
#
# # TESTS FOR train_prediction_model()
#
#
# test_that("train_prediction_model() returns the correct outputs", {
#   input0 <- vote_data
#
#   # remove some values from the data and create multiple input data frames
#   set.seed(1879)
#
#   input_list <- lapply(
#     1:5,
#     function(index) {
#       name <- paste0("input", index)
#       data <- input0
#       data[c("Eidg1", "Kant1")] <- lapply(c("Eidg1", "Kant1"), function(col) replace(input0[[col]], sample(length(input0[[col]]), seq(10, 50, 10)[index]), NA))
#       setNames(list(data), name)
#     })
#
#   input_list <- c(list(input0 = input0), do.call(c, input_list))
#
#   # process the different data frames with train_prediction_model() and compare the outputs with the expected output
#   train_list <- lapply(input_list, function(input) {
#     train_prediction_model("Eidg1", input, to_exclude_vars = "Kant1", geovars = c("gemeinde", "v_gemwkid"))
#   })
#
#   # compare with stored data
#   expect_equal(train_list, readRDS(testthat::test_path("testdata", "expected_outcome_train_prediction_model.rds")))
#
# })
#
#
#
#
#
#
# prediction_list <- lapply(input_list, function(input) {
#   predict_single_vote(train_list[[1]], input)
# })

