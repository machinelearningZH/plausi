library(testthat)


# TESTS FOR train_prediction_model(), predict_single_vote() AND predict_multiple_votes()


# We de facto only test the outcome of predict_multiple_votes() since this
# function includes the other two. If something fails somewhere, it will
# be finally noticed in predict_multiple_votes(). To do this, we run the
# entire process using a couple of different completion states of votedata.

test_that("predict_multiple_votes() returns the expected output", {

  # create different counting
  test_prediction_list <- lapply(
    1:6,
    function(index) {
      # create input file
      test_input <- votedata
      replacement_sequence <- sample(nrow(test_input), seq(0, 100, 20)[index])
      test_input[c("Eidg1", "Kant1")] <- lapply(c("Eidg1", "Kant1"), function(col) replace(input0[[col]], replacement_sequence, NA))

      # train model
      test_model <- train_prediction_model("Eidg1", test_input, to_exclude_vars = "Kant1", geovars = c("gemeinde", "v_gemwkid"))

      # predict multiple votes
      test_prediction <- predict_multiple_votes(c("Eidg1", "Kant1"), traindata = test_input, geovars = c("gemeinde", "v_gemwkid"))
    })

  # compare data to expected result
  expect_equal(test_prediction_list, readRDS(testthat::test_path("testdata", "expected_outcome_predict_multiple_votes.rds")))

})


test_that("errors", {



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
#   input0 <- votedata
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

