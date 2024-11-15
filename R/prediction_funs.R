#' Run predictions for multiple votes
#'
#' This function can be used to predict the outcome of multiple votes based on a number of past vote results.
#' It uses the machine learning models available in the caret package.
#'
#' @inheritParams train_prediction_model
#' @inheritParams predict_single_vote
#' @param x Column names of the dependent variables.
#' @param exclude_votes If set to TRUE, the variables to be predicted will be excluded from each others models.
#' This makes sense on a vote Sunday due to differences in the counting processes. This means, that a lot of the
#' votes in the data can contain NAs and should therefore be excluded. Defaults to TRUE.
#'
#' @importFrom plausi train_prediction_model
#' @importFrom plausi predict_single_vote
#'
#' @return A data.frame.
#' @export
#'
#' @examples
#'
#' predict_votes(c("Eidg1","Kant1"), plausi::votedata)
#'

predict_votes <- function(
    x,
    traindata,
    testdata = traindata,
    method = "svmRadial",
    trControl = NULL,
    exclude_votes = TRUE, # Question whether to even make this changeable...
    geovars = c("gemeinde", "v_gemwkid"),
    training_prop = NA,
    ...
){

  # exclude the votes to be predicted from predicting other votes if exclude_votes is set to TRUE
  to_exclude_vars <- if (exclude_votes) x else NULL

  # apply train_prediction_model and predict_single_vote consecutively on each vote column in x
  output <- lapply(x, function(vote_column) {

    # train a model for this particular vote using the training data
    trained_model <- plausi::train_prediction_model(
      vote_column,
      traindata = traindata,
      method = method,
      trControl = trControl,
      to_exclude_vars = to_exclude_vars,
      geovars = geovars,
      training_prop = training_prop,
      ...
    )

    # run predictions for the trained model using the test data (or training data, if not provided)
    predicted_data <- plausi::predict_single_vote(
      trained_model,
      testdata = testdata
    )

    return(predicted_data)

  })

  # combine all prediction results into a single dataframe
  do.call(rbind, output)

}



#' Train model for prediction of one vote
#'
#' This function can be used to train the model for the prediction of one vote
#' based on a number of past vote results. It uses the machine learning models
#' available in the caret package.
#'
#' @inheritParams caret::train
#' @param x Column name of the dependent variable.
#' @param traindata Data used to train the model containing the dependent variable and the predictor columns.
#' @param to_exclude_vars Variables that should be excluded from the model. It makes sense to exclude other
#' votes from the current Sunday since these can contain a lot of NAs that negatively impact the quality of
#' the model (since all rows containing NAs are dropped from the training data).
#' @param geovars Variables containing labels and IDs of the spatial units.
#' @param training_prop Optional argument to define a share of observations to be randomly kept in the training
#' data. It generates a training dataset by excluding the inverse proportion from the training data.
#' @param ... Optional parameters that can be passed to the [caret::train()][caret::train] function.
#'
#' @importFrom stats na.omit
#' @importFrom stats as.formula
#' @importFrom stats predict
#' @importFrom caret trainControl
#' @importFrom caret train
#'
#' @return A train object.
#' @export
#'
#' @examples
#'
#' train_prediction_model("Eidg1", plausi::votedata, to_exclude_vars = "Kant1")
#'

train_prediction_model <- function(
    x,
    traindata,
    method = "svmRadial",
    trControl = NULL,
    to_exclude_vars = NULL,
    geovars = c("gemeinde", "v_gemwkid"),
    training_prop = NA,
    ...
){

  # stop if any of the variables are not in the data
  if(!all(c(x, to_exclude_vars, geovars) %in% names(votedata))) {
    stop("Not all of your variables are found in the data.")
  }

  # warn user if the data contains NAs in any of the columns that are not either the dependent variable or a column to exclude (data is excluded)
  if (any(is.na(traindata[, !names(traindata) %in% c(x, to_exclude_vars)]))) {
    warning(
      "Your training data contains NAs. NAs are only allowed in the dependent variable (x)
      or in columns that are excluded for the training of the model (to_exclude_vars).
      The model therefore excludes all rows in which there are NAs."
    )
  }

  # drop rows that conatin NAs
  traindata <- traindata[!is.na(traindata[[x]]), ]

  # drop the dependent variable from the variables to exclude from the model (especially relevant for the function predict_multiple_votes)
  if(!is.null(to_exclude_vars)) to_exclude_vars <- to_exclude_vars[!to_exclude_vars %in% x]

  # if training_prop is defined, build the training dataset accordingly
  if (!is.na(training_prop)){

    # set seed so that same sample can be reproduced in future
    set.seed(101)

    # selecting share of data as sample from total n rows of the data and overwrite the data not in the sample with NA
    sample <- sample.int(n = nrow(traindata), size = floor(training_prop * nrow(traindata)), replace = F)
    traindata[-sample, x] <- NA
  }

  # define the model formula
  form <- stats::as.formula(paste0(x, " ~ ."))

  # set default trControl if none specified
  if(is.null(trControl)) trControl <- caret::trainControl(method = "cv", number = 10)

  # if defined, exclude to_exclude_vars from training data
  if(!is.null(to_exclude_vars)) {
    traindata <- traindata[, !names(traindata) %in% to_exclude_vars]
  }

  # exclude geovars from training data
  traindata <- traindata[, !names(traindata) %in% geovars]

  # drop rows that conatin NAs
  traindata <- traindata[!rowSums(is.na(traindata)) > 0, ]

  # train the model
  model <- caret::train(
    form,
    data = traindata,
    method = method,
    trControl = trControl,
    ...
  )

  # store some stuff that we need for the prediction in the model as attributes
  attr(model, "dependent_var") <- x
  attr(model, "geovars") <- geovars
  attr(model, "to_exclude_vars") <- to_exclude_vars

  # return the model
  return(model)

}



#' Run prediction for one vote
#'
#' This function can be used to predict the outcome of one vote based on a trained model,
#' generated by using the [plausi::train_prediction_model()][plausi::train_prediction_model].
#'
#' @param model A trained model, generated using [plausi::train_prediction_model()][plausi::train_prediction_model].
#' @param testdata Dataset on which the prediction should be run. The data must contain all columns of the training
#' data of the model \code{model$trainingData}.
#'
#' @importFrom plausi train_prediction_model
#'
#' @return A data.frame.
#' @export
#'
#' @examples
#'
#' test_model <- train_prediction_model("Eidg1", plausi::votedata, to_exclude_vars = "Kant1")
#'
#' predict_single_vote(test_model, plausi::votedata)
#'

predict_single_vote <- function(model, testdata){

  # stop if model argument is not a model
  if (!any(class(model) == "train")) {
    stop("Your model argument is not of class train.")
  }

  # get information from the model attributes
  x <- attr(model, "dependent_var")
  geovars <- attr(model, "geovars")
  to_exclude_vars <- attr(model, "to_exclude_vars")

  # stop if the data contains NAs in any of the columns that are not either the dependent variable or a column to exclude
  if (any(is.na(testdata[, !names(testdata) %in% c(x, to_exclude_vars)]))) {
    stop(
      "Your test data contains NAs. NAs are only allowed in the dependent variable defined in the model
      or in columns that are excluded for the training of the model."
    )
  }

  # exclude to_exclude_vars from test data
  if(!is.null(to_exclude_vars)) {
    testdata <- testdata[, !names(testdata) %in% to_exclude_vars]
  }

  # stop if the not all variables from the model are in the test data
  if (
    !any(names(model$trainingData)[!names(model$trainingData) %in% ".outcome"] %in% names(testdata)) || # check for predictors
    !x %in% names(testdata) # check for the dependent variable
  ) {
    stop("You cannot use different columns in your test data. All columns in the training data of your model must be present in the test data.")
  }

  # predict results
  testdata$pred <- stats::predict(model, testdata)

  # build output with geovars and prediction results
  output <- testdata[, c(geovars, "pred")]
  output$real <- testdata[[x]]
  output$vorlage <- x
  return(output)

}



#' Calculate RMSE
#'
#' Calculate the Root Mean Square Error (RMSE). The RMSE is the standard deviation of the residuals (prediction
#' errors) and therefore an indicator of how precise the prediction of a specific vote actually is.
#'
#' @param prediction Predicted value.
#' @param observation Oserved value.
#' @param na.rm Remove NA values, defaults to TRUE
#'
#' @return A vector of numeric values.
#' @export
#'
#' @examples
#'
#' pred_data <- predict_votes(c("Eidg1", "Kant1"), plausi::votedata, exclude_votes = TRUE)
#'
#' pred_data$rmse <- plausi::rmse(pred_data$pred, pred_data$real)
#'

rmse = function(prediction, observation, na.rm = TRUE){

  if (length(prediction) != length(observation)) {
    stop("The vectors prediction and observation must have the same length().")
  }

  sqrt(mean((prediction - observation) ^ 2, na.rm = na.rm))

}

