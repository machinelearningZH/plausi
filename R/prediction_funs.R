#' Run prediction for one vote
#'
#' This function can be used to predict the outcome of one vote based on a number of past vote results.
#' It uses the machine learning models available in the caret package.
#'
#' @inheritParams caret::train
#' @param x Column name of the dependent variable.
#' @param traindata Data used to train the model containing the dependent variable and the predictor columns.
#' @param testdata Optional dataset structured identically as the training dataset on which the prediction
#' should be run. Defaults to NULL, which entails that the prediction is run on the training dataset.
#' @param to_exclude_vars Variables that should be excluded from the model.
#' @param geovars Variables containing labels and IDs of the spatial units.
#' @param testprop Optional argument to generate a training dataset by splitting the dataset (testprop = share of observations to be randomly kept).
#' @param ... Optional parameters that can be passed to the [caret::train()][caret::train] function.
#'
#' @importFrom stats na.omit
#' @importFrom stats as.formula
#' @importFrom stats predict
#' @importFrom caret trainControl
#' @importFrom caret train
#'
#' @return data.frame
#' @export
#'
#' @examples
#'
#' predict_single_vote("Eidg1", votedata, to_exclude_vars = "Kant1")
#'

predict_single_vote <- function(
    x,
    traindata,
    testdata = NULL,
    method = "svmRadial",
    trControl = NULL,
    to_exclude_vars = NULL,
    geovars = c("gemeinde", "v_gemwkid"),
    testprop = NA,
    ...
  ){


  # message if both testprop and testdata are defined since testprop is only needed if no testdata is provided
  if(!is.na(testprop) && !is.null(testdata)) {
    message(
      "By setting a testprop, the traindata is split into randomly generated training data.
      There is thus no need to supply a real test dataset via testdata argument."
    )
  }

  # set testdata to traindata if no specific dataset is defined
  if(is.null(testdata)) testdata <- traindata

  # if testprop is defined, build the training dataset accordingly
  if (!is.na(testprop)){

    # set seed so that same sample can be reproduced in future
    set.seed(101)

    # selecting share of data as sample from total n rows of the data and overwrite the data not in the sample with NA
    sample <- sample.int(n = nrow(traindata), size = floor(testprop * nrow(traindata)), replace = F)
    traindata[-sample, x] <- NA
  }

  # drop rows that conatin NAs
  traindata <- stats::na.omit(traindata)

  # drop dependent variable from to_exclude_vars
  if(!is.null(to_exclude_vars)) to_exclude_vars <- to_exclude_vars[!to_exclude_vars %in% x]

  # define the model formula
  form <- stats::as.formula(paste0(x, " ~ ."))

  # set default trControl if none specified
  if(is.null(trControl)) trControl <- caret::trainControl(method = "cv", number = 10)

  # exclude to_exclude_vars from training and testing data
  if(!is.null(to_exclude_vars)) {

    traindata <- traindata[, !names(traindata) %in% to_exclude_vars]

    testdata <- testdata[, !names(testdata) %in% to_exclude_vars] # this used to be conditional (!is.null(testdata)) but as far as I can see, this always holds true at this point
  }

  # train the model
  model <- caret::train(
    form,
    data = traindata[, !names(traindata) %in% c(to_exclude_vars, geovars)],
    method = method,
    trControl = trControl,
    ...
  )

  # predict results
  testdata$pred <- stats::predict(model, testdata)

  # build output with geovars and prediction results
  output <- testdata[, c(geovars, "pred")]
  testdata$real <- testdata
  output$real <- testdata[[x]]
  output$vorlage <- x
  # return(output)

}



#' Run predictions for multiple votes
#'
#' This function can be used to predict the outcome of multiple votes based on a number of past vote results.
#' It uses the machine learning models available in the caret package.
#'
#' @inheritParams predict_single_vote
#' @param x Column names of the dependent variables.
#' @param exclude_votes If set to TRUE, the variables to be predicted will be excluded from each others models.
#' This can make sense on a vote Sunday due to differences in the counting processes.
#'
#' @importFrom plausi predict_single_vote
#'
#' @return data.frame
#' @export
#'
#' @examples
#'
#' predict_votes(c("Eidg1","Kant1"), votedata, exclude_votes=TRUE)

predict_votes_neu <- function(
    x,
    traindata,
    testdata = NULL,
    method = "svmRadial",
    trControl = NULL,
    exclude_votes = FALSE,
    geovars = c("gemeinde","v_gemwkid"),
    testprop = NA,
    ...
  ){

  # exclude the votes to be predicted from predicting other votes if exclude_votes is set to TRUE
  to_exclude_vars <- if (exclude_votes) x else NULL

  # predict across all votes
  results <- lapply(
    x,
    predict_single_vote,
    traindata = traindata,
    testdata = testdata,
    method = method,
    trControl = trControl,
    to_exclude_vars = to_exclude_vars,
    geovars = geovars,
    testprop = testprop,
    ...
  )

  do.call(rbind, results)


}





















































#'
#'
#'
#'
#'
#' #' Train a model for a specific vote
#' #'
#' #' @param x column name of the dependent variable
#' #' @param traindata data used to train the model containing the dependent variable and the predictor-columns
#' #' @param testdata optional dataset structured identically as the trainingdataset on which the prediction should be run. Defaults to NULL, which entails that the prediction is run on the trainingdataset.
#' #' @param method method available in the caret-package which should be used for the prediction
#' #' @param trControl parameters to tune the model
#' #' @param to_exclude_vars variables that should be excluded from the model
#' #' @param geovars variables containing labels and ids of the spatial units
#' #' @param testprop optional argument to generate a training dataset by splitting the dataset (testprop=share of observations to be randomly kept)
#' #' @param ... optional parameters that can be passed to the caret::train function
#' #'
#' #' @importFrom tidyr drop_na
#' #' @importFrom stats as.formula
#' #' @importFrom stats predict
#' #' @importFrom dplyr "%>%"
#' #' @importFrom dplyr select
#' #' @importFrom dplyr mutate
#' #' @importFrom caret trainControl
#' #' @importFrom caret train
#' #' @importFrom purrr map_dfr
#' #'
#' #' @return data.frame
#' #' @export
#' #'
#' #' @examples
#' #'
#' #' predict_single_vote("Eidg1",votedata, to_exclude_vars = "Kant1")
#' #'
#'
#' predict_single_vote <- function(x,traindata,testdata=NULL,method="svmRadial",trControl=NULL,to_exclude_vars=NULL,geovars=c("gemeinde","v_gemwkid"),testprop=NA,...){
#'
#'   if(is.null(testdata)) testdata <- traindata
#'
#'   # Um Trainingsdaten aus tatsächlichen Daten zu simulieren (Trainingsdatensatz wird anhand von 'testprop' generiert)
#'   if (!is.na(testprop)){
#'
#'     if(!is.na(testdata)) message("By setting a testprop the traindata is split into randomly generated training data. There is thus no need to supply a real testdata-set via testdata argument.")
#'
#'     set.seed(101) # Set Seed so that same sample can be reproduced in future also
#'     # Now Selecting 75% of data as sample from total 'n' rows of the data
#'     # sample <- sample.int(n = nrow(preddataframe), size = floor(.75*nrow(preddataframe)), replace = F)
#'
#'     sample <- sample.int(n = nrow(traindata), size = floor(testprop*nrow(traindata)), replace = F)
#'
#'     traindata[-sample, ][[x]] <- NA
#'
#'
#'   }
#'
#'   # schliesse Beobachtungen aus Trainingsdatensatz aus, die NAs enthalten
#'   traindata <- traindata %>% tidyr::drop_na(x)
#'
#'   # Schliesse die zuvorhersagenden Abstimmungen gegenseitig aus den modellen aus, wenn to_exclude_vars übergeben werden
#'   if(!is.null(to_exclude_vars)) to_exclude_vars<-  to_exclude_vars[!to_exclude_vars %in% x]
#'
#'   # varname <-  as.name(x)
#'   form <- stats::as.formula(paste(x,'~.'))
#'
#'   if(is.null(trControl)) trControl <- caret::trainControl(method = "cv", number = 10)
#'
#'   # stelle sicher, dass Vektor aller Vorlagen die augeschlossen werden sollen (z.B. Vorlagen vom selben Abstimmungssonntag), nicht die zu vorhersagende Vorlage enthält
#'   if(!is.null(to_exclude_vars)) traindata <- traindata[, !names(traindata) %in% to_exclude_vars]
#'   if(!is.null(to_exclude_vars)&!is.null(testdata)) testdata <- testdata[, !names(testdata) %in% to_exclude_vars]
#'
#'   # Um zu prüfen, ob gegenseitiger Ausschluss von Vorlagen desselben Abstimmungssonntags funktioniert ->
#'   # print(colnames(traindata))
#'
#'   # Trainiere Model
#'   cv_model_mars <- caret::train(
#'     form,
#'     data = traindata %>% dplyr::select(!tidyselect::all_of(geovars)),
#'     method = method,
#'     trControl = trControl,
#'     ...
#'   )
#'
#'   # lastmod <<-cv_model_mars
#'
#' # cv_model_mars$results
#'
#'
#'   testdata$pred <- stats::predict(cv_model_mars,testdata)
#'
#'   # TO DO :
#'   # Gebietslabel / ID nicht hart vorgeben, sondern via parameter der Funktion übernehmen
#'   # Objekt mit modell und Daten als Output
#'   testdata %>%
#'     select(tidyselect::all_of(geovars), pred, real=x) %>%
#'     mutate(vorlage=x)
#'
#' }
#'
#'
#' Run predictions for multiple columns (specifically votes) in a dataset
#'
#' @param votes names of the dependent variable-columns
#' @param train data used to train the model containing the variables to be predicted and the predictor-columns
#' @param test  optional dataset structured identically as the trainingdataset on which the prediction
#' @param method method available in the caret-package which should be used for the prediction
#' @param trControl parameters to tune the model
#' @param exclude_votes if TRUE the variables to be predicted will be excluded from each others models
#' @param geovars variables containing labels and ids of the spatial units
#' @param  testprop optional argument to generate a training dataset by splitting the dataset (testprop=share of observations to be randomly kept)
#' @param ... optional parameters that can be passed to the caret::train function
#'
#' @return data.frame
#' @export
#'
#' @examples
#'
#' predict_votes(c("Eidg1","Kant1"), votedata, exclude_votes=TRUE)

predict_votes <- function(votes,train,test=NULL,method="svmRadial",trControl=NULL,exclude_votes=FALSE,geovars=c("gemeinde","v_gemwkid"),testprop=NA,...){

  # Schliesse die zuvorhersagenden Abstimmungen gegenseitig aus den modellen aus, wenn exclude_votes = TRUE gesetzt wird (bei mehreren Abstimmungen am selben Datum aufgrund unterschiedlichen Auszählstände sinnvoll)
  if(exclude_votes==TRUE) { to_exclude_vars <- votes} else { to_exclude_vars <- NULL }

  # Iteriere über die vorherzusagenden Vorlagen
  purrr::map_dfr(votes, plausi::predict_single_vote,
                                                     traindata=train,
                                                     testdata=test,
                                                     method=method,
                                                     trControl=trControl,
                                                     to_exclude_vars=to_exclude_vars,
                                                     geovars=geovars,
                 ...)

}
#'
#'
#' #' Calculate RMSE
#' #'
#' #' Root Mean Square Error (RMSE) = standard deviation of the residuals (prediction errors).
#' #'
#' #' @param m predicted value (fitted by modelling)
#' #' @param o oserved 'true' value
#' #' @param na.rm remove NA values, defaults to TRUE
#' #'
#' #' @return numeric value
#' #' @export
#' #'
#' #' @examples
#' #' library(dplyr)
#' #' library(tidyr)
#' #'
#' #' pred_data  <- predict_votes(c("Eidg1","Kant1"), votedata, exclude_votes=TRUE)
#' #'
#' #' pred_data %>%
#' #' drop_na() %>%
#' #' group_by(vorlage) %>%
#' #' summarize(rmse=RMSE(pred,real))
#' #'
#'
#' RMSE = function(m, o, na.rm=TRUE){
#'   sqrt(mean((m - o)^2,na.rm=na.rm))
#' }
