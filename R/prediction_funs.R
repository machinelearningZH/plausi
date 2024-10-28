#' Train a model for a specific vote
#'
#' @param x Column name of the dependent variable.
#' @param traindata Data used to train the model containing the dependent variable and the predictor columns.
#' @param testdata Optional dataset structured identically as the training dataset on which the prediction
#' should be run. Defaults to NULL, which entails that the prediction is run on the training dataset.
#' @param method Method available in the caret-package which should be used for the prediction
#' @param trControl Parameters to tune the model
#' @param to_exclude_vars Variables that should be excluded from the model
#' @param geovars Variables containing labels and ids of the spatial units
#' @param testprop Optional argument to generate a training dataset by splitting the dataset (testprop=share of observations to be randomly kept)
#' @param ... Optional parameters that can be passed to the caret::train function
#'
#' @importFrom tidyr drop_na
#' @importFrom stats as.formula
#' @importFrom stats predict
#' @importFrom dplyr "%>%"
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom caret trainControl
#' @importFrom caret train
#' @importFrom purrr map_dfr
#'
#' @return data.frame
#' @export
#'
#' @examples
#'
#' predict_single_vote("Eidg1",votedata, to_exclude_vars = "Kant1")
#'

predict_single_vote <- function(x,traindata,testdata=NULL,method="svmRadial",trControl=NULL,to_exclude_vars=NULL,geovars=c("gemeinde","v_gemwkid"),testprop=NA,...){

  if(is.null(testdata)) testdata <- traindata

  # Um Trainingsdaten aus tatsächlichen Daten zu simulieren (Trainingsdatensatz wird anhand von 'testprop' generiert)
  if (!is.na(testprop)){

    if(!is.na(testdata)) message("By setting a testprop the traindata is split into randomly generated training data. There is thus no need to supply a real testdata-set via testdata argument.")

    set.seed(101) # Set Seed so that same sample can be reproduced in future also
    # Now Selecting 75% of data as sample from total 'n' rows of the data
    # sample <- sample.int(n = nrow(preddataframe), size = floor(.75*nrow(preddataframe)), replace = F)

    sample <- sample.int(n = nrow(traindata), size = floor(testprop*nrow(traindata)), replace = F)

    traindata[-sample, ][[x]] <- NA


  }

  # schliesse Beobachtungen aus Trainingsdatensatz aus, die NAs enthalten
  traindata <- traindata %>% tidyr::drop_na(x)

  # Schliesse die zuvorhersagenden Abstimmungen gegenseitig aus den modellen aus, wenn to_exclude_vars übergeben werden
  if(!is.null(to_exclude_vars)) to_exclude_vars<-  to_exclude_vars[!to_exclude_vars %in% x]

  # varname <-  as.name(x)
  form <- stats::as.formula(paste(x,'~.'))

  if(is.null(trControl)) trControl <- caret::trainControl(method = "cv", number = 10)

  # stelle sicher, dass Vektor aller Vorlagen die augeschlossen werden sollen (z.B. Vorlagen vom selben Abstimmungssonntag), nicht die zu vorhersagende Vorlage enthält
  if(!is.null(to_exclude_vars)) traindata <- traindata[, !names(traindata) %in% to_exclude_vars]
  if(!is.null(to_exclude_vars)&!is.null(testdata)) testdata <- testdata[, !names(testdata) %in% to_exclude_vars]

  # Um zu prüfen, ob gegenseitiger Ausschluss von Vorlagen desselben Abstimmungssonntags funktioniert ->
  # print(colnames(traindata))

  # Trainiere Model
  cv_model_mars <- caret::train(
    form,
    data = traindata %>% dplyr::select(!tidyselect::all_of(geovars)),
    method = method,
    trControl = trControl,
    ...
  )

  # lastmod <<-cv_model_mars

  # cv_model_mars$results


  testdata$pred <- stats::predict(cv_model_mars,testdata)

  # TO DO :
  # Gebietslabel / ID nicht hart vorgeben, sondern via parameter der Funktion übernehmen
  # Objekt mit modell und Daten als Output
  testdata %>%
    select(tidyselect::all_of(geovars), pred, real=x) %>%
    mutate(vorlage=x)

}































#' Train a model for a specific vote
#'
#' @param x column name of the dependent variable
#' @param traindata data used to train the model containing the dependent variable and the predictor-columns
#' @param testdata optional dataset structured identically as the trainingdataset on which the prediction should be run. Defaults to NULL, which entails that the prediction is run on the trainingdataset.
#' @param method method available in the caret-package which should be used for the prediction
#' @param trControl parameters to tune the model
#' @param to_exclude_vars variables that should be excluded from the model
#' @param geovars variables containing labels and ids of the spatial units
#' @param testprop optional argument to generate a training dataset by splitting the dataset (testprop=share of observations to be randomly kept)
#' @param ... optional parameters that can be passed to the caret::train function
#'
#' @importFrom tidyr drop_na
#' @importFrom stats as.formula
#' @importFrom stats predict
#' @importFrom dplyr "%>%"
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom caret trainControl
#' @importFrom caret train
#' @importFrom purrr map_dfr
#'
#' @return data.frame
#' @export
#'
#' @examples
#'
#' predict_single_vote("Eidg1",votedata, to_exclude_vars = "Kant1")
#'

predict_single_vote <- function(x,traindata,testdata=NULL,method="svmRadial",trControl=NULL,to_exclude_vars=NULL,geovars=c("gemeinde","v_gemwkid"),testprop=NA,...){

  if(is.null(testdata)) testdata <- traindata

  # Um Trainingsdaten aus tatsächlichen Daten zu simulieren (Trainingsdatensatz wird anhand von 'testprop' generiert)
  if (!is.na(testprop)){

    if(!is.na(testdata)) message("By setting a testprop the traindata is split into randomly generated training data. There is thus no need to supply a real testdata-set via testdata argument.")

    set.seed(101) # Set Seed so that same sample can be reproduced in future also
    # Now Selecting 75% of data as sample from total 'n' rows of the data
    # sample <- sample.int(n = nrow(preddataframe), size = floor(.75*nrow(preddataframe)), replace = F)

    sample <- sample.int(n = nrow(traindata), size = floor(testprop*nrow(traindata)), replace = F)

    traindata[-sample, ][[x]] <- NA


  }

  # schliesse Beobachtungen aus Trainingsdatensatz aus, die NAs enthalten
  traindata <- traindata %>% tidyr::drop_na(x)

  # Schliesse die zuvorhersagenden Abstimmungen gegenseitig aus den modellen aus, wenn to_exclude_vars übergeben werden
  if(!is.null(to_exclude_vars)) to_exclude_vars<-  to_exclude_vars[!to_exclude_vars %in% x]

  # varname <-  as.name(x)
  form <- stats::as.formula(paste(x,'~.'))

  if(is.null(trControl)) trControl <- caret::trainControl(method = "cv", number = 10)

  # stelle sicher, dass Vektor aller Vorlagen die augeschlossen werden sollen (z.B. Vorlagen vom selben Abstimmungssonntag), nicht die zu vorhersagende Vorlage enthält
  if(!is.null(to_exclude_vars)) traindata <- traindata[, !names(traindata) %in% to_exclude_vars]
  if(!is.null(to_exclude_vars)&!is.null(testdata)) testdata <- testdata[, !names(testdata) %in% to_exclude_vars]

  # Um zu prüfen, ob gegenseitiger Ausschluss von Vorlagen desselben Abstimmungssonntags funktioniert ->
  # print(colnames(traindata))

  # Trainiere Model
  cv_model_mars <- caret::train(
    form,
    data = traindata %>% dplyr::select(!tidyselect::all_of(geovars)),
    method = method,
    trControl = trControl,
    ...
  )

  # lastmod <<-cv_model_mars

# cv_model_mars$results


  testdata$pred <- stats::predict(cv_model_mars,testdata)

  # TO DO :
  # Gebietslabel / ID nicht hart vorgeben, sondern via parameter der Funktion übernehmen
  # Objekt mit modell und Daten als Output
  testdata %>%
    select(tidyselect::all_of(geovars), pred, real=x) %>%
    mutate(vorlage=x)

}


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

predict_votes <- function(votes,train,test=NULL,method="bagEarth",trControl=NULL,exclude_votes=FALSE,geovars=c("gemeinde","v_gemwkid"),testprop=NA,...){

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


#' Calculate RMSE
#'
#' Root Mean Square Error (RMSE) = standard deviation of the residuals (prediction errors).
#'
#' @param m predicted value (fitted by modelling)
#' @param o oserved 'true' value
#' @param na.rm remove NA values, defaults to TRUE
#'
#' @return numeric value
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tidyr)
#'
#' pred_data  <- predict_votes(c("Eidg1","Kant1"), votedata, exclude_votes=TRUE)
#'
#' pred_data %>%
#' drop_na() %>%
#' group_by(vorlage) %>%
#' summarize(rmse=RMSE(pred,real))
#'

RMSE = function(m, o, na.rm=TRUE){
  sqrt(mean((m - o)^2,na.rm=na.rm))
}
