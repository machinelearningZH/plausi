#' Get the left and right Median Absolute Deviations (MAD) from the median for asymmetric distributions
#'
#' Suited to find outliers in asymetric distributions (in contrast to the standard mad() function which works for symmetric distributions only)
#' The function splits the values along the median and returns separate MADs for the left and the right side of the distribution.
#' https://eurekastatistics.com/using-the-median-absolute-deviation-to-find-outliers/
#'
#' @param x A vector of numeric values.
#' @param zero_mad_action Determines the action in the event of an MAD of zero.
#' Defaults to NULL. The options are:
#' * \strong{NULL}: process runs with no warning
#' * \strong{"warn"}: a warning will be displayed
#' * \strong{"stop"}: process is stopped
#'
#' @return A numeric vector of length 2.
#'
#' @importFrom stats median
#'
#' @export
#'
#' @examples
#'
#' x <- c(1, 2, 3, 3, 4, 4, 4, 5, 5.5, 6, 6, 6.5, 7, 7, 7.5, 8, 9, 12, 52, 90)
#'
#' double_mad(x)
#'

double_mad <- function(x, zero_mad_action = NULL){

  if (!is.numeric(x)) {
    stop("Your input must be numeric.")
  }

  # drop all NAs
  x <- x[!is.na(x)]

  # calculate the median
  median_x <- stats::median(x)

  # calculate the absolute deviations
  abs_dev <- abs(x - median_x)

  # calculate the left and the right MADs
  left_mad <- stats::median(abs_dev[x <= median_x])
  right_mad <- stats::median(abs_dev[x >= median_x])

  # handling of MAD = 0
  if (!is.null(zero_mad_action) && (left_mad == 0 || right_mad == 0)) {
    if (zero_mad_action == "stop") stop("MAD is 0")
    if (zero_mad_action == "warn") warning("MAD is 0")
  }

  return(c(left_mad, right_mad))
}





#' Calculate the distance of a value from the median of a distribution in relation to its Median Absolute Deviation (MAD)
#'
#' This function is suited to find outliers in asymetric distributions (in contrast to the standard mad() function which works for
#' symetric distributions only). The function splits the values along the median and returns the distance for every value from the
#' median, relative to the left or right side MAD.
#' https://eurekastatistics.com/using-the-median-absolute-deviation-to-find-outliers/
#'
#' @inheritParams double_mad
#'
#' @importFrom stats median
#'
#' @return A numeric vector of length \code{length(x)}.
#' @export
#'
#' @examples
#'
#' x <- c(1, 2, 3, 3, 4, 4, 4, 5, 5.5, 6, 6, 6.5, 7, 7, 7.5, 8, 9, 12, 52, 90)
#'
#' double_mad_from_median(x)
#'

double_mad_from_median <- function(x, zero_mad_action = NULL){

  # get left/right MAD
  two_sided_mad <- double_mad(x, zero_mad_action)

  # calculate the median
  median_x <- stats::median(x)

  # create vector of left/right MADs (don't do it with length(x)/2 since it is possible that x == median_x exists multiple times in the vector)
  left_mad <- rep(two_sided_mad[1], length(x[x <= median_x]))
  right_mad <- rep(two_sided_mad[2], length(x[x > median_x]))
  x_mad <- c(left_mad, right_mad)

  # calculate MAD distance, that is distance of every value to the median, relative to the left/right MAD
  mad_distance <- abs(x - median_x) / x_mad
  mad_distance[x == median_x] <- 0

  return(mad_distance)
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
#' # Set seed for reproducibility
#' set.seed(42)
#'
#' pred_data <- predict_votes(c("Eidg1", "Kant1"), vote_data, exclude_votes = TRUE)
#'
#' pred_data$rmse <- rmse(pred_data$pred, pred_data$real)
#'

rmse = function(prediction, observation, na.rm = TRUE){

  if (length(prediction) != length(observation)) {
    stop("The vectors prediction and observation must have the same length().")
  }

  sqrt(mean((prediction - observation) ^ 2, na.rm = na.rm))

}





#' Calculate RMSE after excluding the most extreme values
#'
#' Calculate the Root Mean Square Error (RMSE) after the exclusion of the most extreme values.
#' The RMSE is the standard deviation of the residuals (prediction errors) and therefore an
#' indicator of how precise the prediction of a specific vote actually is.
#'
#' @inheritParams rmse
#' @param cutoff_min Numeric. Minimum number of highest residuals to be cut off before calculating the RMSE.
#' @param cutoff_perc Numeric. Percentage of highest residuals to be cut off before calculating the RMSE.
#'
#' @return A vector of numeric values.
#' @export
#'
#' @examples
#'
#' # Set seed for reproducibility
#' set.seed(42)
#'
#' pred_data <- predict_votes(c("Eidg1", "Kant1"), vote_data, exclude_votes = TRUE)
#'
#' pred_data$rmse <- rmse_cutoff(pred_data$pred, pred_data$real, 3, 4)
#'

rmse_cutoff = function(prediction, observation, cutoff_min = 2, cutoff_perc = 5, na.rm = TRUE) {

  # Check inputs
  if (length(prediction) != length(observation)) {
    stop("The vectors prediction and observation must have the same length().")
  }

  if (!is.numeric(cutoff_min) || !is.numeric(cutoff_perc)) {
    stop("The cutoff values must be numeric.")
  }

  if (length(prediction) <= cutoff_min || cutoff_perc >= 100) {
    stop("You cannot cut off more values than you are analysing.")
  }

  # Compute absolute differences
  diffs <- abs(prediction - observation)

  # Determine how many values to remove
  n <- length(diffs)
  n_remove <- max(cutoff_min, ceiling(0.01 * cutoff_perc * n))

  # Find the indices of the n_remove largest differences
  # Sort indices by decreasing order of differences
  sorted_indices <- order(diffs, decreasing = TRUE)

  # Remove the n_remove cases with largest differences
  if (n_remove > 0) {
    # Get indices of the cases to remove (those with largest differences)
    remove_indices <- sorted_indices[1:n_remove]

    # Keep all cases except those to be removed
    keep <- !(seq_along(diffs) %in% remove_indices)
  } else {
    keep <- rep(TRUE, n)
  }

  # Calculate RMSE with remaining values
  sqrt(mean((prediction - observation)[keep] ^ 2, na.rm = na.rm))

}





#' Detect outliers using MAD from the median for asymmetric distributions
#'
#' Outlier detection based on Median Absolute Deviation (MAD) for asymmetric distributions. The function calculates the distance
#' to the median for every value in the distribution relative to the left or right side MAD. It then compares the value to your
#' threshold and labels the outliers.
#'
#' @inheritParams double_mad
#' @param threshold Z-score threshold (defaults to 3.5).
#'
#' @return A logical vector.
#' @export
#'
#' @examples
#'
#' x <- c(1, 2, 3, 3, 4, 4, 4, 5, 5.5, 6, 6, 6.5, 7, 7, 7.5, 8, 9, 12, 52, 90)
#'
#' is_outlier_double_mad(x)
#'

is_outlier_double_mad <- function(x, zero_mad_action = NULL, threshold = 3.5){

  ifelse(double_mad_from_median(x, zero_mad_action) >= threshold, TRUE, FALSE)

}





#' Get boundaries beyond which a value is an outlier via MAD from the median for asymmetric distributions and IQR
#'
#' Outlier detection based on Median Absolute Deviation (MAD) for asymetric distributions and interquartile range. The function
#' calculates the distance to the median for every value in the distribution relative to the left or right side MAD. It then
#' compares the value to your threshold and labels the outliers.
#'
#' @inheritParams is_outlier_double_mad
#' @param percent Indicator for the scale of the data. If function is run for percantage data, the lower limit will not be negative
#' while the upper limit does not exceed 100 percent. Defaults to TRUE.
#'
#' @importFrom stats median
#' @importFrom stats IQR
#'
#' @return A data.frame with numeric range.
#' @export
#'
#' @examples
#'
#' x <- c(1, 2, 3, 3, 4, 4, 4, 5, 5.5, 6, 6, 6.5, 7, 7, 7.5, 8, 9, 12, 52, 90)
#'
#' outlier_range(x)
#'

outlier_range <- function(x, zero_mad_action = NULL, threshold = 3.5, percent = TRUE){

  if (!is.numeric(x)) {
    stop("Your input must be numeric.")
  }

  # create table
  data <- data.frame(
    median = stats::median(x),
    iqr = stats::IQR(x),
    lower = round(stats::median(x) - double_mad(x)[1] * threshold, 2),
    upper = round(stats::median(x) + double_mad(x)[1] * threshold, 2)
  )

  # limit bandwidth in case of percentage scale
  if(percent == TRUE){


    data["lower"] <- ifelse(data$lower < 0, 0, data$lower)
    data["upper"] <- ifelse(data$upper > 100, 100, data$upper)

  }

  # add label column
  data["label"] <- paste0(data$lower, " - ", data$upper)

  # return the table
  return(data)

}





#' Detect outliers using Z-score with MAD for symmetric distributions
#'
#' Outlier detection based on Median Absolute Deviation (MAD) for symmetric distributions. The function calculates the distance
#' to the median for every value in the distribution relative to the MAD. It then compares the value to your threshold and
#' labels the outliers.
#'
#'
#' @inheritParams double_mad
#' @param threshold Z-score threshold (defaults to 3).
#' @param na.rm Remove NAs, defaults to TRUE.
#'
#' @importFrom stats median
#' @importFrom stats mad
#'
#' @return A logical vector.
#' @export
#'
#' @examples
#'
#' x <- c(1, 2, 3, 3, 4, 4, 4, 5, 5.5, 6, 6, 6.5, 7, 7, 7.5, 8, 9, 12, 52, 90)
#'
#' is_outlier_single_mad(x)
#'

is_outlier_single_mad <- function(x, threshold = 3, na.rm = TRUE) {

  abs(x - stats::median(x, na.rm = na.rm)) > threshold * stats::mad(x, na.rm = na.rm)

}





#' Detect outliers using classic Z-scores for symmetric distributions
#'
#' Outlier detection based on Z-scores for symetric distributions. The function calculates the Z-score, i. e. the distance of a value
#' from the mean in number of standard deviations.
#'
#'
#' @inheritParams is_outlier_single_mad
#'
#' @importFrom stats sd
#'
#' @return A logical vector.
#' @export
#'
#' @examples
#'
#' x <- c(1, 2, 3, 3, 4, 4, 4, 5, 5.5, 6, 6, 6.5, 7, 7, 7.5, 8, 9, 12, 52, 90)
#'
#' is_outlier_z(x)
#'

is_outlier_z <- function(x, threshold = 3, na.rm = TRUE) {

  abs(x - mean(x, na.rm = na.rm)) > threshold * stats::sd(x, na.rm = na.rm)

}





#' Detect outliers using turkey's fences
#'
#' Outlier detection based on turkey's fences. Tukey’s fences is a technique used in box plots. The non-outlier range is defined as
#' Q1−k(Q3−Q1), Q3+k(Q3−Q1), where Q1 and Q3 are the lower and upper quartiles respectively and k - some non-negative constant
#' (popular choice is 1.5).
#'
#'
#' @inheritParams double_mad
#' @param threshold Multiplier for the IQR to set outlier boundaries. Higher values widen the range; default is 1.5.
#' @param na.rm if TRUE, removes NA values before calculations. Default is TRUE.
#'
#' @importFrom stats quantile
#'
#' @return A logical vector.
#'
#' @export
#'
#' @examples
#'
#' x <- c(1, 2, 3, 3, 4, 4, 4, 5, 5.5, 6, 6, 6.5, 7, 7, 7.5, 8, 9, 12, 52, 90)
#'
#' is_outlier_turkey(x)
#'

is_outlier_turkey <- function(x, threshold = 1.5, na.rm = TRUE) {

  quar <- stats::quantile(x, probs = c(0.25, 0.75), na.rm = na.rm)

  iqr <- diff(quar)

  (quar[1] - threshold * iqr > x) | (x > quar[2] + threshold * iqr) # must not be >= or <= since identical values would be counted as outliers

}
