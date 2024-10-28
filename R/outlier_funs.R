#' Get the left and right Median Absolute Deviations (MAD) from the median for asymmetric distributions
#'
#' Suited to find outliers in asymetric distributions (in contrast to the standard mad() function which works for symetric distributions only)
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



#' Detect outliers using MAD from the median for asymmetric distributions
#'
#' Outlier detection based on Median Absolute Deviation (MAD) for asymetric distributions. The function calculates the distance
#' to the median for every value in the distribution relative to the left or right side MAD. It then compares the value to your
#' threshold and labels the outliers.
#'
#' @inheritParams double_mad
#' @param thres Z-score threshold (defaults to 3.5).
#'
#' @return A ogical vector.
#' @export
#'
#' @examples
#' x <- c(1, 2, 3, 3, 4, 4, 4, 5, 5.5, 6, 6, 6.5, 7, 7, 7.5, 8, 9, 12, 52, 90)
#'
#' is_outlier_double_mad(x)
#'

is_outlier_double_mad <- function(x, zero_mad_action = NULL, thres = 3.5){

  ifelse(plausi::double_mad_from_median(x, zero_mad_action) >= thres, TRUE, FALSE)

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
#' x <- c(1, 2, 3, 3, 4, 4, 4, 5, 5.5, 6, 6, 6.5, 7, 7, 7.5, 8, 9, 12, 52, 90)
#'
#' outlier_range(x)
#'

outlier_range <- function(x, zero_mad_action = NULL, thres = 3.5, percent = TRUE){

  # create table
  data <- data.frame(
    median = stats::median(x),
    iqr = stats::IQR(x),
    lower = round(stats::median(x) - plausi::double_mad(x)[1] * thres, 2),
    upper = round(stats::median(x) + plausi::double_mad(x)[1] * thres, 2)
  )

  # limit bandwidth in case of percentage scale
  if(percent == TRUE){

    data <- data |>
      mutate(
        lower = ifelse(lower < 0, 0, lower),
        upper = ifelse(upper > 100, 100, upper)
      )
  }

  # add label column
  data["label"] <- paste0(data$lower, " - ", data$upper)

}



#' Detect outliers using Z-score with MAD for symmetric distributions
#'
#' Outlier detection based on Median Absolute Deviation (MAD) for symetric distributions. The function calculates the distance
#' to the median for every value in the distribution relative to the MAD. It then compares the value to your threshold and
#' labels the outliers.
#'
#'
#' @inheritParams double_mad
#' @param thres Z-score threshold (defaults to 3).
#' @param na.rm Remove NAs, defaults to TRUE.
#'
#' @importFrom stats median
#' @importFrom stats mad
#'
#' @return A logical vector.
#' @export
#'
#' @examples
#' x <- c(1, 2, 3, 3, 4, 4, 4, 5, 5.5, 6, 6, 6.5, 7, 7, 7.5, 8, 9, 12, 52, 90)
#'
#' is_outtlier_single_mad(x)
#'

is_outtlier_single_mad <- function(x, thres = 3, na.rm = TRUE) {

  abs(x - stats::median(x, na.rm)) >= thres * stats::mad(x, na.rm)

}


#' Detect outliers using classic Z-scores for symmetric distributions
#'
#' Outlier detection based on Z-scores for symetric distributions. The function calculates the Z-score, i. e. the distance of a value
#' from the mean in number of standard deviations.
#'
#'
#' @inheritParams is_outtlier_single_mad
#'
#' @importFrom stats sd
#'
#' @return A logical vector.
#' @export
#'
#' @examples
#' x <- c(1, 2, 3, 3, 4, 4, 4, 5, 5.5, 6, 6, 6.5, 7, 7, 7.5, 8, 9, 12, 52, 90)
#'
#' is_outlier_z(x)
#'

is_outlier_z <- function(x, thres = 3, na.rm = TRUE) {

  abs(x - mean(x, na.rm)) >= thres * stats::sd(x, na.rm)

}



#' Detect outliers using turkey's fences
#'
#' Outlier detection based on turkey's fences. Tukey’s fences is a technique used in box plots. The non-outlier range is defined as
#' [Q1−k(Q3−Q1), Q3+k(Q3−Q1)], where Q1 and Q3 are the lower and upper quartiles respectively and k - some non-negative constant
#' (popular choice is 1.5).
#'
#'
#' @inheritParams double_mad
#' @param thres Turkey's fences threshold (defaults to 1.5). Amount of how many IQRs bellow and above the first and third quartiles are
#' accapted as within the bandwidth.
#'
#' @importFrom stats quantile
#'
#' @return A logical vector.
#'
#' @export
#'
#' @examples
#' x <- c(1, 2, 3, 3, 4, 4, 4, 5, 5.5, 6, 6, 6.5, 7, 7, 7.5, 8, 9, 12, 52, 90)
#'
#' is_outlier_turkey(x)
#'

is_outlier_turkey <- function(x, k = 1.5, na.rm = TRUE) {

  quar <- stats::quantile(x, probs = c(0.25, 0.75), na.rm)

  iqr <- diff(quar)

  (quar[1] - k * iqr >= x) | (x >= quar[2] + k * iqr)

}
