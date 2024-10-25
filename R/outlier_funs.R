#' Get the left and right Median Absolute Deviations (MAD) from the median for asymmetric distributions
#'
#' Suited to find outliers in asymetric distributions (in contrast to the standard mad() function which works for symetric distributions only)
#' The function splits the values along the median and returns separate MADs for the left and the right side of the distribution.
#' https://eurekastatistics.com/using-the-median-absolute-deviation-to-find-outliers/
#'
#' @param x a vector of numeric values
#' @param zero_mad_action Determines the action in the event of an MAD of zero.
#' Defaults to NULL. The options are:
#' * \strong{NULL}: process runs with no warning
#' * \strong{"warn"}: a warning will be displayed
#' * \strong{"stop"}: process is stopped
#'
#' @return numeric vector of length 2
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
#' Suited to find outliers in asymetric distributions (in contrast to the standard mad() function which works for symetric distributions only)
#' The function splits the values along the median and returns the distance for every value from the median, relative to the left or right side MAD.
#' https://eurekastatistics.com/using-the-median-absolute-deviation-to-find-outliers/
#'
#' @inheritParams double_mad
#' @importFrom stats median
#'
#' @return numeric vector of length length(x)
#' @export
#'
#' @examples
#' x <- c(1, 2, 3, 3, 4, 4, 4, 5, 5.5, 6, 6, 6.5, 7, 7, 7.5, 8, 9, 12, 52, 90)
#'
#' double_mad_from_median(x)

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



#' Detect Outliers via Median Absolute Deviation from the Median for asymmetric distributions
#'
#' Outlier detection based on MAD for asymetric distributions. The function calculates the distance to the median for every value in the
#' distribution relative to the left or right side MAD. It then compares the value to your threshold and labels the outliers.
#'
#' @inheritParams double_mad
#' @param thres z-score threshold (defaults to 3.5).
#'
#' @return logical vector
#' @export
#'
#' @examples
#' x <- c(1, 2, 3, 3, 4, 4, 4, 5, 5.5, 6, 6, 6.5, 7, 7, 7.5, 8, 9, 12, 52, 90)
#'
#' is_outlier_double_mad(x)

is_outlier_double_mad <- function(x, zero_mad_action = NULL, thres = 3.5){

  ifelse(plausi::double_mad_from_median(x, zero_mad_action) >= thres, TRUE, FALSE)

}













































#' Get boundaries beyond which a value is an outlier via Median Absolute Deviation from the Median for asymmetric distributions
#'
#' @param value variable of interest
#' @param thres z-score threshold (defaults to 3.5, which is a popular choice).
#' @param percent defaults to TRUE. Values below zero are set to 0, values over 100 to 100.
#'
#' @return tibble with numeric range
#' @export

outlier_range<- function(value, thres=3.5,percent=TRUE){

  # iqr_thres as argument to allow for more strict criteria for groups with high variance?
  # if(IQR(value)>iqr_thres) thres <- 1


  data <- tibble(median=median(value),
         iqr=IQR(value),
         lower=median(value)-plausi::DoubleMAD(value)[1]*thres,
         upper=median(value)+plausi::DoubleMAD(value)[2]*thres
  )

if(percent==TRUE){

 data <-data %>%
    mutate(lower=ifelse(lower<0,0,round(lower,1)),
           upper=ifelse(upper>100,100,round(upper,1)))

}

  data %>%
    mutate(label=paste(lower,"-",upper))

}





# useful functions for outlier detection (combined)
# http://www.questionflow.org/2017/12/26/combined-outlier-detection-with-dplyr-and-ruler/


#' Z-score with MAD
#'
#' Outlier detection based on MAD. Median Absolute Deviation is a robust normalization unit based on median as a population center. In order to use MAD “as a consistent estimator for the estimation of the standard deviation” one takes its value multiplied by a factor.
#'
#' @param x variable of interest
#' @param thres z-score threshold (defaults to 3, which is a popular choice).
#' @param na.rm remove NAs, defaults to TRUE
#' @importFrom stats median
#' @importFrom stats mad
#'
#' @return logical vector
#' @export
#'
#' @examples
#' x <- c(1, 2, 3, 3, 4, 4, 4, 5, 5.5, 6, 6, 6.5, 7, 7, 7.5, 8, 9, 12, 52, 90)
#'
#' isnt_out_mad(x)
#'

isnt_out_mad <- function(x, thres = 3, na.rm = TRUE) {
  abs(x - stats::median(x, na.rm = na.rm)) <= thres * stats::mad(x, na.rm = na.rm)
}


#' Z-score
#'
#' Z-score, also called a standard score, of an observation is broadly speaking a distance from the population center measured in number of normalization units. The default choice for center is sample mean and for normalization unit is standard deviation.
#'
#' @param x variable of interest
#' @param thres z-score threshold (defaults to 3, which is a popular choice).
#' @param na.rm remove NAs, defaults to TRUE
#' @importFrom stats quantile
#' @importFrom stats sd
#'
#' @return logical vector
#' @export
#'
#' @examples
#' x <- c(1, 2, 3, 3, 4, 4, 4, 5, 5.5, 6, 6, 6.5, 7, 7, 7.5, 8, 9, 12, 52, 90)
#'
#' isnt_out_z(x)
#'

isnt_out_z <- function(x, thres = 3, na.rm = TRUE) {
  abs(x - mean(x, na.rm = na.rm)) <= thres * stats::sd(x, na.rm = na.rm)
}


#' Tukey’s fences
#'
#' Tukey’s fences is a technique used in box plots. The non-outlier range is defined with [Q1−k(Q3−Q1), Q3+k(Q3−Q1)], where Q1 and Q3 are the lower and upper quartiles respectively, k - some nonnegative constant (popular choice is 1.5).
#'
#' @param x variable of interest
#' @param k defaults to 1.5
#' @param na.rm remove NAs, defaults to TRUE
#' @importFrom stats quantile
#'
#' @return logical vector
#' @export
#'
#' @examples
#' x <- c(1, 2, 3, 3, 4, 4, 4, 5, 5.5, 6, 6, 6.5, 7, 7, 7.5, 8, 9, 12, 52, 90)
#'
#' isnt_out_turkey(x)

isnt_out_turkey <- function(x, k = 1.5, na.rm = TRUE) {
  quar <- stats::quantile(x, probs = c(0.25, 0.75), na.rm = na.rm)
  iqr <- diff(quar)

  (quar[1] - k * iqr <= x) & (x <= quar[2] + k * iqr)
}


# maha_dist <- . %>% select_if(is.numeric) %>%
#   mahalanobis(center = colMeans(.), cov = cov(.))
#
# isnt_out_maha <- function(tbl, isnt_out_f, ...) {
#   tbl %>% maha_dist() %>% isnt_out_f(...)
# }

# isnt_out_funs_long <- funs(
#   z_long = isnt_out_z,
#   mad_long = isnt_out_mad,
#   turkey_long = isnt_out_turkey
# )
#

# isnt_out_funs_cross <- funs(
#   z_cross = isnt_out_z,
#   mad_cross = isnt_out_mad,
#   turkey_cross = isnt_out_turkey
# )
