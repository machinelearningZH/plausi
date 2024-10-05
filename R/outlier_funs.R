
# MAD Functions

#' Get the Median Absolute Deviation from the Median for asymmetric distributions
#'
#' Suited to find outliers in asymetric distributions (in contrast to the standard mad() function which works for symetric distributions only)
#' https://eurekastatistics.com/using-the-median-absolute-deviation-to-find-outliers/
#'
#'
#' @param x numeric vector
#' @param zero.mad.action defaults to "warn" : if MAD = 0 a warning will be displayed
#' @return numeric value
#' @importFrom stats median
#'
#' @export
#'
#' @examples
#' x <- c(1, 2, 3, 3, 4, 4, 4, 5, 5.5, 6, 6, 6.5, 7, 7, 7.5, 8, 9, 12, 52, 90)
#'
#' DoubleMAD(x)
#'

DoubleMAD <- function(x, zero.mad.action="warn"){
  # The zero.mad.action determines the action in the event of an MAD of zero.
  # Possible values: "stop", "warn", "na" and "warn and na".
  x         <- x[!is.na(x)]
  m         <- stats::median(x)
  abs.dev   <- abs(x - m)
  left.mad  <- stats::median(abs.dev[x<=m])
  right.mad <- stats::median(abs.dev[x>=m])
  if (left.mad == 0 || right.mad == 0){
    if (zero.mad.action == "stop") stop("MAD is 0")
    if (zero.mad.action %in% c("warn", "warn and na")) warning("MAD is 0")
    if (zero.mad.action %in% c(  "na", "warn and na")){
      if (left.mad  == 0) left.mad  <- NA
      if (right.mad == 0) right.mad <- NA
    }
  }
  return(c(left.mad, right.mad))
}


#' Calculate the distance of a value from the Median of a distribution in term of Median Absolute Deviations (MAD)
#'
#' @param x vector of numeric values
#' @param zero.mad.action action in the event of a MAD of zero (Options: "stop", "warn", "na" and "warn and na")
#' @importFrom stats median
#'
#' @return numeric vector
#' @export
#'
#' @examples
#' x <- c(1, 2, 3, 3, 4, 4, 4, 5, 5.5, 6, 6, 6.5, 7, 7, 7.5, 8, 9, 12, 52, 90)
#'
#' DoubleMADsFromMedian(x)

DoubleMADsFromMedian <- function(x, zero.mad.action="warn"){
  # The zero.mad.action determines the action in the event of an MAD of zero.
  # Possible values: "stop", "warn", "na" and "warn and na".
  two.sided.mad <- DoubleMAD(x, zero.mad.action)
  m <- stats::median(x, na.rm=TRUE)
  x.mad <- rep(two.sided.mad[1], length(x))
  x.mad[x > m] <- two.sided.mad[2]
  mad.distance <- abs(x - m) / x.mad
  mad.distance[x==m] <- 0
  return(mad.distance)
}


#' Detect Outliers via Median Absolute Deviation from the Median for asymmetric distributions
#'
#' Outlier detection based on MAD for asymetric distributions. Calculates separate MADs for each half of the distribution.
#' Median Absolute Deviation is a robust normalization unit based on median as a population center.
#'
#' @param value variable of interest
#' @param thres z-score threshold (defaults to 3.5).
#'
#' @return logical vector
#' @export
#'
#' @examples
#' x <- c(1, 2, 3, 3, 4, 4, 4, 5, 5.5, 6, 6, 6.5, 7, 7, 7.5, 8, 9, 12, 52, 90)
#'
#' isnt_outlier_double_mad(x)

is_outlier_double_mad <- function(value, thres=3.5){

  ifelse(plausi::DoubleMADsFromMedian(value)>=thres,TRUE,FALSE)

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
