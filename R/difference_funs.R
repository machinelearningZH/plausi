#' Calculation of the voter turnout difference between two votes
#'
#' This function creates a table with the differences in turnout between two
#' votes for every counting circle in the original data.
#'
#' @param df A table containing the municipality ID and voter turnout for
#' various issues. Each column represents a specific issue, and the column names
#' should correspond to the issue IDs (e.g., 'eidg1', 'kant2').
#' @param issue1,issue2 A character vector specifying the name of the columns
#' containing the voter turnout of the issues of interest (e.g., "eidg1",
#' "kant2").
#' @param geo_cols The name of the geo-column containing an identifier of the
#' counting circle.
#'
#' @importFrom stats reshape
#'
#' @return A dataframe with the turnout difference between two vote topics.
#' @export
#'
#' @examples
#'
#' testdata <- data.frame(
#'   gemwkid = c(13,49,41,43,44),
#'   eidg1 = c(60.90,61.18,65.27,55.36,57.68),
#'   eidg2 = c(62.16,62.54,66.95,56.65,58.68),
#'   kant1 = c(57.73,60.27,63.31,51.93,54.49)
#' )
#'
#' cross_fun(testdata, "eidg1", "eidg2", "gemwkid")
#'
#'  # generate combinations
#' combinations <- as.data.frame(t(combn(c("eidg1", "eidg2", "kant1"), 2)))
#'
#' # difference between columns named as the first combination
#' cross_fun(testdata, combinations$V1[1], combinations$V2[1], "gemwkid")
#'

cross_fun <- function(df, issue1, issue2, geo_cols){

  # stop if not all columns are in the data
  if (!all(sapply(c(issue1, issue2, geo_cols), function(col) col %in% names(df)))) {
    stop("The arguments issue1, issue2 and geo_cols must be column names of your data frame (df) in quotation marks.")
  }

  # define the name of the new comparison variable
  varname <- paste0(issue1, "_", issue2)

  # define the content of the new comparison variable as a new column in the dataframe, with the name stored in varname
  df[[varname]] <- df[[as.character(issue1)]] - df[[as.character(issue2)]]

  # select the geographical columns and the new comparison variable
  df_selected <- df[, c(geo_cols, varname)]

  # pivot longer (convert from wide to long format)
  # In this case, we are using reshape() to replicate pivot_longer()
  df_long <- reshape(
    df_selected,
    varying = varname,
    v.names = "difference",
    timevar = "combination",
    times = varname,
    direction = "long"
  ) |>
    subset(select = -id)

  # remove row names that were generated by reshape
  rownames(df_long) <- NULL

  # return the long-format dataframe
  return(df_long)

}



#' Calculation of the voter turnout difference between multiple votes
#'
#' This function creates a table with the differences in turnout between
#' multiple votes for every counting circle in the original data.
#'
#'
#' @param df A table containing the counting circle ID and voter turnout for
#' various issues. Each column represents a specific issue, and the column names
#' should correspond to the issue IDs (e.g., 'eidg1', 'kant2').
#' @param comb1,comb2 A character vector specifying the the first and second set
#' of column to be compared. The column names represent columns in df that
#' contain voter turnout data of the issues of interest (e.g., "eidg1", "kant2").
#' @param geo_cols The name of the geo-column containing an identifier of the
#' counting circle.
#'
#' @return A dataframe containing voter turnout differences between all
#' combinations of vote issues defined.
#' @export
#'
#' @examples
#'
#' testdata <- data.frame(
#'   gemwkid = c(13,49,41,43,44),
#'   eidg1 = c(60.90,61.18,65.27,55.36,57.68),
#'   eidg2 = c(62.16,62.54,66.95,56.65,58.68),
#'   kant1 = c(57.73,60.27,63.31,51.93,54.49)
#' )
#'
#' # generate combinations
#' combinations <- as.data.frame(t(combn(c("eidg1", "eidg2", "kant1"), 2)))
#'
#' # calculate all possible differences between columns
#' get_differences(testdata, combinations$V1, combinations$V2, "gemwkid")
#'

get_differences <- function(df, comb1, comb2, geo_cols = c("gemwkid", "gemeinde")){

  # error if combination vectors do not have the same length
  if (length(comb1) != length(comb2)) {
    stop("Both combination vectors comb1 and comb2 must have the same length().")
  }

  # run cross_fun() over all combinations and bind rows
  crosscheckdata_new <- do.call(rbind, Map(function(x, y) cross_fun(df, x, y, geo_cols = geo_cols), comb1, comb2))

  # remove row names
  rownames(crosscheckdata_new) <- NULL

  # return data
  return(crosscheckdata_new)

}
