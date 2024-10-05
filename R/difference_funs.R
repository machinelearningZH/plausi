#' Calculation of the voter turnout difference between two vote topics ("Vorlagen")
#'
#' @param df A data frame containing the municipality ID and voter turnout for various proposals.
#'           Each column represents a specific proposal, and the column names should correspond
#'           to the proposal IDs (e.g., 'eidg1', 'kant2').
#' @param vorl1 A character vector specifying the proposal ID for the first proposal (e.g., "eidg1", "kant2").
#'              The proposal ID should match the column name in the dataset.
#' @param vorl2 A character vector specifying the proposal ID for the second proposal (e.g., "eidg1", "kant2").
#'              This should also correspond to a column name in the dataset.
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr "%>%"
#'
#' @return Tibble with the turnout difference between two vote topics
#' @export
#'
#' @examples
#'
#' testdata <- tibble(gemwkid = c(13,49,41,43,44),
#' eidg1 = c(60.90,61.18,65.27,55.36,57.68),
#' eidg2 = c(62.16,62.54,66.95,56.65,58.68),
#' kant1 = c(57.73,60.27,63.31,51.93,54.49))
#'
#' cross_fun(testdata,"eidg1","eidg2")
#'
#'  # generate combinations
#' combinations <-as.data.frame(t(combn(c("eidg1","eidg2","kant1"),2)))
#'
#' # difference between columns named as the first combination
#' cross_fun(crosscheckdata,combinations$V1[1],combinations$V2[1])
#'
cross_fun <- function(df, vorl1, vorl2,geo_cols=geocols){

  varname <- paste0(vorl1,"_",vorl2)

  df %>% dplyr::mutate(!!varname :=.data[[as.character(vorl1)]] -.data[[as.character(vorl2)]]) %>%
    dplyr::select(tidyselect::all_of(geo_cols),!!varname) %>%
    pivot_longer(cols=varname,names_to="combination",values_to="difference")
}


#' Calculation of voter turnout differences between all vote topics ("Vorlagen")
#'
#' @param df A data frame containing the municipality ID and voter turnout for various vote topics,
#'           with each vote topic in a separate column.
#' @param vorl1 A vector of proposal IDs corresponding to column names in the dataset (e.g., "eidg1", "kant2").
#' @param vorl2 A vector of proposal IDs (e.g., "eidg1", "kant2") for comparison with `vorl1`.
#' @importFrom dplyr "%>%"
#' @importFrom dplyr select
#' @importFrom dplyr bind_cols
#' @importFrom dplyr contains
#' @importFrom dplyr ends_with
#' @importFrom purrr map2
#'
#' @return A tibble containing voter turnout differences between all possible combinations of vote topics.
#' @export
#'
#' @examples
#' library(tibble)
#'
#' testdata <- tibble(gemwkid = c(13,49,41,43,44),
#' eidg1 = c(60.90,61.18,65.27,55.36,57.68),
#' eidg2 = c(62.16,62.54,66.95,56.65,58.68),
#' kant1 = c(57.73,60.27,63.31,51.93,54.49))
#'
#' single_difference(testdata,"eidg1","eidg2")
#'
#'  # generate combinations
#' combinations <-as.data.frame(t(combn(c("eidg1","eidg2","kant1"),2)))
#'
#' # calculate all possible differences between columns
#'
#' get_differences(testdata,combinations$V1,combinations$V2)
#'

get_differences <- function(df, vorl1,vorl2, geo_cols=c("gemwkid","gemeinde")){

  crosscheckdata_new <- purrr::map2_dfr(vorl1,vorl2,~cross_fun(df,.x,.y,geo_cols = geo_cols))

}

