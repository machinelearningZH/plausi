#' Test data containing past results and incomplete results for two votes.
#'
#' @description
#' Test data containing past results covering 5 years of federal and cantonal
#' votes in the canton of Zurich as well as incomplete results of two current
#' issues.
#'
#' The units presented are counting circles. Counting circles usually correspond
#' to municipalities, with the exception of the cities Zürich and Winterthur,
#' which are divided into and published as sub units.
#'
#'
#' @format A dataframe with 171 rows and 79 columns.
#' \describe{
#'   \item{gemeinde}{name of counting circle}
#'   \item{v_gemwkid}{FSO number of the municipality (for the counting circles
#'   of Zürich and Winterthur, this consists of a numeric, a zero and the FSO
#'   number of the municipality)}
#'   \item{Eidg1}{yes-share of the current incomplete federal vote}
#'   \item{Kant1}{yes-share of the current incomplete cantonal vote}
#'   \item{v_....}{yes-shares of past votes}
#'}
#'

"vote_data"



#' Test data containing results for national votes from the canton of Zurich for
#' all Sundays from 2017-03-01 to 2020-09-27.
#'
#' @format A dataframe with 3864 rows and 16 columns.
#' \describe{
#'   \item{name}{Name of the referendum.}
#'   \item{id}{ID of the referendum.}
#'   \item{canton_id}{ID of the canton.}
#'   \item{canton_name}{Name of the canton.}
#'   \item{mun_name}{Name of the municipality.}
#'   \item{geoLevelParentnummer}{Geo level number of the parent geo unit, in the case of municipalities that means district number.}
#'   \item{gebietAusgezaehlt}{Indicator for finalised counting status.}
#'   \item{jaStimmenInProzent}{Percentage of yes-votes.}
#'   \item{jaStimmenAbsolut}{Absolut number of yes-votes.}
#'   \item{neinStimmenAbsolut}{Absolut number of no-votes.}
#'   \item{stimmbeteiligungInProzent}{Turnout.}
#'   \item{eingelegteStimmzettel}{Total number of submitted ballots.}
#'   \item{anzahlStimmberechtigte}{Number of elligable voters.}
#'   \item{gueltigeStimmen}{Total number of valid yes- and no-votes.}
#'   \item{votedate}{Date of the vote.}
#'}
#'

"result_data"
