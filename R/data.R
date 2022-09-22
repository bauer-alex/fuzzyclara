
#' Data from the German Reiseanalyse survey
#' 
#' This dataset from the
#' \href{https://reiseanalyse.de/home/}{Reiseanalyse survey} comprises travel
#' information on German travelers between 2009 and 2018. Data were collected
#' in a yearly repeated cross-sectional survey of German pleasure travels,
#' based on a sample representative for all German-speaking residents in
#' Germany. Note that the sample only contains trips with at least five days of
#' trip length.
#' 
#' The data are a 20% random sample of all respondents who undertook at least
#' one trip in the respective year, between 2010 and 2018. We thank the
#' \href{https://reiseanalyse.de/about-us/}{Forschungsgemeinschaft Urlaub und Reisen e.V.}
#' for allowing us to publish this sample.
#' 
#' @docType data
#' 
#' @usage data(travel)
#' 
#' @format A dataframe containing
#' \describe{
#'   \item{travel_year}{Year in which the respondent traveled.}
#'   \item{number_trips}{Number of trips of the respondent in the respective
#'   year.}
#'   \item{totalExpenses}{Summed (inflation-adjusted, with base year 2018)
#'   expenses (in €) of the respondent for all trips in the respective year.}
#'   \item{max_travelDistance}{Maximum travel distance (in km) among all trips
#'   of the respondent in the respective year.}
#'   \item{max_tripLength}{Maximum trip length (in days) among all trips of the
#'   respondent in the respective year.}
#' }
#' 
#' @references Forschungsgemeinschaft Urlaub und Reisen e.V. (FUR) (2020b) \emph{Survey of
#' tourist demand in Germany for holiday travel and short breaks}. Available at:
#' \href{https://reiseanalyse.de/wp-content/uploads/2019/08/RA2020_Infoflyer_EN.pdf}{https://reiseanalyse.de/wp-content/uploads/2019/08/RA2020_Infoflyer_EN.pdf}
#' (accessed 22 November 2021).
#' 
#' @keywords datasets
#' 
"travel"
