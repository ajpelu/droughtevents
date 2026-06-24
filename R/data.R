#' SPEI time series for Granada
#'
#' @description
#' Standardised Precipitation-Evapotranspiration Index (SPEI) time series
#' for Granada, Spain (coordinates -3.75, 37.25), at four different time
#' scales (6, 12, 24, and 48 months). SPEI is a multi-scalar drought index
#' that combines precipitation and evapotranspiration data; negative values
#' indicate drier-than-average conditions, positive values indicate
#' wetter-than-average conditions.
#'
#' Note that the longer time scales (`spei12`, `spei24`, `spei48`) require
#' that many months of prior data to be computed, so they contain `NaN`
#' values at the start of the series.
#'
#' @format A data frame (tibble) with 1,483 rows and 5 columns:
#' \describe{
#'   \item{date}{Date of the observation (class `Date`), monthly from
#'   1901-06-16 to 2024-12-16.}
#'   \item{spei6}{SPEI at the 6-month time scale.}
#'   \item{spei12}{SPEI at the 12-month time scale. `NaN` for the first
#'   months of the series.}
#'   \item{spei24}{SPEI at the 24-month time scale. `NaN` for the first
#'   months of the series.}
#'   \item{spei48}{SPEI at the 48-month time scale. `NaN` for the first
#'   months of the series.}
#' }
#'
#' @source \url{https://spei.csic.es/spei_database/}
#'
#' @references
#' Vicente-Serrano, S.M., Beguería, S., López-Moreno, J.I. (2010). A
#' Multi-scalar Drought Index Sensitive to Global Warming: The Standardized
#' Precipitation Evapotranspiration Index. *Journal of Climate*, 23(7),
#' 1696-1718. \doi{10.1175/2009JCLI2909.1}
#'
#' @examples
#' data(spei_granada)
#' head(spei_granada)
#'
#' # Detect drought events using the 12-month SPEI
#' droughts(spei_granada, vname = "spei12", threshold = -1.28)
"spei_granada"
