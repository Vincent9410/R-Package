#' Global temperature anomaly dataset
#'
#' Monthly global temperature anomalies from 1880 to 2025, expressed as
#' departures from a long-term climatological baseline (°C).
#'
#' @format A data frame (or tibble) with 5 variables:
#' \describe{
#'   \item{Year}{Integer year (1880–2025)}
#'   \item{Month}{Month \emph{abbreviation} (Jan, Feb, ..., Dec)}
#'   \item{Temperature_Anomaly}{Temperature anomaly in °C (numeric)}
#'   \item{Date}{Date (set to the 15th of each month)}
#'   \item{Quarter}{Quarter label as a character: "Q1", "Q2", "Q3", "Q4"}
#' }
#'
#' @source Cleaned from NASA GISTEMP (e.g., \code{GLB.Ts+dSST.csv}).
"climate_data"

#' Synthetic Fire Weather Index (FWI) dataset
#'
#' A \strong{synthetic} monthly Fire Weather Index (FWI) dataset from 1900 to 2025,
#' provided for teaching, demonstration, and interactive exploration.
#'
#' @format A data frame (or tibble) with 5 variables:
#' \describe{
#'   \item{Year}{Integer year (1900–2025)}
#'   \item{Month}{Month \emph{abbreviation} (Jan, Feb, ..., Dec)}
#'   \item{FWI}{Fire Weather Index (numeric)}
#'   \item{Date}{Date (set to the 15th of each month)}
#'   \item{Quarter}{Quarter label as a character: "Q1", "Q2", "Q3", "Q4"}
#' }
#'
#' @details
#' Example FWI risk categories:
#' \itemize{
#'   \item 0–5: Low
#'   \item 5–15: Moderate
#'   \item 15–30: High
#'   \item 30+: Extreme
#' }
#'
#' @source \strong{Synthetic demo data} generated for teaching; not directly computed from ERA5/CFFDRS.
"fwi_data"
