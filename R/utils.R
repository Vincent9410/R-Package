# Internal helper: pick dataset and validate columns ---------------------------

.pick_dataset <- function(data_type = c("climate", "fwi")) {
  data_type <- match.arg(data_type)
  if (data_type == "climate") {
    df <- ausbushfire::climate_data
    value_col <- "Temperature_Anomaly"
    label <- "Temperature anomaly (°C)"
    required <- c("Year", "Date", "Month", "Quarter", "Temperature_Anomaly")
    name <- "climate_data"
  } else {
    df <- ausbushfire::fwi_data
    value_col <- "FWI"  # FIX: use the correct column name for FWI
    label <- "Fire Weather Index (FWI)"
    required <- c("Year", "Date", "Month", "Quarter", "FWI")
    name <- "fwi_data"
  }
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop(sprintf("Dataset '%s' is missing columns: %s",
                 name, paste(missing, collapse = ", ")), call. = FALSE)
  }
  list(df = df, value_col = value_col, label = label, name = name)
}

#' Compute descriptive statistics
#'
#' Calculate a set of descriptive statistics for the selected dataset
#' (temperature anomaly or FWI) within a year range.
#'
#' @param data_type One of \code{"climate"} or \code{"fwi"}.
#' @param start_year Start year (inclusive).
#' @param end_year End year (inclusive).
#'
#' @return A tibble with one row of summary statistics.
#' @export
#'
#' @examples
#' calculate_statistics("climate", 2000, 2020)
#' calculate_statistics("fwi", 2000, 2020)
calculate_statistics <- function(data_type = c("climate", "fwi"),
                                 start_year = 1900,
                                 end_year = 2025) {
  ds <- .pick_dataset(data_type)
  df <- ds$df |>
    dplyr::filter(.data$Year >= start_year, .data$Year <= end_year)

  vals <- df[[ds$value_col]]
  n_total <- length(vals)
  n_nonmissing <- sum(!is.na(vals))

  tibble::tibble(
    Dataset = unname(data_type)[1],
    Period = paste0(start_year, "-", end_year),
    N = n_total,
    N_non_missing = n_nonmissing,
    Mean = round(mean(vals, na.rm = TRUE), 3),
    Median = round(stats::median(vals, na.rm = TRUE), 3),
    SD = round(stats::sd(vals, na.rm = TRUE), 3),
    Min = round(min(vals, na.rm = TRUE), 3),
    Max = round(max(vals, na.rm = TRUE), 3),
    IQR = round(stats::IQR(vals, na.rm = TRUE), 3)
  )
}

#' Annual trends
#'
#' Compute annual mean, max and min for the selected dataset
#' within a year range.
#'
#' @param data_type One of \code{"climate"} or \code{"fwi"}.
#' @param start_year Start year (inclusive).
#' @param end_year End year (inclusive).
#'
#' @return A tibble with columns \code{Year}, \code{Annual_Mean}, \code{Annual_Max}, \code{Annual_Min}.
#' @export
#'
#' @examples
#' annual_trends("climate", 2000, 2020)
annual_trends <- function(data_type = c("climate", "fwi"),
                          start_year = 1900,
                          end_year = 2025) {
  ds <- .pick_dataset(data_type)
  ds$df |>
    dplyr::filter(.data$Year >= start_year, .data$Year <= end_year) |>
    dplyr::group_by(.data$Year) |>
    dplyr::summarise(
      Annual_Mean = round(mean(.data[[ds$value_col]], na.rm = TRUE), 3),
      Annual_Max  = round(max(.data[[ds$value_col]], na.rm = TRUE), 3),
      Annual_Min  = round(min(.data[[ds$value_col]], na.rm = TRUE), 3),
      .groups = "drop"
    )
}

#' Extreme events frequency
#'
#' Count the number and frequency of observations exceeding a threshold
#' per year for the selected dataset.
#'
#' @param data_type One of \code{"climate"} or \code{"fwi"}.
#' @param threshold Numeric threshold (e.g. \code{1.0} for anomalies, \code{10} for FWI).
#' @param comparator Comparison operator as a string: one of \code{">"}, \code{">="}, \code{"<"}, \code{"<="}.
#'   Defaults to \code{">"}.
#' @param start_year Start year (inclusive).
#' @param end_year End year (inclusive).
#'
#' @return A tibble with columns \code{Year}, \code{Extreme_Count}, \code{Extreme_Frequency}.
#' @export
#'
#' @examples
#' # Temperature anomalies > 1°C
#' extreme_events("climate", threshold = 1.0, start_year = 2000, end_year = 2020)
#'
#' # FWI >= 10 (extreme)
#' extreme_events("fwi", threshold = 10, comparator = ">=", start_year = 2000, end_year = 2020)
extreme_events <- function(data_type = c("climate", "fwi"),
                           threshold,
                           comparator = c(">", ">=", "<", "<="),
                           start_year = 1900,
                           end_year = 2025) {
  stopifnot(is.numeric(threshold), length(threshold) == 1L, is.finite(threshold))
  comparator <- match.arg(comparator)

  ds <- .pick_dataset(data_type)
  df <- ds$df |>
    dplyr::filter(.data$Year >= start_year, .data$Year <= end_year)

  # build logical vector based on comparator
  fun <- switch(comparator,
                ">"  = function(x) x >  threshold,
                ">=" = function(x) x >= threshold,
                "<"  = function(x) x <  threshold,
                "<=" = function(x) x <= threshold)

  df |>
    dplyr::group_by(.data$Year) |>
    dplyr::summarise(
      Extreme_Count = sum(fun(.data[[ds$value_col]]), na.rm = TRUE),
      Total = dplyr::n(),
      Extreme_Frequency = round(Extreme_Count / Total, 3),
      .groups = "drop"
    ) |>
    dplyr::select(.data$Year, .data$Extreme_Count, .data$Extreme_Frequency)
}
