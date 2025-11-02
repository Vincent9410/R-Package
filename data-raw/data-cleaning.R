#' Clean and prepare Australian bushfire climate data
#'
#' @param raw_data_path Path to the raw CSV file
#' @return Cleaned tibble with climate data
#' @export
clean_climate_data <- function(raw_data_path = "data-raw/GLB.Ts+dSST.csv") {
  raw_data <- read.csv(raw_data_path, skip = 1, header = TRUE, na.strings = "***")

  names(raw_data) <- gsub("[^A-Za-z0-9]", "", names(raw_data))

  climate_data <- raw_data |>
    dplyr::select(Year, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec) |>
    tidyr::pivot_longer(
      cols = -Year,
      names_to = "Month",
      values_to = "Temperature_Anomaly"
    ) |>
    dplyr::filter(!is.na(Temperature_Anomaly)) |>
    dplyr::mutate(
      Date = as.Date(paste(Year, match(Month, month.abb), "15", sep = "-")),
      Month = factor(Month, levels = month.abb),
      Quarter = dplyr::case_when(
        Month %in% c("Jan", "Feb", "Mar") ~ "Q1",
        Month %in% c("Apr", "May", "Jun") ~ "Q2",
        Month %in% c("Jul", "Aug", "Sep") ~ "Q3",
        Month %in% c("Oct", "Nov", "Dec") ~ "Q4"
      )
    ) |>
    dplyr::arrange(Date)

  return(climate_data)
}

fwi <- read.csv("data-raw/fwi_1900_2025_monthly.csv")

fwi_data <- fwi |>
  dplyr::mutate(
    date = as.Date(date),
    Year = as.integer(format(date, "%Y")),
    Month = format(date, "%b"),
    FWI = fwi,
    Date = as.Date(paste(Year, match(Month, month.abb), "15", sep = "-")),
    Quarter = dplyr::case_when(
      Month %in% c("Jan", "Feb", "Mar") ~ "Q1",
      Month %in% c("Apr", "May", "Jun") ~ "Q2",
      Month %in% c("Jul", "Aug", "Sep") ~ "Q3",
      Month %in% c("Oct", "Nov", "Dec") ~ "Q4"
    )
  ) |>
  dplyr::select(Year, Month, FWI, Date, Quarter) |>
  dplyr::mutate(Month = factor(Month, levels = month.abb)) |>
  dplyr::arrange(Date)

climate_data <- clean_climate_data()

usethis::use_data(climate_data, overwrite = TRUE)
usethis::use_data(fwi_data, overwrite = TRUE)

View(fwi_data)
