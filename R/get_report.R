#' R wrapper for luchtmeetnet report api
#'
#' @param obj The object to query
#' @param components  List of components (pm25, pm10, no2, o3)
#' @param report The report (hourly)
#' @param start Start date (i.e. 2014-01-01T20:30:00Z)
#' @param end End date (i.e. 2014-01-01T20:30:00Z)
#' @param verbose Whether to print out stuffs
#' @return A data.frame with reponse data
#' @export
#' @importFrom dplyr %>%
luchtmeetnet_get_report <- function(components, report, start, end, verbose = FALSE) {
  component_map <- list(
    "pm25" = "012502d5-6a46-479b-b1a3-1f005e9de998",
    "pm10" = "567663c8-27a4-4bc2-aa8d-d6b56baaed5b",
    "no2" = "6274c56d-4554-4f08-9d49-30aafd868349",
    "o3" = "460f0fa9-40a2-4c9c-9533-20d0135be687"
  )
  report_map <- list(
    "hourly" = "34d04bda-f739-4e9b-94e9-3e2ffae5e0db"
  )
  url <- "https://api.luchtmeetnet.nl/sos/measurements/csv"

  report_id <- report_map[[report]]

  if (is.null(report_id)) {
    stop("Please supply a valid report (hourly)")
  }

  if (as.numeric(ymd_hms(end) - ymd_hms(start), "days") > 31) {
    warning("You can't request more than 1 month of data.")
  }

  dataj <- components %>% purrr::map(function(component) {
    component_id <- component_map[[component]]
    if (is.null(component_id)) {
      stop("Please supply valid components (pm25, pm10, no2, o3)")
    }

    params = list(
      start_date = start,
      end_date = end,
      sos_component_id = component_id,
      sos_report_id = report_id
    )

    pparams <- paste(names(params), params, sep = "=", collapse = "&")
    if (verbose) message("Downloading from {url}?{pparams}" %>% glue::glue())

    r <- httr::RETRY("GET", url, query = params, times = 10)
    fd <- httr::content(r, "raw")
      # readr::read_delim(delim = ";", col_types = list(X1 = "date", .default = "numeric"), na = c("-"))
    csv <- suppressWarnings(
      readr::read_delim(fd, delim = ";", col_types = list(X1 = "T", .default = "d"), na = c("-"), skip_empty_rows = T)
    )
    data <- csv %>%
      dplyr::rename(timestamp_measured = X1) %>%
      tidyr::gather("station_number", "value", -timestamp_measured) %>%
      dplyr::mutate(formula = component %>% toupper()) %>%
      tidyr::drop_na(timestamp_measured)


    return(data)
  }) %>% purrr::reduce(union_all)

  return(dataj)
}
