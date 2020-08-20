luchtmeetnet_get_data <- function(obj, params = list(), verbose = FALSE) {
  luchtmeetnet_request <- function (url, params) {
    next_page = 1
    last_page = -1
    current_page = 0

    pages <- list()

    while (current_page != last_page) {
      params$page <- next_page
      pparams <- paste(names(params), params, sep = "=", collapse = "&")
      if (verbose) message("Downloading from {url} with {pparams}" %>% glue::glue())
      r <- httr::RETRY("GET", url, query = params, times = 10)
      httr::stop_for_status(r)
      body <- httr::content(r)

      if(is.null(names(body$data))) {
        pages[[next_page]] = body$data %>%
          dplyr::bind_rows()
      } else {
        # not a paginated list
        return(body$data)
      }

      next_page <- body$pagination$next_page
      last_page <- body$pagination$last_page
      current_page <- body$pagination$current_page
    }

    return(pages %>% jsonlite::rbind_pages())

  }

  url <- "https://api.luchtmeetnet.nl/open_api/{obj}/" %>% glue::glue()
  return(luchtmeetnet_request(url, params))
}
