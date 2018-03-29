

# API version to use
API_VER <- "v1.4"

# API names of the data that can be retreived
# index by version number
AVAILABLE_DATA <- list("v1.4" = c("user-information",
                                  "study-queue",
                                  "level-progression",
                                  "srs-distribution",
                                  "recent-unlocks",
                                  "critical-items",
                                  "radicals",
                                  "kanji",
                                  "vocabulary"))


#' Get a specific "data unit" from the WaniKani API
#'
#' Extracts a unit of data from the WaniKani API by name.
#' The names correspond to the strings used in the URL
#' requests and can be found on the WaniKani API documentation page.
#'
#' @export
#'
#' @param data_name the name of the "data unit" -- see the API docs
#' @param api_key the users API key
#' @param api_ver the version of the API to use; defaults to API_VER
#' @param opt_arg the optional URL argument that can be used for some "data units"
#' @param time_message print a time message
#' @param print_url print the URL string being used, useful for debugging
#'
#' @return results in the form of a list
#'
get_wk_data_by_name <- function(data_name, api_key, api_ver = API_VER,
                                opt_arg = NULL, time_message = TRUE, print_url = TRUE) {

  # verify the args
  if (!api_ver %in% names(AVAILABLE_DATA)) {
    stop(paste0(api_ver, " is not a correct API version; must be one of {",
                paste(names(AVAILABLE_DATA), collapse = ", "), "}"))
  }

  if (!data_name %in% AVAILABLE_DATA[[api_ver]]) {
    stop(paste0(data_name, " is not a correct data name; must be of of {",
                paste(AVAILABLE_DATA[[api_ver]], collapse = ", "), "}"))
  }

  t0 <- lubridate::now()

  base_url <- paste0("https://www.wanikani.com/api/", api_ver, "/user/", api_key, "/")
  data_url <- paste0(base_url, data_name)
  if (!is.null(opt_arg)) {
    data_url <- paste0(data_url, "/", opt_arg)
  }

  if (print_url) {
    cat(paste0("... ", data_url, "\n"))
  }

  # flatten = TRUE flattens out nested data frames
  # into a single data frame with prefixed column names
  results <- jsonlite::fromJSON(RCurl::getURL(data_url), flatten = TRUE)

  # Just parse out the info we need
  # This depends on the data type
  if (data_name == "user-information") {
    results <- results$user_information
  } else {
    results <- results$requested_information
  }

  t1 <- lubridate::now()

  if (time_message) {
    print_time_message(data_name, t0, t1)
  }

  results
}


#' Get all of the available data from the WaniKani API
#'
#' Retrieves all of the data from the the WaniKani API
#'
#' @export
#'
#' @param api_key the users API key
#' @param api_ver the API version to use
#' @param cache_dir directory read cached data from
#' @param param max_recent_unlocks max number of recent unlocks to get; an opt_arg
#' @param param critical_items_pct percentage threshold for critical items; an opt_arg
#' @param time_message print a time message
#' @param print_url print the URL string being used, useful for debugging
#'
#' @return a list of lists, one item for each "data unit"
#'
get_wk_data <- function(api_key,
                        api_ver = API_VER, cache_dir = NULL,
                        max_recent_unlocks = 100, critical_items_pct = 75,
                        time_message = TRUE, print_url = TRUE) {

  cat("Extracting WaniKani data...", "\n")

  t0 <- lubridate::now()

  # First check to see if there is cached data
  # If so, read the data from the cache
  # set cache_dir <- NULL to force re-read
  if (!is.null(cache_dir)) {
    cache_filename <- file.path(cache_dir, paste0(api_key, ".rds"))
    if (file.exists(cache_filename)) {
      results <- readRDS(cache_filename)
      t1 <- now()
      print_time_message(paste0(cache_filename, " from cache"), t0, t1)
      return(results)
    }
  }

  # Get the data from the WK API based on the version
  data_names <- AVAILABLE_DATA[[api_ver]]
  # Construct an optional args list based on the data type
  # most will just be empty strings
  opt_args <- rep("", length(data_names))
  opt_args[which(data_names == "recent-unlocks")] <- max_recent_unlocks
  opt_args[which(data_names == "critical-items")] <- critical_items_pct


  # Now read the data
  results <- purrr::pmap(list(data_names, opt_args), function(nm, oarg) {
    get_wk_data_by_name(data_name = nm, api_key = api_key, api_ver = api_ver,
                        opt_arg = oarg, time_message = time_message, print_url = print_url)
  })
  names(results) <- stringr::str_replace(data_names, "-", "_")

  t1 <- lubridate::now()
  print_time_message("All Data", t0, t1)

  results
}


print_time_message <- function(data_name, t0, t1) {
  dtime_sec <- signif(as.numeric(t1 - t0), 3)
  cat(paste0("Extracted ", data_name, " in ", dtime_sec, " seconds", "\n"))
}
