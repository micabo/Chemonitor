#' Run Data Getter Function
#'
#' Runs a function to obtain data sets for the app.
#'
#' @param data_getter A function to obtain data
#' @param ... Optional parameters passed to the data_getter function
#'
#' @returns The data sets returned by executing the data_getter function
#' @export
#'
#' @examples run_data_getter(function() stop("Dummy"))
run_data_getter <- function(data_getter, ...) {
  stopifnot(is.function(data_getter))
  tryCatch(
    {
      # connect to data service
      data_getter(...)
    },
    error = function(cnd) {
      # TODO: use package internal data
      warning(paste(
        as.character(cnd),
        "-> Fallback to package internal data."
      ))
      return(get_dummy_data)
    }
  )
}


get_dummy_data <- function() {
  data.frame(
    product = "Probablyl",
    stage = sample(LETTERS[1:5], 100, replace = TRUE),
    batch_id = seq(1:100),
    result_name = "Purity",
    result_value = rnorm(100, mean = 95, sd = 1),
    result_unit = "%",
    result_loq = FALSE,
    production_date = lubridate::dmy("07.07.2022") + sample(c(-2, -1, 0, 1, 2), 100, TRUE),
    production_line = sample(LETTERS[1:5], 100, replace = TRUE),
    lineage = sample(LETTERS[1:5], 100, replace = TRUE)
  ) |> chemonitor_base()
}
