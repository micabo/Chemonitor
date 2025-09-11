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
  # simulate production data
  # expand.grid(list(product = c("A", "B"), stage = 1:5))

  # d <- data.frame(
  #   product = "Probablyl",
  #   stage = sample(LETTERS[1:5], 100, replace = TRUE),
  #   batch_id = stringr::str_pad(seq_len(100), 6, side = "left", pad = "0"),
  #   result_name = "Purity",
  #   result_value = rnorm(100, mean = 95, sd = 1),
  #   result_unit = "%",
  #   result_loq = FALSE,
  #   production_date = lubridate::dmy("07.07.2022") + sample(c(-2, -1, 0, 1, 2), 100, TRUE),
  #   production_line = sample(LETTERS[1:5], 100, replace = TRUE),
  #   production_process = sample(LETTERS[1:5], 100, replace = TRUE)
  #   # lineage = sample(LETTERS[1:5], 100, replace = TRUE)
  # )

  d <- simulate_data_1()

  return(chemonitor_base(d))
}


simulate_data_1 <- function() {
  batch_info <- data.frame(
    product = "Acetotransmyrin",
    stage = rep(1:4, 25),
    batch_id = 1:100,
    production_date = sort(lubridate::dmy("10.09.2025") + sample(1:500, 100, TRUE) - 250),
    production_line = rep(sample(c("P15", "P10", "P5", "X7"), 4), 25)
  )
  batch_info$production_process <- "LSP"
  batch_info$production_process[batch_info$production_line == "X7"] <- "SSP"


  d <- batch_info[order(batch_info$stage, batch_info$production_process, batch_info$production_line), ]

  purity <- d
  purity$result_name <- "Purity"
  purity$result_unit <- "%"
  purity$result_value <- runif(100, 0, 1) + batch_info$stage + 95
  purity$result_loq <- FALSE

  total_imp <- purity
  total_imp$result_name <- "Total Impurities"
  total_imp$result_value <- 100 - purity$result_value
  total_imp$result_loq <- total_imp$result_value < 0.5

  water <- purity
  water$result_name <- "Water (KF)"
  water$result_unit <- "%w/w"
  tmp <- rnorm(100, 0.5)
  water$result_value <- ifelse(tmp < 0.05, 0.05, tmp)
  water$result_loq <- tmp < 0.05

  result_info <- dplyr::bind_rows(purity, total_imp, water)

  my_data <- merge(batch_info, result_info)
}
