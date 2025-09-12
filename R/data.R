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

  d <- simulate_data_2()

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


simulate_data_2 <- function() {
  # create batch lineages
  n_lineage <- 25
  batch_w_lineage <- data.frame(
    product = "Acetotransmyrin",
    production_line = sample(c("P15", "P10", "P5", "X7"), n_lineage, TRUE),
    batch_lineage = sample(LETTERS, n_lineage),
    batch_base_production_date = sort(lubridate::dmy("10.09.2025") + sample(1:500, n_lineage, TRUE) - 500),
    batch_base_id = (1:n_lineage) * 10
  )
  batch_w_lineage$production_process <- "LSP"
  batch_w_lineage$production_process[batch_w_lineage$production_line == "X7"] <- "SSP"
  batch_w_lineage$production_process[batch_w_lineage$production_line == "P15" & batch_w_lineage$batch_base_production_date > lubridate::dmy("01.01.2025")] <- "RLSP"


  add_result <- function(data, n_stages, result_name, result_unit, result_function) {
    # add results
    n_row <- nrow(data)
    for (stage in 1:n_stages) {
      data[[paste0("stage-", stage)]] <- result_function(n_row, stage)
    }
    data <- tidyr::pivot_longer(data, cols = tidyr::starts_with("stage"), names_to = "stage", values_to = "result_value")
    data <- tidyr::separate(data, "stage", c("result_name", "batch_stage"))
    data$result_name <- result_name
    data$result_unit <- result_unit
    return(data)
  }

  # purity <- add_result(batch_w_lineage, 4, "Purity", "%a/a", function(n_row, stage) {round(95 + stage + runif(n_row), 2)})
  purity <- add_result(batch_w_lineage, 4, "Purity", "%a/a", function(n_row, stage) {95 + stage})
  purity$result_value <- round(rnorm(nrow(purity), purity$result_value), 2)
  purity$result_value[purity$result_value > 100] <- 100 - (purity$result_value[purity$result_value > 100] - 100)
  purity$result_loq <- FALSE

  water <- add_result(batch_w_lineage, 4, "Water", "%w/w", function(n_row, stage) { 4 - stage })
  water$result_value <- round(rnorm(nrow(water), water$result_value), 2)
  water$result_loq <- water$result_value < 0.05
  water$result_value[water$result_loq] <- 0.05

  data <- dplyr::bind_rows(purity, water)
  data$batch_id <- as.integer(data$batch_base_id) + as.integer(data$batch_stage)
  data$batch_id <- stringr::str_pad(data$batch_id, 8, side = "left", pad = "0")
  data$batch_production_date <- data$batch_base_production_date + as.integer(data$batch_stage)

  data$batch_base_id <- NULL
  data$batch_base_production_date <- NULL
  data$batch_base_id <- NULL

  return(data)
}
