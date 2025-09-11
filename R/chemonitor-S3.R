# Factory ----------------------------------------------------------------------

make_chemonitor_analysis <- function(d, type = c("default")) {
  stopifnot(is.data.frame(d))
  stopifnot(is.character(type))
  switch(type[[1]],
    default = chemonitor_base(d)
  )
}


# Generics ---------------------------------------------------------------------

#' @export
to_plot <- function(obj, ...) {
  UseMethod("to_plot")
}

#' @export
plot_selection <- function(obj, ...) {
  UseMethod("plot_selection")
}

#' @export
get_categorical_vars <- function(obj, ...) {
  UseMethod("get_categorical_vars")
}

#' @export
get_var_choices <- function(obj, var, ...) {
  UseMethod("get_var_choices")
}


# Base Class -------------------------------------------------------------------

CHEMONITOR_VARS <- c(
  "product",
  "stage",
  "batch_id",
  "result_name",
  "result_value",
  "result_unit",
  "result_loq",
  "production_date",
  "production_line",
  "production_process" # ,
  # "lineage"
)


chemonitor_base <- function(d) {
  if (is_chemonitor_base(d)) {
    return(d)
  }
  stopifnot(is.data.frame(d), nrow(d) > 0)
  if (!all(CHEMONITOR_VARS %in% names(d))) {
    stop("Missing data columns: ", paste(CHEMONITOR_VARS[!CHEMONITOR_VARS %in% names(d)]))
  }

  # data conversions and warnings
  # TODO ...

  # select only relevant variables and introduce a row_id
  d <- d[CHEMONITOR_VARS]
  d$row_id <- 1:nrow(d)
  structure(d, class = c("chemonitor_base", class(d)))
}


is_chemonitor_base <- function(x) {
  inherits(x, "chemonitor_base")
}


#' @export
get_categorical_vars.chemonitor_base <- function(obj, ...) {
  c(
    "stage",
    "result_name",
    "production_line",
    "production_process" # ,
    # "lineage"
  )
}


#' @export
get_var_choices.chemonitor_base <- function(obj, var, ...) {
  stopifnot(is.character(var) || !is.na(var))
  stopifnot(var %in% names(obj))
  v <- obj[[var]]
  if (is.factor(v)) {
    v <- levels(droplevels(v))
  } else if (is.Date(v)) {
    v <- range(v, na.rm = TRUE)
  } else {
    v <- unique(v)
    # can we parse v as numeric?
    v_num <- suppressWarnings(as.numeric(v))
    if (any(is.na(v_num))) {
      # not parsable as numeric, sort alphabetically
      v <- sort(v)
    } else {
      # parsable as numeric, sort by numerical value
      v <- v[order(v_num)]
    }
  }
  return(v)
}


#' @export
to_plot.chemonitor_base <- function(obj, color_by = NULL, ...) {
  if (!is.null(color_by)) {
    obj$color <- do.call(combine_factors, obj[color_by])
    color_by <- "color"
  }

  p <- category_plot(obj, "stage", "result_value",
    order = "production_date",
    colour = color_by,
    key = "row_id"
  )

  p <- plotly::event_register(p, "plotly_selected")
  p <- plotly::layout(p, dragmode = "select")
}


#' @export
plot_selection.chemonitor_base <- function(obj, ...) {
  click_data <- plotly::event_data("plotly_selected", source = "A")
  if (is.null(click_data)) {
    return(obj)
  }

  selected_data <- dplyr::filter(obj, .data$row_id %in% click_data$key)
  if (nrow(selected_data) == 0) selected_data <- obj

  return(selected_data)
}
