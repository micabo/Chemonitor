# Factory ----------------------------------------------------------------------

chemonitor_analysis_types <- c("default", "scatter")


make_chemonitor_analysis <- function(d, type = chemonitor_analysis_types) {
  stopifnot(is.data.frame(d))
  stopifnot(is.character(type))
  switch(type[[1]],
    default = chemonitor_base(d),
    scatter = chemonitor_scatter(d),
    stop("Unknown type")
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

chemonitor_vars <- c(
  "product",
  "production_process",
  "production_line",
  "batch_id",
  "batch_stage",
  "batch_production_date",
  "batch_lineage",
  "result_name",
  "result_value",
  "result_unit",
  "result_loq"
)


chemonitor_base <- function(d) {
  if (is_chemonitor_base(d)) {
    return(d)
  }
  stopifnot(is.data.frame(d), nrow(d) > 0)
  missing_vars <- chemonitor_vars[!chemonitor_vars %in% names(d)]
  if (length(missing_vars) > 0) {
    stop("Missing data columns: ", paste(missing_vars, collapse = ", "))
  }

  # data conversions and warnings
  # TODO
  # - ensure proper date format
  # - ensure stage is a factor
  # - ensure LOQ is a factor

  # select only relevant variables and introduce a row_id
  d <- d[chemonitor_vars]
  d$row_id <- 1:nrow(d)
  structure(d, class = c("chemonitor_base", class(d)))
}


is_chemonitor_base <- function(x) {
  inherits(x, "chemonitor_base")
}


#' @export
get_categorical_vars.chemonitor_base <- function(obj, ...) {
  c(
    "batch_stage",
    "result_name",
    "production_process",
    "production_line",
    "batch_lineage"
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

  p <- category_plot(obj, "batch_stage", "result_value",
    order = "batch_production_date",
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


# Scatter ----------------------------------------------------------------------
chemonitor_scatter <- function(d) {
  if (is_chemonitor_scatter(d)) {
    return(d)
  }
  if (!is_chemonitor_base(d)) {
    d <- chemonitor_base(d)
  }
  structure(d, class = c("chemonitor_scatter", class(d)))
}


is_chemonitor_scatter <- function(x) {
  inherits(x, "chemonitor_scatter")
}


#' @export
to_plot.chemonitor_scatter <- function(obj, color_by = NULL, ...) {
  if (!is.null(color_by)) {
    obj$color <- do.call(combine_factors, obj[color_by])
    color_by <- "color"
  }

  p <- scatter_plot(obj, "batch_production_date", "result_value",
    colour = color_by,
    key = "row_id"
  )

  p <- plotly::event_register(p, "plotly_selected")
  p <- plotly::layout(p, dragmode = "select")
}
