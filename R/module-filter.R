filter_ui <- function(id, config) {
  d <- config$data[[1]]
  date_range <- get_var_choices(d, "production_date")
  filter_ids <- get_categorical_vars(d)
  tags <- tagList(
    dateRangeInput(
      inputId = NS(id, "production_date"),
      label = to_title("production_date"),
      format = "dd.mm.yyyy",
      weekstart = 1,
      start = date_range[[1]],
      end = date_range[[2]]
    ),
    purrr::map(filter_ids, function(.) {
      create_multiselect_input(NS(id, .), label = to_title(.), choices = get_var_choices(d, .))
    })
  )
  return(tags)
}


filter_server <- function(id, data) {
  stopifnot(is.reactive(data))
  moduleServer(id, function(input, output, session) {
    observeEvent(data(), {
      update_filter_ui(session, data(), reset = TRUE)
    })


    d_plot_data <- reactive({
      tmp <- req(data())

      # apply filters
      row_selection <- TRUE

      # apply date filter
      req(input$production_date)
      row_selection <- date_in_range(tmp$production_date, get_var_choices(tmp, "production_date"))

      # apply categorical filters
      filter_ids <- get_categorical_vars(tmp)
      for (id in filter_ids) {
        selected_vals <- req(input[[id]])
        if (is.null(selected_vals) || length(selected_vals) == 0) next
        row_selection <- row_selection & ((tmp[[id]] %in% selected_vals) | is.na(tmp[[id]]))
      }

      tmp <- tmp[row_selection, ]
      make_chemonitor_analysis(tmp, "default")
    })

    return(d_plot_data)
  })
}


update_filter_ui <- function(session, data, reset = TRUE) {
  # update date range
  date_range <- get_var_choices(data, "production_date")
  freezeReactiveValue(session$input, "production_date")
  updateDateRangeInput(session, "production_date", start = date_range[[1]], end = date_range[[2]])

  # update filters
  ids <- get_categorical_vars(data)
  for (id in ids) {
    choices <- get_var_choices(data, id)
    update_filter_visibility(id, choices)
    update_multiselect_input(session, id, choices, reset)
  }
}


update_filter_visibility <- function(id, choices) {
  if (length(choices) < 2) {
    shinyjs::hide(id)
  } else {
    shinyjs::show(id)
  }
}
