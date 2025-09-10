data_ui <- function(id, config) {
  d <- config$data[[1]]
  tags <- tagList(
    create_select_input(
      inputId = NS(id, "dataset"),
      label = "Data Set",
      choices = c(names(config$data), "Custom")
    ),
    create_select_input(
      inputId = NS(id, "product"),
      label = "Product",
      choices = get_var_choices(d, "product")
    )
  )
  return(tags)
}


data_server <- function(id, data) {
  stopifnot(!is.reactive(data))
  moduleServer(id, function(input, output, session) {
    # root data
    d_selected_dataset <- reactiveVal()

    observeEvent(input$dataset, {
      if (input$dataset == "Custom") {
        showModal(custom_data_modal())
      } else {
        d_selected_dataset(data[[input$dataset]])
      }
    })

    custom_data_modal <- function(failed = FALSE) {
      modalDialog(
        if (failed) {
          div(strong("Invalid dataset", style = "color: red;"))
        },
        span("Select an .xlsx or .csv dataset, which follows the apps data layout"),
        fileInput(NS(id, "custom_data_file"),
          label = "Choose custom dataset",
          accept = c("text/csv", "applications/vns.openxmlformats-officedocument.spreadsheetml.sheet")
        ),
        footer = tagList(
          modalButton("Cancel"),
          downloadButton(NS(id, "custom_data_template"), "Download Template"),
          actionButton(NS(id, "custom_data_ok"), "OK")
        )
      )
    }

    observeEvent(input$custom_data_ok, {
      custom_data <- read_custom_data_file(input$custom_data_file$datapath)
      if (is.null(custom_data)) {
        showModal(custom_data_modal(failed = TRUE))
      } else {
        d_selected_dataset(custom_data)
        removeModal()
      }
    })

    output$custom_data_template <- downloadHandler(
      filename = function() {
        "chemonitor-data-template.csv"
      },
      content = function(filepath) {
        write_custom_data_file(data[[1]], filepath)
      },
      contentType = "text/csv"
    )

    observeEvent(d_selected_dataset(), {
      update_select_input(session, "product", get_var_choices(d_selected_dataset(), "product"))
    })

    d_selected_product <- reactive({
      req(input$product)
      dplyr::filter(d_selected_dataset(), .data$product == input$product)
    })

    return(d_selected_product)
  })
}


filter_ui <- function(id, config) {
  d <- config$data[[1]]
  date_range <- get_var_choices(d, "production_date")
  filter_ids <- get_categorical_vars(d)
  tags <- tagList(
    sidebarHeader("Apply Filter"),
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


analysis_ui <- function(id, config) {
  color_vars <- get_categorical_vars(config$data[[1]])
  tags <- tagList(
    sidebarHeader("Analysis Options"),
    create_select_input(
      inputId = NS(id, "analysis_type"),
      label = to_title("analysis_type"),
      choices = c("scatter", "boxplot", "depletion", "correlation")
    ),
    checkboxInput(
      inputId = NS(id, "show_limits"),
      label = to_title("show_limits"),
      value = FALSE
    ),
    create_multiselect_input(
      inputId = NS(id, "color_by"),
      label = to_title("color_by"),
      choices = color_vars,
      selected = color_vars[[1]]
    )
  )
  return(tags)
}


analysis_out <- function(id) {
  tagList(
    plotly::plotlyOutput(NS(id, "plot")),
    DT::DTOutput(NS(id, "selection_table"))
  )
}


analysis_server <- function(id, data) {
  stopifnot(is.reactive(data))
  moduleServer(id, function(input, output, session) {
    output$plot <- plotly::renderPlotly({
      to_plot(data(),
        color_by = input$color_by
      )
    })

    selection <- reactive(plot_selection(data()))

    output$selection_table <- DT::renderDataTable(selection(),
      options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE)
    )
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



create_select_input <- function(inputId,
                                label = to_title(inputId),
                                choices = NULL,
                                selected = NULL) {
  shinyWidgets::pickerInput(inputId, label, choices, selected, multiple = FALSE)
}


update_select_input <- function(session, inputId, choices,
                                reset = FALSE) {
  current_val <- session$input[[inputId]]
  next_val <- if (!is.null(current_val) && current_val %in% choices) current_val
  if (reset || is.null(next_val)) {
    freezeReactiveValue(session$input, inputId)
    next_val <- NULL # in case of reset
  }
  shinyWidgets::updatePickerInput(session, inputId, choices = choices, selected = next_val)
}


create_multiselect_input <- function(inputId,
                                     label = to_title(inputId),
                                     choices = NULL,
                                     selected = choices,
                                     max_selected = NULL) {
  shinyWidgets::pickerInput(inputId, label, choices, selected,
    multiple = TRUE,
    options = shinyWidgets::pickerOptions(actionsBox = TRUE, maxOptions = max_selected)
  )
}


update_multiselect_input <- function(session, inputId, choices,
                                     reset = FALSE) {
  next_vals <- intersect(session$input[[inputId]], choices)
  if (reset || length(next_vals) == 0 || is.null(next_vals)) {
    freezeReactiveValue(session$input, inputId)
    next_vals <- choices
  }
  shinyWidgets::updatePickerInput(session, inputId,
    choices = choices,
    selected = next_vals
  )
}


sidebarHeader <- h4
sidebarDivider <- tags$hr
