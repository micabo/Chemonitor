chemonitor_app <- function(config) {
  shinyApp(
    ui = chemonitor_ui(config),
    server = chemonitor_server(config)
  )
}


chemonitor_ui <- function(config) {
  ui <- bslib::page_sidebar(
    title = div(
      img(src = "assets/logo.png", height = 60),
      paste("chemonitor App", config$version)
    ),
    sidebar = bslib::sidebar(
      width = 300,
      tabsetPanel(
        type = "tabs",
        tabPanel("Data", create_data_ui(config)),
        tabPanel("Filter", create_filter_ui(config)),
        tabPanel("Analysis", create_analysis_ui(config))
      )
    ),
    shinyjs::useShinyjs(),
    tags$style(HTML("
      .datatables {
        font-size: 10px;
      }
    ")),
    plotly::plotlyOutput("plot"),
    DT::DTOutput("selection_table")
  )
  return(ui)
}


chemonitor_server <- function(config) {
  # deconstruct config if necessary
  data <- config$data

  server <- function(input, output, session) {
    d_selected_dataset <- reactive({
      req(input$dataset)
      data[[input$dataset]]
    })

    observeEvent(d_selected_dataset(), {
      update_select_input(session, "stage", get_var_choices(d_selected_dataset(), "stage"))
      update_filter_ui(session, d_selected_dataset(), reset = TRUE)
    })

    d_selected_stage <- reactive({
      req(input$stage)
      dplyr::filter(d_selected_dataset(), .data$stage == input$stage)
    })

    observeEvent(d_selected_stage(), {
      update_select_input(session, "result_name", get_var_choices(d_selected_stage(), "result_name"))
    })

    d_selected_result <- reactive({
      req(input$result_name)
      dplyr::filter(d_selected_stage(), .data$result_name == input$result_name)
    })

    observeEvent(d_selected_result(), {
      update_filter_ui(session, d_selected_result(), reset = FALSE)
    })

    d_plot_data <- reactive({
      tmp <- d_selected_result()
      row_selection <- TRUE

      # filter for data in date range
      row_selection <- date_in_range(tmp$production_date, get_var_choices(tmp, "production_date"))

      # apply categorical filters
      filter_ids <- get_categorical_vars(tmp)
      for (id in filter_ids) {
        selected_vals <- input[[id]]
        if (is.null(selected_vals) || length(selected_vals) == 0) next
        row_selection <- row_selection & ((tmp[[id]] %in% selected_vals) | is.na(tmp[[id]]))
      }

      tmp <- tmp[row_selection, ]
      make_chemonitor_analysis(tmp, "default")
    })

    output$plot <- plotly::renderPlotly({
      to_plot(d_plot_data(),
        color_by = input$color_by
      )
    })

    d_plot_data_selection <- reactive(plot_selection(d_plot_data()))

    output$selection_table <- DT::renderDataTable(d_plot_data_selection(),
      options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE)
    )
  }
  return(server)
}


create_data_ui <- function(config) {
  d <- config$data[[1]]
  tags <- tagList(
    sidebarHeader("Data Selection"),
    create_select_input(
      inputId = "dataset",
      choices = names(config$data)
    ),
    create_select_input(
      inputId = "product",
      choices = get_var_choices(d, "product")
    ),
    create_select_input(
      inputId = "stage",
      choices = get_var_choices(d, "stage")
    ),
    create_select_input(
      inputId = "result_name",
      choices = get_var_choices(d, "result_name")
    )
  )
  return(tags)
}


create_filter_ui <- function(config) {
  d <- config$data[[1]]
  date_range <- get_var_choices(d, "production_date")
  filter_ids <- get_categorical_vars(d)
  tags <- tagList(
    sidebarHeader("Apply Filter"),
    dateRangeInput(
      inputId = "production_date",
      label = to_title("production_date"),
      format = "dd.mm.yyyy",
      weekstart = 1,
      start = date_range[[1]],
      end = date_range[[2]]
    ),
    purrr::map(filter_ids, function(.) {
      create_multiselect_input(., choices = get_var_choices(d, .))
    })
  )
  return(tags)
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


create_analysis_ui <- function(config) {
  color_vars <- get_categorical_vars(config$data[[1]])
  tags <- tagList(
    sidebarHeader("Analysis Options"),
    create_select_input(
      inputId = "analysis_type",
      choices = c("scatter", "boxplot", "depletion", "correlation")
    ),
    checkboxInput(
      inputId = "show_limits",
      label = to_title("show_limits"),
      value = FALSE
    ),
    create_multiselect_input(
      inputId = "color_by",
      choices = color_vars,
      selected = color_vars[[1]]
    )
  )
  return(tags)
}


update_analysis_ui <- function() {
  # do nothing
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
