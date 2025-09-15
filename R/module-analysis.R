analysis_ui <- function(id, config) {
  color_vars <- get_categorical_vars(config$data[[1]])
  tags <- tagList(
    create_select_input(
      inputId = NS(id, "analysis_type"),
      label = to_title("analysis_type"),
      choices = chemonitor_analysis_types
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
    d_analysis <- reactive({
      make_chemonitor_analysis(data(), type = input$analysis_type)
    })

    output$plot <- plotly::renderPlotly({
      to_plot(d_analysis(), color_by = input$color_by)
    })

    output$selection_table <- DT::renderDataTable(plot_selection(d_analysis()),
      options = list(pageLength = 10, autoWidth = TRUE, scrollX = TRUE)
    )
  })
}
