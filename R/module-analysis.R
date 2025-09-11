analysis_ui <- function(id, config) {
  color_vars <- get_categorical_vars(config$data[[1]])
  tags <- tagList(
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
