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
      bslib::accordion(
        bslib::accordion_panel("Data", data_ui("product", config)),
        bslib::accordion_panel("Filter", filter_ui("filter", config)),
        bslib::accordion_panel("Analysis", analysis_ui("analysis", config))
      )
      # tabsetPanel(
      #   type = "tabs",
      #   tabPanel("Data", data_ui("product", config)),
      #   tabPanel("Filter", filter_ui("filter", config)),
      #   tabPanel("Analysis", analysis_ui("analysis", config))
      # )
    ),
    shinyjs::useShinyjs(),
    tags$style(HTML("
      .datatables thead th {
        font-size: 10px;
        padding: 4px 4px;
      }

      .datatables tbody th,
      .datatables tbody td {
        font-size: 10px;
        padding: 2px 4px;
        line-height: 1.2;
      }
    ")),
    analysis_out("analysis")
  )
  return(ui)
}


chemonitor_server <- function(config) {
  # deconstruct config if necessary
  data <- config$data

  server <- function(input, output, session) {
    d_product <- data_server("product", data)
    d_plot_data <- filter_server("filter", d_product)
    analysis_server("analysis", d_plot_data)
  }
  return(server)
}
