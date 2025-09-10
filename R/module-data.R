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


read_custom_data_file <- function(filepath) {
  # DUMMY
  get_dummy_data()
}


write_custom_data_file <- function(x, filepath) {
  write.csv(x, filepath)
}
