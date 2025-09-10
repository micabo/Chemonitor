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
