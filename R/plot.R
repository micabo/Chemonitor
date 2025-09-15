create_aes <- function(x, y,
                       order = NULL, colour = NULL, shape = NULL,
                       text = NULL, key = NULL) {
  plot_aes <- aes(
    x = .data[[x]],
    y = .data[[y]],
    order = .data[[order]],
    colour = .data[[colour]],
    shape = .data[[shape]],
    text = .data[[text]],
    key = .data[[key]]
  )

  if (is.null(order)) plot_aes$order <- NULL
  if (is.null(colour)) plot_aes$colour <- NULL
  if (is.null(shape)) plot_aes$shape <- NULL
  if (is.null(text)) plot_aes$text <- NULL
  if (is.null(key)) plot_aes$key <- NULL

  return(plot_aes)
}


base_plot <- function(data) {
  stopifnot(is.data.frame(data))
  ggplot(data)
}


add_points <- function(g, x, y, order = NULL, colour = NULL, shape = NULL, text = NULL, key = NULL) {
  stopifnot(is_ggplot(g))
  plot_aes <- create_aes(x, y, order = NULL, colour = colour, shape = shape, text = text, key = key)
  g + geom_point(mapping = plot_aes)
}


add_ordered_points <- function(g, x, y, order = NULL, colour = NULL, shape = NULL, text = NULL, key = NULL) {
  stopifnot(is_ggplot(g))
  plot_aes <- create_aes(x, y, order, colour, shape, text, key)
  g + geom_point(mapping = plot_aes, position = position_ordered(0.1))
}


add_limits <- function(g, show_limits = FALSE, limit_low = NULL, limit_high = NULL) {
  stopifnot(is_ggplot(g))
  if (!show_limits) {
    return(g)
  }
  # TODO:
  # - add limit style
  low <- if (!is.null(limit_low)) {
    geom_hline(mapping = aes(yintercept = .data[[limit_low]]))
  }
  high <- if (!is.null(limit_high)) {
    geom_hline(mapping = aes(yintercept = .data[[limit_high]]))
  }
  g + low + high
}


add_facetting <- function(g, facet = NULL) {
  stopifnot(is_ggplot(g))
  if (!is.null(facet)) {
    g + facet_wrap(facet, scales = "free_x")
  } else {
    g
  }
}


add_titles <- function(g, title = NULL, x_title = NULL, y_title = NULL) {
  stopifnot(is_ggplot(g))
  if (!is.null(title)) {
    g <- g + ggtitle(title)
  }
  if (!is.null(x_title)) {
    g <- g + xlab(x_title)
  }
  if (!is.null(y_title)) {
    g <- g + ylab(y_title)
  }
  return(g)
}


add_theme <- function(g, ...) {
  stopifnot(is_ggplot(g))
  # g + theme_bw() + theme(plot.title = element_text(hjust = 0.5))
  g + theme_bw()
}


scatter_plot <- function(data, x, y,
                         colour = NULL, shape = NULL, facet = NULL,
                         show_limits = FALSE, limit_low = NULL, limit_high = NULL,
                         text = NULL, key = NULL,
                         title = NULL, x_title = NULL, y_title = NULL, ...) {
  g <- base_plot(data) |>
    add_points(x, y, colour = colour, shape = shape, text = text, key = key) |>
    add_limits(show_limits, limit_low, limit_high) |>
    add_facetting(facet) |>
    add_titles(title, x_title, y_title) |>
    add_theme()

  p <- plotly::ggplotly(g, ...)
  return(p)
}


category_plot <- function(data, x, y,
                          order = NULL, colour = NULL, shape = NULL,
                          facet = NULL,
                          show_limits = FALSE, limit_low = NULL, limit_high = NULL,
                          text = NULL, key = NULL,
                          title = NULL, x_title = NULL, y_title = NULL, ...) {
  g <- base_plot(data) |>
    add_ordered_points(x, y, order, colour, shape, text, key) |>
    add_limits(show_limits, limit_low, limit_high) |>
    add_facetting(facet) |>
    add_titles(title, x_title, y_title) |>
    add_theme()

  p <- plotly::ggplotly(g, ...)
  return(p)
}
