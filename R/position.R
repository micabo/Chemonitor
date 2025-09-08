jitter_ordered <- function(x, y, order_by = y, half_width = 0.5) {
  stopifnot(is.numeric(x))
  stopifnot((is.numeric(y) || is.Date(y)) && length(y) == length(order_by))
  stopifnot(is.numeric(half_width))
  if (length(half_width) > 1) {
    warning("Multiple half_widths specified, will use only first element")
    half_width <- half_width[[1]]
  }

  order_index <- order(order_by, na.last = NA)
  if (length(order_index) == 1) {
    x_new <- if (length(x) == 1) x else x[[order_index]]
  } else {
    delta <- seq(-half_width, half_width, length.out = length(order_index))
    order_by[order_index] <- delta
    x_new <- x + order_by
  }
  return(data.frame(x = x_new, y = y))
}


position_ordered <- function(width) {
  ggproto(NULL, PositionOrdered,
    width = width
  )
}


PositionOrdered <- ggproto("PositionOrdered", Position,
  required_aes = c("x", "y", "order"),
  setup_params = function(self, data) {
    list(width = self$width)
  },
  compute_panel = function(self, data, params, scales) {
    compute_ordered(data, params$width)
  }
)


compute_ordered <- function(data, width) {
  x_jittered <- double(length(data$x))
  y_jittered <- double(length(data$y))

  for (category in levels(droplevels(factor(data$x)))) {
    filter <- data$x == category
    jittered <- jitter_ordered(data$x[filter], data$y[filter], data$order[filter], width)
    x_jittered[filter] <- jittered$x
    y_jittered[filter] <- jittered$y
  }

  .x <- x_jittered - data$x
  .y <- y_jittered - data$y

  transform_position(
    data,
    function(.) . + .x,
    function(.) . + .y
  )
}
