to_title <- function(x) {
  x |>
    stringr::str_replace_all("_", " ") |>
    stringr::str_trim() |>
    stringr::str_squish() |>
    stringr::str_to_title()
}


combine_factors <- function(..., sep = ":") {
  args <- list(...)
  stopifnot(length(args) > 0)

  acc <- factor(args[[1]])
  if (length(args) == 1) {
    return(acc)
  }
  for (f in args[2:length(args)]) {
    levels_f <- levels(factor(f))
    combined_levels <- sapply(levels(acc), function(.) {
      paste(., levels_f, sep = sep)
    })
    acc <- factor(paste(acc, f, sep = sep), levels = combined_levels)
  }
  return(acc)
}


date_in_range <- function(x, date_range) {
  stopifnot(is.Date(x))
  stopifnot(is.Date(date_range), length(date_range) == 2)
  (x >= date_range[[1]] & x <= date_range[[2]]) | is.na(x)
}
