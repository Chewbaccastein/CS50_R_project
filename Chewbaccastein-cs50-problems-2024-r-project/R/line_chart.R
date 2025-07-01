line_chart <- function(data, x, y) {
  if (!is.data.frame(data)) {
    stop("Input must be a data frame/tibble.")
  }

  if (!(x %in% names(data))) {
    stop(paste0("Column ", x, " not found"))
  }

  if (!(y %in% names(data))) {
    stop(paste0("Column ", y, " not found"))
  }

  if (!is.numeric(data[[y]])) {
    stop("The y column must be numeric.")
  }


  ggplot(data, aes(x = .data[[x]], y = .data[[y]])) +
    geom_line() +
    theme_classic()
}
