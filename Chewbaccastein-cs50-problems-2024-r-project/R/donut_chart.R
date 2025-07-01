
donut_chart <- function(data, category_col, value_col){
  if (!is.data.frame(data)) {
    stop("Input must be a data frame/tibble.")
  }

  if (!category_col %in% names(data)) {
    stop(paste0("Column ", category_col, " not found"))
  }

  if (!value_col %in% names(data)) {
    stop(paste0("Column ", value_col, " not found"))
  }

  data <- data |>
    dplyr::mutate(
      fraction = .data[[value_col]] / sum(.data[[value_col]]),
      ymax = cumsum(fraction),
      ymin = dplyr::lag(ymax, default=0),
      labelPosition = (ymax + ymin) / 2,
      label = paste0(.data[[category_col]], " (", round(fraction * 100), "%)")
    )


  ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=.data[[category_col]])) +
    geom_rect(color = "white") +
    geom_label(x=3.5, aes(y=labelPosition, label=label), size=2) +
    scale_fill_viridis_d(option = "plasma") +
    coord_polar(theta="y") +
    xlim(c(2,4)) +
    theme_void() +
    theme(legend.position = "none")
}