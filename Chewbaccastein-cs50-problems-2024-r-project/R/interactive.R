load_csv <- function(path) {

  if (!file.exists(path)) {
    stop("File does not exist: ", path)
  }
  data <- import_csv(path)
  message("CSV file successfully loaded!")
  return(data)
}



get_chart_types <-function() {
  return(c("bar", "donut", "line"))
}


create_chart <- function(data, chart_type, x, y) {
  if (!is.data.frame(data)) {
    stop("Data must be a dataframe.")
  }

  chart_type <- tolower(chart_type)

  if (chart_type == "bar") {
    bar_chart(data, x, y)
  } else if (chart_type == "line") {
    line_chart(data, x, y)
  } else if (chart_type == "donut") {
    donut_chart(data, category_col=x, value_col=y)
  } else{
    stop(paste0("Unknown chart type: ", chart_type))
  }
}

start_chartsmith <- function() {
  cat("Welcome to Chartsmith! \n")

  path <- readline("Enter CSV file path: ")
  data <- load_csv(path)

  if (is.null(data)) {
    stop("data not valid")
  }

  # num_rows <- readline("How many rows would you like to preview? (default: 10): ")
  # num_rows <- as.integer(num_rows)
  # if (is.na(num_rows)) num_rows <- 10
  #
  # print(head(data, n = num_rows))

  print(data)

  types <- get_chart_types()
  cat("\nAvailable chart types:\n", paste0(" - ", types), "\n")
  chart_type <- readline("Choose a chart type: ")

  x_col <- readline("Enter column name for X / Category: ")
  y_col <- readline("Enter column name for Y / Value: ")

  plot <- create_chart(data, chart_type, x_col, y_col)
  print(plot)
}