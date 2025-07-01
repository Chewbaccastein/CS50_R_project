# library(tidyverse)

import_csv <- function(filepath) {
  if (!file.exists(filepath)) {
    stop("File does not exist: ", filepath)
  }

  first_line <- readLines(filepath, n=1, warn=FALSE)
  comment <- grepl("^#", first_line) || !grepl(",", first_line)


  data <- read_csv(
    filepath,
    skip = if (comment) 1 else 0,
    show_col_types=FALSE
    )

  if (ncol(data) == 0 ) {
    stop("The CSV file has no columns.")
  }

  return(as_tibble(data))

}
