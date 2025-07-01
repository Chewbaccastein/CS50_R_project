library(ggplot2)
library(readr)
library(dplyr)

describe("import_csv()", {
  it("imports .csv file and returns a tibble", {
    tmp <- tempfile(fileext = ".csv")
    write.csv(data.frame(x = 1:3, y = c("a","b","c")), tmp, row.names=FALSE)
    data <- import_csv(tmp)
    expect_equal(nrow(data), 3) # checks it has 3 rows
    expect_equal(ncol(data), 2) # checks it has 2 columns
    expect_equal(data$x, c(1,2,3))
    expect_equal(data$y, c("a","b","c"))
  })
  it("returns error on missing file", {
    expect_error(import_csv("nonexistent_file_1234.csv"))
  })
})