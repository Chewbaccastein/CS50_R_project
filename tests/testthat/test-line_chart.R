library(ggplot2)
library(readr)
library(dplyr)

describe("line_chart()", {
  it("checks if a chart is created", {
    tmp <- tempfile(fileext = ".csv")
    write.csv(data.frame(x = 1:3, y = 4:6), tmp, row.names=FALSE)
    data <- import_csv(tmp)

    plot <- line_chart(data, x="x", y="y")
    expect_s3_class(plot, "ggplot")
  })

  it("gives an error if y column is non-numberic", {
    tmp <- tempfile(fileext = ".csv")
    write.csv(data.frame(x = 1:3, y = "a", "b", "c"), tmp, row.names=FALSE)
    data <- import_csv(tmp)

    expect_error(line_chart(data, x="x", y="y"))
  })

  it("gives an error when columns do not exist", {
    tmp <- tempfile(fileext = ".csv")
    write.csv(data.frame(x = 1:3, y = 4:6), tmp, row.names=FALSE)
    data <- import_csv(tmp)

    expect_error(line_chart(data, x="nonexistent", y="b"))
    expect_error(line_chart(data, x="a", y="nonexistent"))
  })

  it("gives an error when no dataframe/tibble exist", {
    expect_error(line_chart("nonexistingdframe", x="x", y="y"))
  })
})