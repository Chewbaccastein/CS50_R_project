library(ggplot2)
library(readr)
library(dplyr)

describe("donut_chart()", {
  it("checks if a chart is created", {
    tmp <- tempfile(fileext = ".csv")
    write.csv(data.frame(x = 1:3, y = c(10,20,30)), tmp, row.names=FALSE)
    data <- import_csv(tmp)

    plot <- donut_chart(data, category_col="x", value_col="y")
    expect_s3_class(plot, "ggplot")
  })

  it("gives an error when columns do not exist", {
    tmp <- tempfile(fileext = ".csv")
    write.csv(data.frame(x = 1:3, y = c(10,20,30)), tmp, row.names=FALSE)
    data <- import_csv(tmp)

    expect_error(donut_chart(data, category_col="nonexistent", value_col="y"))
    expect_error(donut_chart(data, category_col="x", value_col="nonexistent"))
  })

  it("gives an error when no dataframe/tibble exist", {
    expect_error(donut_chart("nonexistingdframe", category_col="x", value_col="y"))
  })
})