library(ggplot2)
library(readr)
library(dplyr)


describe("bar_chart()", {
  it("checks if a chart is created", {
    tmp <- tempfile(fileext = ".csv")
    write.csv(data.frame(x = 1:3, y = c("a","b","c")), tmp, row.names=FALSE)
    data <- import_csv(tmp)

    plot <- bar_chart(data, x="x", y="y")
    expect_s3_class(plot, "ggplot")
  })

  it("gives an error when columns do not exist", {
    tmp <- tempfile(fileext = ".csv")
    write.csv(data.frame(x = 1:3, y = c("a","b","c")), tmp, row.names=FALSE)
    data <- import_csv(tmp)

    expect_error(bar_chart(data, x="nonexistent", y="b"))
    expect_error(bar_chart(data, x="a", y="nonexistent"))
  })

  it("gives an error when no dataframe/tibble exist", {
    expect_error(bar_chart("nonexistingdframe", x="x", y="y"))
  })
})