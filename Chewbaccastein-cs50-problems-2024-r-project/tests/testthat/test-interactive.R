library(ggplot2)
library(readr)
library(dplyr)

describe("load_csv()", {
  it("loads a csv file and returns a tibble", {
    tmp <- tempfile(fileext = ".csv")
    write.csv(data.frame(a=1:3, b=4:6), tmp, row.names=FALSE)
    data <- load_csv(tmp)
    expect_s3_class(data, "tbl_df")
  })
  it("gives an error if missing file", {
    expect_error(load_csv("_nonexistentfile_.csv"))
  })
})


describe("get_chart_types()", {
  it("returns a list of available chart types", {
    types <- get_chart_types()
    expect_true("bar" %in% types)
    expect_true("donut" %in% types)
    expect_true("line" %in% types)
  })
})

describe("create_chart()", {
  it("returns a ggplot object for a bar chart", {
    df <- tibble(x=letters[1:3], y=1:3)
    plot <- create_chart(df, chart_type = "bar", x="x", y="y")
    expect_s3_class(plot, "ggplot")
  })
  it("returns a ggplot object for a donut chart", {
    df <- tibble(category_col = c("a", "b", "c"), value_col = 1:3)
    plot <- create_chart(df, chart_type = "donut", x="category_col", y="value_col")
    expect_s3_class(plot, "ggplot")
  })
  it("returns a ggplot object for a line chart", {
    df <- tibble(x=1:3, y=4:6)
    plot <- create_chart(df, chart_type="line", x="x", y="y")
    expect_s3_class(plot, "ggplot")
  })
  it("give an error if unknow chart type is entered", {
    df <- tibble(x = 1:3, y = 4:6)
    expect_error(create_chart(df, chart_type="_unknown_", x="x", y="y"))
  })
})