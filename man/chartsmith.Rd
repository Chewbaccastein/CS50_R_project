\name{chartsmith-package}
\alias{chartsmith}
\title{Chartsmith: Generate Charts from CSV Files Quickly}
\description{Chartsmith is a lightweight and user-friendly R package designed to create basic data visualizations—bar charts, line graphs, and donut plots—directly from CSV files. It streamlines the workflow for users who want fast and clean visual output with minimal setup.
}
\usage{
start_chartsmith()
import_csv(filepath)
bar_chart(data, x, y)
line_chart(data, x, y)
donut_chart(data, category_col, value_col)
\arguments{
  \item{None}{This function does not take arguments.}
}
\value{
This package provides multiple functions that return ggplot2 plots or tibbles depending on user inputs.
}
\examples{
start_chartsmith()

df <- import_csv("example.csv")
bar_chart(df, x = "category", y = "value")
}