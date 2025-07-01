\name{import_csv}
\alias{import_csv}
\title{Import a CSV File as a Tibble}
\description{
Imports a CSV file from a specified file path and returns it as a tibble.
The function automatically detects and skips comment or metadata lines at the start of the file.
}
\usage{
import_csv(filepath)
}
\arguments{
  \item{filepath}{A character string specifying the path to the CSV file to be imported.}
}
\details{
This function checks if the file exists before attempting to import.
If the first line is a comment (starts with '#') or does not contain commas, it skips that line during import.
The returned object is a tibble for easy manipulation with the tidyverse suite of packages.
}
\value{
A tibble containing the data read from the CSV file.
}
\examples{
# Import a CSV file from a relative path
data <- import_csv("data/sample.csv")

# View the imported data
print(data)
}