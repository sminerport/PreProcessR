% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_visualization.R
\name{create_boxplots}
\alias{create_boxplots}
\title{Create Boxplots for Numeric Data}
\usage{
create_boxplots(
  data,
  numeric_column = NULL,
  category_column = NULL,
  facet = FALSE
)
}
\arguments{
\item{data}{A data frame containing the data.}

\item{numeric_column}{Optionally, the name of a single numeric column to plot. If NULL, boxplots for all numeric columns are created. Defaults to NULL.}

\item{category_column}{Optionally, the name of a categorical column to group the numeric data by. Only used if `numeric_column` is specified. Defaults to NULL.}

\item{facet}{Logical; if TRUE and `numeric_column` is NULL, creates a single faceted plot for all numeric columns. If `numeric_column` is specified and `category_column` is NULL, faceting is ignored. Defaults to FALSE.}
}
\value{
A ggplot object representing the boxplot(s) of the numeric column(s). If multiple plots are generated without faceting, a list of ggplot objects is returned.
}
\description{
This function creates boxplots for numeric data within a dataframe. It can generate boxplots for all numeric columns, or for a specified numeric column, optionally grouped by a categorical column. Faceting is supported to create a single plot with a boxplot for each numeric column.
}
\examples{
# Boxplot for all numeric columns
create_boxplots(mtcars)
# Faceted boxplot for all numeric columns
create_boxplots(mtcars, facet = TRUE)
# Boxplot for a specific column, grouped by a category
create_boxplots(mtcars, numeric_column = "mpg", category_column = "cyl")
# Faceted boxplot for a specific column (facet ignored if category_column is provided)
create_boxplots(mtcars, numeric_column = "mpg", category_column = "cyl", facet = TRUE)
}
