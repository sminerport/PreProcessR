% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_visualization.R
\name{create_corr_plot}
\alias{create_corr_plot}
\title{Create Correlation Plot}
\usage{
create_corr_plot(data, plotType = "corrplot", hclust = FALSE)
}
\arguments{
\item{data}{A dataframe containing the data.}

\item{plotType}{The type of plot to generate, either "corrplot" or "pairplot".}

\item{hclust}{Logical, should hierarchical clustering be performed for the heatmap.}
}
\description{
This function creates a correlation plot for all numeric columns in a given dataframe.
Non-numeric columns are automatically excluded. The type of plot is determined by
the 'plotType' argument. It can create a standard correlation plot, a heatmap with
hierarchical clustering, or a pair plot.
}
\examples{
data(mtcars)
createCorrPlot(mtcars, plotType = "heatmap", hclust = TRUE)
createCorrPlot(mtcars, plotType = "pairplot")
}
