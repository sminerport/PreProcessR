#' Calculate Interquartile Range (IQR) and Identify Outliers
#'
#' This function calculates the interquartile range (IQR) for a specified column
#' in a data frame and identifies any outliers. It also returns the first and third
#' quartiles, and the lower and upper bounds used for outlier detection.
#'
#' @param data A data frame containing the data.
#' @param column The name of the column for which to calculate the IQR.
#'
#' @return A list with six elements: Column (the name of the column),
#'         Q1 (first quartile), Q3 (third quartile), IQR (the interquartile range),
#'         Lower Bound, Upper Bound, and Outliers (vector of outlier values).
#' @importFrom stats quantile
#' @export
#'
#' @examples
#' calculateIQR(mtcars, "mpg")
#'
#' # To apply calculateIQR to all numeric columns in a dataframe:
#' numeric_columns <- sapply(mtcars, is.numeric)
#' IQR_results <- lapply(names(mtcars)[numeric_columns], function(col) calculateIQR(mtcars, col))
#'

calculateIQR <- function(data, column) {
    Q1 <- quantile(data[[column]], 0.25, names = FALSE)
    Q3 <- quantile(data[[column]], 0.75, names = FALSE)
    IQR <- Q3 - Q1

    lower_bound <- Q1 - 1.5 * IQR
    upper_bound <- Q3 + 1.5 * IQR

    lower_outliers <- data[[column]][data[[column]] < lower_bound]
    upper_outliers <- data[[column]][data[[column]] > upper_bound]

    return(
        list(
            Column = column,
            Q1 = Q1,
            Q3 = Q3,
            IQR = IQR,
            Lower_Bound = lower_bound,
            Upper_Bound = upper_bound,
            Lower_Half_Outliers = sort(lower_outliers),
            Upper_Half_Outliers = sort(upper_outliers)
        )
    )
}

#' Estimate Optimal Lambda for Box-Cox Transformation
#'
#' This function estimates the optimal lambda parameter for the Box-Cox transformation
#' of a specified column in a dataset. It requires all values in the column to be positive.
#'
#' @param data A data frame containing the data.
#' @param column_name The name of the column for which to estimate lambda.
#'
#' @return The estimated lambda value for the Box-Cox transformation.
#' @importFrom MASS boxcox
#' @importFrom stats as.formula
#' @export
#'
#' @examples
#' data(iris)
#' lambda_value <- estimateBoxCoxLambda(iris, "Sepal.Length")
#' print(lambda_value)

# Function to estimate lambda for Box-Cox transformation
estimateBoxCoxLambda <- function(data, column_name) {
    # Check if the column exists in the dataset
    if (!column_name %in% names(data)) {
        stop("Column not found in the dataset.")
    }

    # Check for non-positive values
    if (any(data[[column_name]] <= 0)) {
        stop(
            "Non-positive values found. Box-Cox transformation requires all values to be positive."
        )
    }

    # Estimate lambda using boxcox
    formula <- as.formula(paste(column_name, "~ 1"))
    bc_result <- boxcox(formula, data = data, plotit = FALSE)

    # Find the lambda corresponding to the maximum log-likelihood
    optimal_lambda <- bc_result$x[which.max(bc_result$y)]

    return(optimal_lambda)
}

#' Apply Box-Cox Lambda Estimation to All Numeric Columns
#'
#' This function applies the Box-Cox lambda estimation to all numeric columns
#' in a given dataset. It returns a named vector of lambda values, with NA for
#' columns where the estimation is not applicable or fails.
#'
#' @param data A data frame containing the data.
#'
#' @return A named vector of estimated lambda values for each numeric column.
#' @export
#'
#' @examples
#' if (requireNamespace("mlbench", quietly = TRUE)) {
#'     data(Glass, package = "mlbench")
#'     lambda_values <- applyBoxCoxToAll(Glass)
#'     print(lambda_values)
#' }

# Apply estimateBoxCoxLambda to all numeric columns in the Glass dataset
applyBoxCoxToAll <- function(data) {
    # Filter only numeric columns
    numeric_columns <- sapply(data, is.numeric)
    numeric_data <- data[, numeric_columns]

    # Apply the function to each numeric column
    sapply(names(numeric_data), function(col) {
        tryCatch({
            estimateBoxCoxLambda(data, col)
        }, warning = function(w) {
            NA  # Return NA in case of non-positive values or other warnings
        }, error = function(e) {
            NA  # Return NA in case of errors
        })
    })
}

#' Create Histograms for Numeric Columns
#'
#' This function creates histograms for all numeric columns in a given dataframe.
#' Optionally, it can create a single plot with a histogram for each numeric column using facets.
#' @param data A dataframe.
#' @param bins Number of bins for the histogram.
#' @param facet Boolean; if TRUE, create a single faceted plot for all histograms.
#' @importFrom ggplot2 ggplot aes_string geom_histogram labs facet_wrap
#' @export
#' @examples
#' createHistograms(mtcars)
#' createHistograms(iris, bins = 20)
#' createHistograms(mtcars, facet = TRUE)
createHistograms <- function(data, bins = 30, facet = FALSE) {
    # Ensure ggplot2 is available
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("ggplot2 must be installed to use createHistograms")
    }

    # Identify numeric columns
    numericCols <- sapply(data, is.numeric)

    if (!facet) {
        # Loop through numeric columns and create histograms
        for (colName in names(data)[numericCols]) {
            p <- ggplot2::ggplot(data, ggplot2::aes_string(x = colName)) +
                ggplot2::geom_histogram(
                    bins = bins,
                    fill = "blue",
                    color = "black"
                ) +
                ggplot2::labs(title = colName)
            print(p)
        }
    } else {
        # Create a single faceted plot for all histograms
        long_data <- reshape2::melt(data[, numericCols])
        p <- ggplot2::ggplot(long_data, ggplot2::aes(x = value)) +
            ggplot2::geom_histogram(bins = bins,
                                    fill = "blue",
                                    color = "black") +
            ggplot2::facet_wrap(~ variable, scales = "free") +
            ggplot2::labs(x = "Value", y = "Frequency") +
            ggplot2::theme_minimal()
        print(p)
    }
}

#' Create Bar Charts for Categorical Columns
#'
#' This function creates bar charts for all categorical columns in a given dataframe.
#' Optionally, it can create a single plot with a bar chart for each categorical column using facets.
#'
#' @param data A dataframe containing the data to be plotted.
#' @param facet Logical; if TRUE, create a single faceted plot for all bar charts.
#'              If FALSE, create individual plots for each categorical column. Defaults to FALSE.
#'
#' @importFrom ggplot2 ggplot aes_string geom_bar labs facet_wrap theme_minimal
#' @importFrom reshape2 melt
#' @import reshape2
#'
#' @return An invisible list of ggplot objects if `facet` is FALSE. If `facet` is TRUE,
#'         a single ggplot object is returned. Each plot represents the frequency
#'         distribution of a categorical variable in the dataframe.
#'
#' @examples
#' createBarCharts(mtcars, facet = FALSE)
#' createBarCharts(iris, facet = TRUE)
#'
#' @export

createBarCharts <- function(data, facet = FALSE) {
    # Ensure ggplot2 and reshape2 are available
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("ggplot2 must be installed to use createBarCharts")
    }
    if (!requireNamespace("reshape2", quietly = TRUE)) {
        stop("reshape2 must be installed to use createBarCharts")
    }

    # Identify categorical columns
    categoricalCols <-
        sapply(data, is.factor) | sapply(data, is.character)

    if (!facet) {
        # Loop through categorical columns and create bar charts
        for (colName in names(data)[categoricalCols]) {
            p <- ggplot2::ggplot(data, ggplot2::aes_string(x = colName)) +
                ggplot2::geom_bar(fill = "blue", color = "black") +
                ggplot2::labs(
                    title = paste("Distribution of", colName),
                    x = colName,
                    y = "Frequency"
                ) +
                ggplot2::theme_minimal()
            print(p)
        }
    } else {
        # Create a single faceted plot for all bar charts
        # Melt the data into long format for faceting
        long_data <-
            reshape2::melt(data, measure.vars = names(data)[categoricalCols])
        p <- ggplot2::ggplot(long_data, ggplot2::aes(x = value)) +
            ggplot2::geom_bar(fill = "blue", color = "black") +
            ggplot2::facet_wrap( ~ variable, scales = "free_x") +
            ggplot2::labs(x = "Category", y = "Frequency") +
            ggplot2::theme_minimal()
        print(p)
    }
}

#' Create Boxplots for Numeric Columns
#'
#' This function creates a boxplot for each numeric column in a data frame.
#' Optionally, it can create a single plot with a boxplot for each numeric column using facets.
#'
#' @param data A data frame containing the data.
#' @param facet Logical; if TRUE, create a single faceted plot for all boxplots.
#' @return A list of ggplot objects representing the boxplots of the numeric columns, or a single ggplot object if faceting.
#' @importFrom ggplot2 ggplot aes_string geom_boxplot labs theme_minimal facet_wrap
#' @importFrom reshape2 melt
#' @export
#'
#' @examples
#' createBoxplots(mtcars)
#' createBoxplots(mtcars, facet = TRUE)

createBoxplots <- function(data, facet = FALSE) {
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("ggplot2 must be installed to use createBoxplots")
    }

    # Identify numeric columns
    numericCols <- sapply(data, is.numeric)

    if (!facet) {
        # Loop through numeric columns and create boxplots
        plots <-
            lapply(names(data)[numericCols], function(colName) {
                ggplot2::ggplot(data, ggplot2::aes_string(y = colName)) +
                    ggplot2::geom_boxplot() +
                    ggplot2::labs(title = paste("Boxplot of", colName),
                                  y = colName) +
                    ggplot2::theme_minimal()
            })

        # Print all plots
        for (p in plots) {
            print(p)
        }
    } else {
        # Create a single faceted plot for all boxplots
        long_data <- reshape2::melt(data[, numericCols])
        p <- ggplot2::ggplot(long_data, ggplot2::aes(y = value)) +
            ggplot2::geom_boxplot() +
            ggplot2::facet_wrap( ~ variable, scales = "free_y") +
            ggplot2::labs(x = "Variable", y = "Value") +
            ggplot2::theme_minimal()
        print(p)
    }
}


#' Create Boxplot for a Numeric Column by Categorical Group
#'
#' This function creates boxplots for a specified numeric column in a data frame,
#' distributed by a specified categorical variable. It's useful for visualizing
#' the distribution of the numeric variable across different categories.
#'
#' @param data A data frame containing the data.
#' @param numeric_column The name of the numeric column to plot.
#' @param category_column The name of the categorical column to group by.
#'
#' @return A ggplot object representing the boxplot of the numeric column, grouped by the categorical variable.
#' @importFrom ggplot2 ggplot aes_string geom_boxplot labs theme_minimal
#' @export
#'
#' @examples
#' # Assuming 'iris' dataset and 'Species' as the categorical variable
#' createBoxplotByCategory(iris, "Sepal.Length", "Species")

createBoxplotByCategory <-
    function(data, numeric_column, category_column) {
        if (!requireNamespace("ggplot2", quietly = TRUE)) {
            stop("ggplot2 must be installed to use this function.")
        }

        ggplot2::ggplot(data,
                        ggplot2::aes_string(x = category_column, y = numeric_column)) +
            ggplot2::geom_boxplot() +
            ggplot2::labs(title = paste("Boxplot of", numeric_column, "by", category_column)) +
            ggplot2::theme_minimal()
    }


#' Create Correlation Plot
#'
#' This function creates a correlation plot for all numeric columns in a given dataframe.
#' Non-numeric columns are automatically excluded. The type of plot is determined by
#' the 'plotType' argument. It can create a standard correlation plot, a heatmap with
#' hierarchical clustering, or a pair plot.
#'
#' @param data A dataframe containing the data.
#' @param plotType The type of plot to generate, either "corrplot" or "pairplot".
#' @param hclust Logical, should hierarchical clustering be performed for the heatmap.
#' @importFrom corrplot corrplot
#' @importFrom GGally ggpairs
#' @export
#' @examples
#' data(mtcars)
#' createCorrPlot(mtcars, plotType = "heatmap", hclust = TRUE)
#' createCorrPlot(mtcars, plotType = "pairplot")
createCorrPlot <- function(data, plotType = "corrplot", hclust = FALSE) {
    # Check for required packages
    if (plotType == "pairplot" && !requireNamespace("GGally", quietly = TRUE)) {
        stop("GGally must be installed to create a pair plot.")
    }

    # Remove non-numeric columns
    numeric_data <- data[sapply(data, is.numeric)]

    # Calculate correlations
    correlations <- cor(numeric_data)

    if (plotType == "corrplot") {
        # Existing corrplot code
        corrplot::corrplot(
            correlations,
            order = ifelse(hclust, 'hclust', 'original'),
            tl.cex = 1.6,
            number.cex = 0.9,
            method = "circle",
            type = "lower",
            addCoef.col = "black",
            tl.col = "black",
            tl.srt = 45
        )
    } else if (plotType == "pairplot") {
        # Code to generate a pair plot using GGally::ggpairs or similar
        GGally::ggpairs(data = numeric_data)
    }
}

