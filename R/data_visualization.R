#' Visualize Missing Data Patterns
#'
#' This function creates visualizations to help understand the patterns of missing data in the dataset.
#' It generates two plots: one showing the proportion of missing data per predictor, and another
#' showing the distribution of missing data across classes.
#'
#' @param data A dataframe representing the dataset.
#' @param missing_proportions A named vector with predictors as names and proportions of missing data as values.
#' @param missing_by_class A table with class labels and counts of missing data occurrences in each class.
#' @import ggplot2
#' @export
#' @examples
#' data("Soybean", package = "mlbench")
#' missing_proportions <- missing_data_proportion(Soybean)
#' missing_by_class <- missing_data_by_class(Soybean, "Class")
#' visualize_missing_data(Soybean, missing_proportions, missing_by_class)
visualize_missing_data <-
    function(data,
             missing_proportions,
             missing_by_class) {
        # Visualize missing data proportions
        ggplot(data = as.data.frame(missing_proportions), aes(x = rownames(missing_proportions), y = V1)) +
            geom_bar(stat = "identity") +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
            labs(x = "Predictor", y = "Proportion of Missing Data", title = "Missing Data Proportions per Predictor")

        # Visualize missing data by class
        ggplot(data = as.data.frame(missing_by_class),
               aes(x = names(missing_by_class), y = missing_by_class)) +
            geom_bar(stat = "identity") +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
            labs(x = "Class", y = "Count of Missing Data", title = "Distribution of Missing Data Across Classes")

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
#' create_histograms(mtcars)
#' create_histograms(iris, bins = 20)
#' create_histograms(mtcars, facet = TRUE)
create_histograms <- function(data, bins = 30, facet = FALSE) {
    # Ensure ggplot2 is available
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("ggplot2 must be installed to use create_histograms")
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
#' create_bar_charts(mtcars, facet = FALSE)
#' create_bar_charts(iris, facet = TRUE)
#'
#' @export

create_bar_charts <- function(data, facet = FALSE) {
    # Ensure ggplot2 and reshape2 are available
    if (!requireNamespace("ggplot2", quietly = TRUE)) {
        stop("ggplot2 must be installed to use create_bar_charts")
    }
    if (!requireNamespace("reshape2", quietly = TRUE)) {
        stop("reshape2 must be installed to use create_bar_charts")
    }

    # Identify categorical columns
    categoricalCols <-
        sapply(data, is.factor) | sapply(data, is.character)

    if (!facet) {
        # Loop through categorical columns and create bar charts
        plots <- list()
        for (colName in names(data)[categoricalCols]) {
            # Exclude NA values for frequency calculation and ordering
            freq <- table(data[[colName]], useNA = "no")
            levels <- names(sort(freq, decreasing = TRUE))
            data[[colName]] <-
                factor(data[[colName]], levels = levels)

            # Create the plot
            p <-
                ggplot2::ggplot(data, ggplot2::aes_string(x = colName)) +
                ggplot2::geom_bar(fill = "blue", color = "black") +
                ggplot2::labs(
                    title = paste("Distribution of", colName),
                    x = colName,
                    y = "Frequency"
                ) +
                ggplot2::theme_minimal() +
                ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
            plots[[colName]] <- p
        }
        return(plots)
    } else {
        # Create a single faceted plot for all bar charts
        # Melt the data into long format for faceting, excluding NA values
        long_data <-
            reshape2::melt(data, measure.vars = names(data)[categoricalCols])
        long_data <- na.omit(long_data)

        # Calculate frequencies for ordering within each facet
        long_data$value <-
            reorder(long_data$value, long_data$variable, function(x)
                - length(x))

        p <- ggplot2::ggplot(long_data, ggplot2::aes(x = value)) +
            ggplot2::geom_bar(fill = "blue", color = "black") +
            ggplot2::facet_wrap( ~ variable, scales = "free_x") +
            ggplot2::labs(x = "Category", y = "Frequency") +
            ggplot2::theme_minimal() +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
        return(p)
    }
}

#' Create Boxplots for Numeric Data
#'
#' This function creates boxplots for numeric data within a dataframe. It can generate boxplots for all numeric columns, or for a specified numeric column, optionally grouped by a categorical column. Faceting is supported to create a single plot with a boxplot for each numeric column.
#'
#' @param data A data frame containing the data.
#' @param numeric_column Optionally, the name of a single numeric column to plot. If NULL, boxplots for all numeric columns are created. Defaults to NULL.
#' @param category_column Optionally, the name of a categorical column to group the numeric data by. Only used if `numeric_column` is specified. Defaults to NULL.
#' @param facet Logical; if TRUE and `numeric_column` is NULL, creates a single faceted plot for all numeric columns. If `numeric_column` is specified and `category_column` is NULL, faceting is ignored. Defaults to FALSE.
#' @return A ggplot object representing the boxplot(s) of the numeric column(s). If multiple plots are generated without faceting, a list of ggplot objects is returned.
#' @importFrom ggplot2 ggplot aes geom_boxplot labs theme_minimal facet_wrap
#' @importFrom reshape2 melt
#' @export
#'
#' @examples
#' # Boxplot for all numeric columns
#' create_boxplots(mtcars)
#' # Faceted boxplot for all numeric columns
#' create_boxplots(mtcars, facet = TRUE)
#' # Boxplot for a specific column, grouped by a category
#' create_boxplots(mtcars, numeric_column = "mpg", category_column = "cyl")
#' # Faceted boxplot for a specific column (facet ignored if category_column is provided)
#' create_boxplots(mtcars, numeric_column = "mpg", category_column = "cyl", facet = TRUE)

create_boxplots <-
    function(data,
             numeric_column = NULL,
             category_column = NULL,
             facet = FALSE) {
        # Check for required package
        if (!requireNamespace("ggplot2", quietly = TRUE)) {
            stop("ggplot2 must be installed to use create_boxplots")
        }

        # If no numeric column is specified, use all numeric columns
        if (is.null(numeric_column)) {
            numericCols <- sapply(data, is.numeric)
            data <- data[, numericCols, drop = FALSE]
        } else {
            data <- data[, c(numeric_column, category_column), drop = FALSE]
        }

        # If faceting is requested without a category, treat all numeric columns separately
        if (facet && is.null(category_column)) {
            long_data <- reshape2::melt(data)
            plot <-
                ggplot2::ggplot(long_data, ggplot2::aes(y = value)) +
                ggplot2::geom_boxplot() +
                ggplot2::facet_wrap( ~ variable, scales = "free_y") +
                ggplot2::labs(x = "Variable", y = "Value") +
                ggplot2::theme_minimal()
        } else if (!is.null(category_column)) {
            # Create boxplot by category
            plot <-
                ggplot2::ggplot(data,
                                ggplot2::aes_string(x = category_column, y = numeric_column)) +
                ggplot2::geom_boxplot() +
                ggplot2::labs(title = paste("Boxplot of", numeric_column, "by", category_column)) +
                ggplot2::theme_minimal()
        } else {
            # Create individual boxplots for specified/each numeric column(s)
            plots <- lapply(names(data), function(colName) {
                ggplot2::ggplot(data, ggplot2::aes_string(y = colName)) +
                    ggplot2::geom_boxplot() +
                    ggplot2::labs(title = paste("Boxplot of", colName),
                                  y = colName) +
                    ggplot2::theme_minimal()
            })

            # If only one plot, don't return a list
            if (length(plots) == 1) {
                plot <- plots[[1]]
            } else {
                plot <- plots
            }
        }

        # Print or return the plot(s)
        if (exists("plot")) {
            print(plot)
        } else {
            return(plot)
        }
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
create_corr_plot <-
    function(data,
             plotType = "corrplot",
             hclust = FALSE) {
        # Check for required packages
        if (plotType == "pairplot" &&
            !requireNamespace("GGally", quietly = TRUE)) {
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
