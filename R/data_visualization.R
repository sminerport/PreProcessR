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
            ggplot2::facet_wrap( ~ variable, scales = "free") +
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

create_bar_charts <- function(data, facet = FALSE) {
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
            ggplot2::facet_wrap(~ variable, scales = "free_x") +
            ggplot2::labs(x = "Category", y = "Frequency") +
            ggplot2::theme_minimal() +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1))
        return(p)
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

create_boxplots <- function(data, facet = FALSE) {
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
            ggplot2::facet_wrap(~ variable, scales = "free_y") +
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

create_boxplot_by_category <-
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
