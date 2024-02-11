#' Analyze Missing Data with Optional Class Analysis
#'
#' This function calculates the proportion of missing data for each predictor in the dataset.
#' Optionally, if a class column is specified, it also examines the pattern of missing data in relation
#' to the classes of the dataset. It can return results sorted by the proportion of missing data or,
#' for class-specific analysis, by the count of missing occurrences within each class.
#'
#' @param data A dataframe representing the dataset.
#' @param class_col Optionally, the name of the column representing the class or target variable.
#'        If NULL, the function will only calculate the proportion of missing data for each predictor.
#'        Defaults to NULL.
#' @param sort_by Determines how the results should be sorted.
#'        "none": No sorting (default).
#'        "proportion": Sort by the proportion of missing data.
#'        "class": Sort by the count of missing data occurrences within each class, applicable only if class_col is provided.
#' @return Depending on the input parameters, the function returns either:
#'         - A named vector with predictors as names and proportions of missing data as values (default),
#'         - Or, if class_col is specified, a list containing both the proportions of missing data and the
#'           analysis of missing data by class. The results can be sorted based on the sort_by parameter.
#' @export
#' @examples
#' data("Soybean", package = "mlbench")
#' # Basic usage without class analysis
#' missing_proportions <- analyze_missing_data(Soybean)
#' print(missing_proportions)
#'
#' # With class analysis, no sorting
#' missing_analysis <- analyze_missing_data(Soybean, class_col = "Class")
#' print(missing_analysis)
#'
#' # With class analysis, sorted by missing data proportion
#' missing_analysis_sorted <- analyze_missing_data(Soybean, class_col = "Class", sort_by = "proportion")
#' print(missing_analysis_sorted)
#'
#' # Note: Sorting by class count is only meaningful if class_col is provided
#' # missing_analysis_class_sorted <- analyze_missing_data(Soybean, class_col = "Class", sort_by = "class")
#' # print(missing_analysis_class_sorted)

analyze_missing_data_patterns <- function(data, class_col, sort_order = "none") {
    # Ensure the class column exists
    if (!class_col %in% names(data)) {
        stop("class_col not found in the data")
    }

    # Calculate the proportion of missing data for each predictor
    missing_proportions <- sapply(data, function(x) sum(is.na(x)) / length(x))

    # Apply sorting if requested
    if (sort_order == "ascending") {
        missing_proportions <- sort(missing_proportions, decreasing = FALSE)
    } else if (sort_order == "descending") {
        missing_proportions <- sort(missing_proportions, decreasing = TRUE)
    }

    # Identify overall missing data percentage
    overall_missing <- sum(sapply(data, function(x) sum(is.na(x)))) / sum(sapply(data, length))

    # Analyze missing data by class
    data_with_na <- data[!complete.cases(data), ]
    missing_data_by_class <- table(data_with_na[[class_col]])

    # Calculate the proportion of missing entries for each class
    class_counts <- table(data[[class_col]])
    missing_data_proportion_by_class <- missing_data_by_class / class_counts

    # Sort missing data proportion by class if requested
    if (sort_order == "ascending") {
        missing_data_proportion_by_class <- sort(missing_data_proportion_by_class, decreasing = FALSE)
    } else if (sort_order == "descending") {
        missing_data_proportion_by_class <- sort(missing_data_proportion_by_class, decreasing = TRUE)
    }

    # Return a list containing the analysis
    list(
        Overall_Missing_Percentage = overall_missing,
        Missing_Proportions_By_Predictor = missing_proportions,
        Missing_Data_Proportion_By_Class = missing_data_proportion_by_class
    )
}

#' Detect Degenerate Distributions in Categorical Predictors
#'
#' This function examines categorical predictors in a dataset to identify
#' degenerate distributions, including zero variance and near-zero variance predictors.
#' Zero variance predictors have a single unique value, while near-zero variance
#' predictors have a very small number of unique values with severely disproportionate
#' frequencies.
#'
#' @param data A dataframe containing the dataset to be analyzed.
#' @return A list indicating which predictors are degenerate. Each predictor is labeled as
#'         either "Zero Variance" or "Near-Zero Variance" based on the criteria.
#' @examples
#' library(mlbench)
#' data("Soybean")
#' degenerate_predictors <- detect_degenerate_distributions(Soybean)
#' print(degenerate_predictors)
#' @export
detect_degenerate_distributions <- function(data) {
    results <- list()

    for (col in names(data)) {
        if (is.factor(data[[col]])) {
            freqs <- table(data[[col]])
            unique_values <- length(freqs)
            total_samples <- nrow(data)
            most_common_freq <- max(freqs)
            second_most_common_freq <-
                sort(freqs, decreasing = TRUE)[2]

            # Zero variance check
            if (unique_values == 1) {
                results[[col]] <- "Zero Variance"
                next
            }

            # Near-zero variance check
            fraction_unique <- unique_values / total_samples
            freq_ratio <-
                ifelse(
                    second_most_common_freq > 0,
                    most_common_freq / second_most_common_freq,
                    Inf
                )

            if (fraction_unique < 0.1 && freq_ratio > 20) {
                results[[col]] <- "Near-Zero Variance"
            }
        }
    }

    return(results)
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
#' lambda_value <- estimate_boxcox_lambda(iris, "Sepal.Length")
#' print(lambda_value)

# Function to estimate lambda for Box-Cox transformation
estimate_boxcox_lambda <- function(data, column_name) {
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
#'     lambda_values <- apply_boxcox_to_all(Glass)
#'     print(lambda_values)
#' }

# Apply estimate_boxcox_lambda to all numeric columns in the Glass dataset
apply_boxcox_to_all <- function(data) {
    # Filter only numeric columns
    numeric_columns <- sapply(data, is.numeric)
    numeric_data <- data[, numeric_columns]

    # Apply the function to each numeric column
    sapply(names(numeric_data), function(col) {
        tryCatch({
            estimate_boxcox_lambda(data, col)
        }, warning = function(w) {
            NA  # Return NA in case of non-positive values or other warnings
        }, error = function(e) {
            NA  # Return NA in case of errors
        })
    })
}
