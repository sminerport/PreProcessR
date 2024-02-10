#' Calculate Proportion of Missing Data
#'
#' This function calculates the proportion of missing data for each predictor in the dataset.
#' It returns a named vector where names are the predictors and values are the proportions
#' of missing data.
#'
#' @param data A dataframe representing the dataset.
#' @return A named vector with predictors as names and proportions of missing data as values.
#' @export
#' @examples
#' data("Soybean", package = "mlbench")
#' missing_proportions <- missing_data_proportion(Soybean)
#' print(missing_proportions)
missing_data_proportion <- function(data) {
    sapply(data, function(x)
        sum(is.na(x)) / length(x))
}

#' Analyze Missing Data by Class
#'
#' This function examines the pattern of missing data in relation to the classes of the dataset.
#' It identifies rows with missing data and tabulates the frequency of each class within these rows.
#'
#' @param data A dataframe representing the dataset.
#' @param class_col The name of the column representing the class or target variable as a string.
#' @return A table with class labels and counts of missing data occurrences in each class.
#' @export
#' @examples
#' data("Soybean", package = "mlbench")
#' missing_by_class <- missing_data_by_class(Soybean, "Class")
#' print(missing_by_class)
missing_data_by_class <- function(data, class_col) {
    if (!class_col %in% names(data)) {
        stop("class_col not found in the data")
    }

    data_with_na <- data[!complete.cases(data), ]
    class_data_with_na <- data_with_na[[class_col]]

    if (is.factor(class_data_with_na)) {
        return(table(class_data_with_na, useNA = "ifany"))
    } else {
        stop("class_col is not a factor. This function is intended for categorical class variables.")
    }
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
#' lambda_value <- estimateBoxCoxLambda(iris, "Sepal.Length")
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
#'     lambda_values <- applyBoxCoxToAll(Glass)
#'     print(lambda_values)
#' }

# Apply estimateBoxCoxLambda to all numeric columns in the Glass dataset
apply_boxcox_to_all <- function(data) {
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
