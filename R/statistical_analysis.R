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
#' calculate_iqr(mtcars, "mpg")
#'
#' # To apply calculate_iqr to all numeric columns in a dataframe:
#' numeric_columns <- sapply(mtcars, is.numeric)
#' IQR_results <- lapply(names(mtcars)[numeric_columns], function(col) calculate_iqr(mtcars, col))
#'

calculate_iqr <- function(data, column) {
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
