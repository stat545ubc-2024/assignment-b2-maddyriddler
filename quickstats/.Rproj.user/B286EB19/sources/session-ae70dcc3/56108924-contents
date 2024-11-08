#' Calculate quick statistics for numeric columns in a data frame
#'
#' @description This function calculates basic statistics (mean, standard deviation, median, mode, and range)
#' for all numeric columns in a given data frame.
#'
#' @param data A data frame containing numeric columns.
#' @param na.rm Logical. If TRUE, NA values are removed before calculations. Default is TRUE.
#'
#' @return A tibble with statistics for each numeric column. If no numeric columns are found, returns NULL.
#'
#' @examples
#' # Example using the built-in mtcars dataset
#' quick_stats(mtcars)
#'
#' # Example with a custom dataset including NA values
#' df <- data.frame(
#'   a = c(1, 2, NA, 4, 5),
#'   b = c(10, 20, 30, NA, 50),
#'   c = c("x", "y", "z", "w", "v")
#' )
#' quick_stats(df)
#'
#' # Example with no numeric columns
#' df_no_numeric <- data.frame(
#'   a = c("a", "b", "c"),
#'   b = c("x", "y", "z")
#' )
#' quick_stats(df_no_numeric)
#'
#' @export
quick_stats <- function(data, na.rm = TRUE) {
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }

  numeric_data <- data %>% dplyr::select(dplyr::where(is.numeric))

  if (ncol(numeric_data) == 0) {
    message("No numeric columns found")
    return(NULL)
  }

  result <- numeric_data %>%
    dplyr::summarise(dplyr::across(dplyr::everything(),
                                   list(
                                     mean = ~mean(., na.rm = na.rm),
                                     sd = ~stats::sd(., na.rm = na.rm),
                                     median = ~stats::median(., na.rm = na.rm),
                                     mode = ~as.numeric(names(which.max(table(., useNA = "no")))),
                                     range = ~diff(range(., na.rm = na.rm))
                                   )
    )) %>%
    tidyr::pivot_longer(cols = dplyr::everything(),
                        names_to = c("variable", ".value"),
                        names_pattern = "(.*)_(.*)") %>%
    dplyr::select(variable, dplyr::everything())

  return(result)
}
