#' Calculate quick statistics for numeric columns in a data frame
#'
#' @description This function calculates basic statistics (mean, standard deviation, median, mode, and range)
#' for all numeric columns in a given data frame.
#'
#' @param data A data frame or tibble containing numeric columns.
#' @param na.rm Logical. If TRUE, NA values are removed before calculations. Default is TRUE.
#' @param stats A character vector specifying which statistics to calculate.
#'   Options are "mean", "sd", "median", "mode", and "range". Default is all.
#'
#' @return A tibble with statistics for each numeric column. If no numeric columns are found, returns NULL.
#'
#' @examples
#' # Calculate all statistics for mtcars dataset
#' quickstats(mtcars)
#'
#' # Calculate only mean and median
#' quickstats(mtcars, stats = c("mean", "median"))
#'
#' # Handle dataset with no numeric columns
#' quickstats(data.frame(a = c("x", "y", "z"), b = c("a", "b", "c")))
#'
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @export
quickstats <- function(data, na.rm = TRUE) {
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }

  numeric_data <- data %>% select(where(is.numeric))

  if (ncol(numeric_data) == 0) {
    message("No numeric columns found")
    return(NULL)
  }

  result <- numeric_data %>%
    summarise(across(everything(),
                     list(
                       mean = ~mean(., na.rm = TRUE),
                       sd = ~sd(., na.rm = TRUE),
                       median = ~median(., na.rm = TRUE),
                       mode = ~as.numeric(names(which.max(table(., useNA = "no")))),
                       range = ~diff(range(., na.rm = TRUE))
                     )
    )) %>%
    pivot_longer(cols = everything(),
                 names_to = c("variable", ".value"),
                 names_pattern = "(.*)_(.*)") %>%
    select(variable, everything())

  return(result)
}
