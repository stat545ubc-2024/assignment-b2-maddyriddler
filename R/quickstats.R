#' Calculate quick statistics for numeric columns in a data frame
#'
#' @param data A data frame containing numeric columns.
#' @param na.rm Logical. If TRUE, NA values are removed before calculations. Default is TRUE.
#'
#' @return A tibble with statistics for each numeric column. If no numeric columns are found, returns NULL.
#'
#' @examples
#' # Calculate statistics for mtcars dataset
#' quickstats(mtcars)
#'
#' # Handle dataset with no numeric columns
#' quickstats(data.frame(a = c("x", "y", "z"), b = c("a", "b", "c")))
#'
#' @import dplyr
#' @import tidyr
#' @importFrom stats sd median
#' @importFrom magrittr %>%

#' @importFrom rlang .data
#' @export
quickstats <- function(data, na.rm = TRUE) {
  if (!is.data.frame(data)) {
    stop("Input must be a data frame.")
  }

  numeric_data <- data %>%
    dplyr::select(dplyr::where(is.numeric))

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
    dplyr::select(.data$variable, dplyr::everything())

  return(result)
}
