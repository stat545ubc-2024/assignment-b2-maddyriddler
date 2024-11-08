#' Calculate quick statistics for numeric columns in a data frame
#'
#' @description This function calculates basic statistics (mean, standard deviation, median, mode, and range)
#' for all numeric columns in a given data frame.
#'
#' @param data A data frame containing numeric columns.
#' @param na.rm Logical. If TRUE, NA values are removed before calculations. Default is TRUE.
#' @param stats A character vector specifying which statistics to calculate.
#'   Options are "mean", "sd", "median", "mode", and "range". Default is all.
#' @param as_list Logical. If TRUE, returns results as a list instead of a tibble. Default is FALSE.
#'
#' @return A tibble or list with statistics for each numeric column. If no numeric columns are found, returns NULL.
#'
#' @examples
#' # Example using the built-in mtcars dataset
#' quick_stats(mtcars)
#'
#' # Example with custom statistics and list output
#' quick_stats(mtcars, stats = c("mean", "median"), as_list = TRUE)
#' @import dplyr
#' @import tidyr
#' @import stats
#' @export
quick_stats <- function(data, na.rm = TRUE, stats = c("mean", "sd", "median", "mode", "range"), as_list = FALSE) {
  if (!is.data.frame(data)) {
    stop("Input must be a data frame or tibble.", call. = FALSE)
  }

  if (!is.logical(na.rm)) {
    stop("na.rm must be a logical value (TRUE or FALSE).", call. = FALSE)
  }

  valid_stats <- c("mean", "sd", "median", "mode", "range")
  stats <- match.arg(stats, valid_stats, several.ok = TRUE)

  numeric_data <- data %>% dplyr::select(dplyr::where(is.numeric))

  if (ncol(numeric_data) == 0) {
    message("No numeric columns found in the input data frame.")
    return(NULL)
  }

  stat_functions <- list(
    mean = ~mean(., na.rm = na.rm),
    sd = ~stats::sd(., na.rm = na.rm),
    median = ~stats::median(., na.rm = na.rm),
    mode = ~{
      tbl <- table(., useNA = "no")
      max_freq <- max(tbl)
      as.numeric(names(tbl[tbl == max_freq]))
    },
    range = ~diff(range(., na.rm = na.rm))
  )

  result <- tryCatch({
    numeric_data %>%
      dplyr::summarise(dplyr::across(dplyr::everything(),
                                     stat_functions[stats]
      )) %>%
      tidyr::pivot_longer(cols = dplyr::everything(),
                          names_to = c("variable", ".value"),
                          names_pattern = "(.*)_(.*)") %>%
      dplyr::select(variable, dplyr::everything())
  }, error = function(e) {
    message("An error occurred: ", e$message)
    return(NULL)
  })

  if (as_list && !is.null(result)) {
    result <- split(result, result$variable)
    result <- lapply(result, function(x) {
      x$variable <- NULL
      as.list(x)
    })
  }

  return(result)
}
