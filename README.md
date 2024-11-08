Quick Summary Statistics for Data Frames Package
================
Maddy Riddler

- [quickstats](#quickstats)
  - [Description](#description)
  - [Usage](#usage)
  - [Tests](#tests)
    - [NA vectors](#na-vectors)
    - [Non-numeric vectors](#non-numeric-vectors)
    - [No NA vectors](#no-na-vectors)

# quickstats

## Description

> The quickstats package provides tools for quickly calculating useful
> summary statistics for all numeric variables in a data frame. It
> includes functions for handling missing values and generating
> comprehensive statistical summaries. The main function, quickstats(),
> allows users to calculate various statistics such as mean, standard
> deviation, median, mode, and range for numeric columns in a given data
> frame. Installation You can install the development version of
> quickstats from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("stat545ubc-2024/quickstats")
```

    ## Skipping install of 'quickstats' from a github remote, the SHA1 (74348841) has not changed since last install.
    ##   Use `force = TRUE` to force installation

## Usage

> Here are some examples of how to use the quick_stats() function:

``` r
library(quickstats)

# Calculate all statistics for mtcars dataset
quickstats(mtcars)
```

    ## # A tibble: 11 × 6
    ##    variable    mean      sd median   mode  range
    ##    <chr>      <dbl>   <dbl>  <dbl>  <dbl>  <dbl>
    ##  1 mpg       20.1     6.03   19.2   10.4   23.5 
    ##  2 cyl        6.19    1.79    6      8      4   
    ##  3 disp     231.    124.    196.   276.   401.  
    ##  4 hp       147.     68.6   123    110    283   
    ##  5 drat       3.60    0.535   3.70   3.07   2.17
    ##  6 wt         3.22    0.978   3.32   3.44   3.91
    ##  7 qsec      17.8     1.79   17.7   17.0    8.4 
    ##  8 vs         0.438   0.504   0      0      1   
    ##  9 am         0.406   0.499   0      0      1   
    ## 10 gear       3.69    0.738   4      3      2   
    ## 11 carb       2.81    1.62    2      2      7

``` r
# Handle dataset with no numeric columns
quickstats(data.frame(a = c("x", "y", "z"), b = c("a", "b", "c")))
```

    ## No numeric columns found

    ## NULL

## Tests

### NA vectors

``` r
test_that("quickstats handles vectors with NA values correctly", {
  test_data <- data.frame(x = c(1, 2, NA, 4, 5))
  result <- quickstats(test_data)
  expect_s3_class(result, "tbl_df")
  expect_equal(result$mean, 3)
  expect_equal(result$sd, sd(c(1, 2, 4, 5), na.rm = TRUE))
  expect_equal(result$median, 3)})
```

    ## Test passed 🎊

### Non-numeric vectors

``` r
test_that("quickstats handles non-numeric data correctly", {
  test_data <- data.frame(x = c("a", "b", "c", "d", "e"))
  expect_message(result <- quickstats(test_data), "No numeric columns found")
  expect_null(result)
  test_data_factor <- data.frame(x = factor(c("low", "medium", "high", "low", "high")))
  expect_message(result_factor <- quickstats(test_data_factor), "No numeric columns found")
  expect_null(result_factor)})
```

    ## Test passed 🎊

### No NA vectors

``` r
test_that("quickstats handles data with no NA values correctly", {
  test_data <- data.frame(x = 1:5, y = 6:10)
  result <- quickstats(test_data)
  expect_no_error(quickstats(test_data))
  expect_equal(nrow(result), ncol(test_data))
 expect_equal(result$variable, c("x", "y"))})
```

    ## Test passed 🥇
