assignment-b1
================
Yuehao Wang
2023-11-01

# STAT 545B Assignment B1: Making a Function

This is Assignment-b1 of STAT 545B. In this .Rmd file, I will define and
document a new function(Exercises 1 & 2). Then I will use somes examples
to show how to use this function (Exercise 3), followed by some formal
tests of the function using the `testthat` package (Exercise 4).

## Load Packages

``` r
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(testthat))
suppressPackageStartupMessages(library(datateachr))
suppressPackageStartupMessages(library(palmerpenguins))
```

## Exercises 1 & 2: Make a function and document it

In my mini data analysis, I often find the need to group by a
categorical variable or combinations of categorical variables and return
the result as a summary tibble. Furthermore, in the steam_games dataset,
the genre column (e.g., Action,Adventure,Strategy) contains combinations
of genres, which leads to an excessive number of rows when executing a
group_by operation. For instance, in dates (e.g., May 12, 2016), I only
want to focus on the year, not the exact date, when performing a
group_by. Hence, I created a new function count_by_general. The input
parameters to the function are the data frame of interest (`data`), a
character vector containing the name(s) of the grouping variable(s)
(`column_names`), and a date (`date`). For example, if steam_games is
used as the input data, and **genre** and **release_date** are provided
as `column_names` and `date` respectively, the function will process
column_names and date. For instance, it will take only the characters
before the first comma in a comma-separated genre, and for the date, it
will take only the year. It would then return a tibble listing the games
across different genre and release_date.

The function examines the type of the input `column_names`, ensuring
they belong to the character class. If the `column_names` do not belong
to the acceptable class (character), an error message saying “Incorrect
column_names class. Ensure all column_names are of class chr” is
returned. To generalize the handling of genre or release_date through
this function, it’s necessary to ensure they are comma-separated (only
the substring before the first comma is considered from the genre
column).

``` r
#' Generalized Count Function
#'
#' This function groups data by one or more categorical variables and returns a summary tibble.
#' For each specified column, if a value contains a comma, only the part before the comma is kept.
#' If a date column is specified, only the year is kept.
#'
#' @param data A data frame.The data should be the raw data frame from which the user wants to extract information.
#' @param column_names character vectors containing the names of the columns to group by. It could be one or more
#' For each specified column, if a value contains a comma, only the part before the comma is kept. So be careful,
#' if you don't want to split the column values by comma, you should use you own functions. Also only accept comma split.
#' @param date A character string containing the name of the date column (optional). If you want to pass a column
#' which is a date, you should use date to specify the parameter. The date must be in the form of mm dd, yy. 
#' @return A tibble containing the count of rows for each group.
#' @examples
#' \dontrun{
#' summary_result <- count_by_general(steam_games, c("genre", "language"), date = "release_date")
#' }
#'
#' @export
count_by_general <- function(data, column_names, date = NULL) {
  
  # Check if data is empty
  if (is.null(data) || nrow(data) == 0) {
    stop("Data is empty. Please provide a valid data frame.")
  }
  
    # Check if any specified column has all NA values
  all_na_cols <- column_names[sapply(column_names, function(col) all(is.na(data[[col]])))]
  if (length(all_na_cols) > 0) {
    stop(paste("All values in column(s)", paste(all_na_cols, collapse = ", "), "are NA."))
  }
  
  # Check if the columns specified in column_names are of class character
  column_classes <- map(column_names, ~ class(data[[.x]])[1])
  non_char_cols <- column_names[column_classes != "character"]
  if (length(non_char_cols) > 0) {
    stop(paste("Incorrect class for columns:", paste(non_char_cols, collapse = ", "), 
               ". Ensure all column_names are of class chr"))
  }
  
  # Process all columns specified in column_names to keep only the part before any comma
  data <- data %>%
    mutate(across(all_of(column_names), ~ str_extract(., "^[^,]*")))
  
  # Process date column if specified
  if (!is.null(date)) {
    date_column <- as.character(date)
    # Check if date column exists in the data
    if (!(date_column %in% names(data))) {
      stop(paste("Date column", date_column, "not found in the data."))
    }
    # Attempt to convert the date column values to Date class
    date_check <- try(as.Date(data[[date_column]], format = "%B %d, %Y"))
    # Check if any date conversion resulted in NA
    if (any(is.na(as.Date(data[[date_column]], format = "%B %d, %Y")))) {
      stop(paste("Date values in column", date_column, "do not match the expected format (Month Day, Year)."))
    }
    # Extract year from date column
    data <- data %>%
      mutate(across(all_of(date_column), 
                    ~ as.Date(., format = "%B %d, %Y") %>% format("%Y")))
    # Append the date column to column_names for grouping
    column_names <- c(column_names, date_column)
  }
  
  # Group by and summarise
   summary_tibble <- data %>%
    group_by(across(all_of(column_names))) %>%
    summarise(count = n(), .groups = 'drop')
  
  return(summary_tibble)
}
```

## Exercise 3: Examples

Now, I will test the function using the *steam_games* dataset for the
`data` parameter.

I will show how the function works when only one categorical
variable(**genre**) is used for the parameter `column_names`.

``` r
summary_result <- count_by_general(steam_games, "genre")
summary_result
```

    ## # A tibble: 32 × 2
    ##    genre                 count
    ##    <chr>                 <int>
    ##  1 Accounting                6
    ##  2 Action                16290
    ##  3 Adventure              6854
    ##  4 Animation & Modeling    231
    ##  5 Audio Production        122
    ##  6 Casual                 6348
    ##  7 Design & Illustration   471
    ##  8 Early Access              6
    ##  9 Education                83
    ## 10 Free to Play            577
    ## # ℹ 22 more rows

I will then show that the function works when `column_names` is a
character vector containing the names of two categorical grouping
variables (**genre**, **languages**).

``` r
summary_result <- count_by_general(steam_games, c("genre", "languages"))
summary_result
```

    ## # A tibble: 66 × 3
    ##    genre      languages          count
    ##    <chr>      <chr>              <int>
    ##  1 Accounting English                6
    ##  2 Action     Czech                  3
    ##  3 Action     Danish                 1
    ##  4 Action     English            16272
    ##  5 Action     German                 2
    ##  6 Action     Japanese               2
    ##  7 Action     Russian                2
    ##  8 Action     Simplified Chinese     1
    ##  9 Action     Turkish                1
    ## 10 Action     <NA>                   6
    ## # ℹ 56 more rows

I will then show that the function works when `date` is not NA
(**release_date**).

``` r
summary_result <- count_by_general(steam_games, "genre", date = "release_date")
```

    ## Error in count_by_general(steam_games, "genre", date = "release_date"): Date values in column release_date do not match the expected format (Month Day, Year).

``` r
summary_result
```

    ## # A tibble: 66 × 3
    ##    genre      languages          count
    ##    <chr>      <chr>              <int>
    ##  1 Accounting English                6
    ##  2 Action     Czech                  3
    ##  3 Action     Danish                 1
    ##  4 Action     English            16272
    ##  5 Action     German                 2
    ##  6 Action     Japanese               2
    ##  7 Action     Russian                2
    ##  8 Action     Simplified Chinese     1
    ##  9 Action     Turkish                1
    ## 10 Action     <NA>                   6
    ## # ℹ 56 more rows

I will show how the function still works for normal values without
comma. (**developer**)

``` r
summary_result <- count_by_general(steam_games, "developer")
summary_result
```

    ## # A tibble: 16,604 × 2
    ##    developer                  count
    ##    <chr>                      <int>
    ##  1 #workshop                      3
    ##  2 'What Day is it?' Games        1
    ##  3 (Not Applicable)               1
    ##  4 (STCG) Smoker The Car Game     1
    ##  5 +7 Software                    3
    ##  6 +Mpact Games                   1
    ##  7 .ez Games                      1
    ##  8 01 Studio                      1
    ##  9 07th Expansion                11
    ## 10 08 Games                       2
    ## # ℹ 16,594 more rows

## Exercise 4: Test the function

I will use the *steam_games* dataset for all of the following tests.

### Edge cases

I use - NULL and empty data -Vector with no NA’s -Vector with NA’s
-Vector of a different type to test all the edge cases of my function.
To see if my function could detect invalid input or wrong format.

**Error due to incorrect data name**I will create an empty data and just
use NULL as the input `data`,to see if Function handles empty data
correctly.

``` r
#If we make data is null, the function should give error message.
test_that("Function handles empty data correctly", {
  empty <-tibble(data.frame())
  expect_error(count_by_general(empty, c("genre"), date = "release_date"),
               "Data is empty. Please provide a valid data frame.")
  expect_error(count_by_general(NULL, c("genre"), date = "release_date"),
               "Data is empty. Please provide a valid data frame.")
})
```

    ## Test passed 🌈

**Error due to non-character columns** I will create a new column with
int value and I can also use penguins dataset to test non-character
columns. Because island in penguins is of class fctr. To see Function
handles non-character columns correctly.

``` r
test_that("Function handles non-character columns correctly", {
  new_data <- steam_games
  new_data_1 <- new_data %>%
  mutate(new_column = 1)
  expect_error(count_by_general(new_data_1, c("new_column"), date = "release_date"),
               "Incorrect class for columns: new_column . Ensure all column_names are of class chr")
  expect_error(count_by_general(penguins, c("island")),
               "Incorrect class for columns: island . Ensure all column_names are of class chr")
})
```

    ## Test passed 🌈

**Error due to all-NA columns** I will use languages column here, and
change all the column value to NA to see if Function handles all NA
values in specified columns correctly.

``` r
test_that("Function handles all NA values in specified columns correctly", {
  na_data <- steam_games
  na_data$languages <- NA
  expect_error(count_by_general(na_data, c("languages")), "All values in column\\(s\\) languages are NA.")
})
```

    ## Test passed 🎉

**Error due to no exist date column** Here I just use a no_exist_column
which is not exist in the steam_games, to see if Function handles all no
exist date column correctly.

``` r
test_that("Function handles all no exist date column correctly", {
  expect_error(count_by_general(steam_games, c("languages"), date = "no_exist_column"),
               "Date column no_exist_column not found in the data.")
})
```

    ## Test passed 🥳

**Error due to wrong format of date column** Here I change the
release_date to “May” which is a wrong format, and then I change the
value to May112022(only delete the comma), to see if Function handles
all wrong format of date column correctly.

``` r
test_that("Function handles all wrong format of date column correctly", {
  na_data <- steam_games
  na_data$release_date <- "May"
  expect_error(count_by_general(na_data, c("languages"), date = "release_date"),
               "Date values in column release_date do not match the expected format \\(Month Day, Year\\).")
   na_data$release_date <- "May112022"
  expect_error(count_by_general(na_data, c("languages"), date = "release_date"),
               "Date values in column release_date do not match the expected format \\(Month Day, Year\\).")
})
```

    ## Test passed 🎉

### compare function output with manually output

Here I manually group the data by genre after I split the values by
comma, and then I test the output by myself and the output of function,
to see if Function output is equal to manually output.

``` r
data <- steam_games
data <- data %>%
    mutate(across(all_of(c("genre")), ~ str_extract(., "^[^,]*")))
manually_summary_tibble <- data %>%
    group_by(across(all_of(c("genre")))) %>%
    summarise(count = n(), .groups = 'drop')
function_summary_tibble = count_by_general(steam_games, c("genre"))
test_that("Function output is equal to manually output", {
  expect_equivalent(function_summary_tibble, manually_summary_tibble)
})
```

    ## Test passed 🎉
