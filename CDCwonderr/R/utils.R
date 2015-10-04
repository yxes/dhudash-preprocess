
#' Removes Helper Columns
#'
#' The helper columns (as used in this package) have column names that end with
#' "_row".
#'
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#'
clean_cols <- function(df) {
  df %>% select(-ends_with("_row"))
}

#' Removes spaces in column names
#'
#' @importFrom stringr str_replace_all
#' @importFrom magrittr %>%
#' @return a dataframe, with no spaces in the column names
#'
rm_space_names <- function(df) {
  df %>% set_names(., str_replace_all(names(.), " ", ""))
}

compact <- function(x) x[which(names(x) != "")]
