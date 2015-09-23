
#' Removes Helper Columns
#'
#' @importFrom dplyr select
#' @importFrom magrittr `%>%`
#'
clean_cols <- function(df) {
  df %>% select(-ends_with("_row"))
}

#' Removes spaces in column names
#'
#' @importFrom stringr str_replace_all
#' @importFrom magrittr `%>%`
#'
rm_space_names <- function(df) {
  set_names(., str_replace_all(names(.), " ", ""))
}



