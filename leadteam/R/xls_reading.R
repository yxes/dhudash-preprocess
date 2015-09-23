


#' Simplifies Working with columns in the xls files
#'
#' Removes empty columns, and renames the columns with non-unique names.
#'
#' @param df
#' @importFrom stringr str_detect
#' @importFrom magrittr `%>%`
#' @export
fix_col_names <- function(df) {

  df <- rm_space_names(df)
  empty_cols <- which(names(df) == "")
  df <- df[, -empty_cols]

  year_cols <- names(df) %>% str_detect("[0-9]+") %>% which
  names(df)[-1] <- rep(names(df)[year_cols], each=3) %>%
    paste0(c("rounded", "raw", "error"))

  df
}

#' Remove the last lines of an excel sheet.
#'
#' This function aims to remove the documentation often contained included at
#' at the tail end of excel document. Those last lines also
#' tend to have hyphens in the "Objective" column, which interferes with the
#' heuristic used to generate a column for the name of the FOCUS that data
#' corresponds to.
#'
#' @param df a dataframe
#' @param num_rows number of rows to remove from end
#' @export
rm_tail <- function(df, num_rows = 6) {
  last_rows <- seq(to = nrow(df), by = 1, length.out = num_rows)
  df[-last_rows, ]
}


#' Identifies Gender-related rows
#'
#' by adding another column
#'
#' @param df a dataframe
add_gender <- function(df) {

  mod_df <- df %>% mutate( gender_row = str_detect(Objective, "(Fe)?[Mm]ale") ) %>%
    select(Objective, gender_row, everything())
  mod_df
}


#' Identify Rows Beginning State-related Section
#'
#' Assumes there is a column named "Objective"
#'
#' @importFrom dplyr mutate select
#' @importFrom stringr str_detect
#' @importFrom magrittr `%>%`
#'
#' @param df a dataframe
#' @export
add_states <- function(df) {

  mod_df <- df %>% mutate(State_row = str_detect(Objective, "STATE")) %>%
    select(Objective, State_row, everything())
  mod_df
}

#' Adds a Column With State Name
#'
#' NB: assume that there is a State_row column present in the dataframe
#'
#' @importFrom dplyr mutate select
#' @importFrom stringr str_sub str_replace_all
#' @importFrom tidyr fill
#' @importFrom magrittr `%>%`
#'
#' @param df a dataframe
#'
#' @export
#'
add_state_names <- function(df) {

  if( !("State_row" %in% names(df)) | !("Objective" %in% names(df)))
    stopifnot( "State_row should be a column in the dataframe", call. = FALSE)

  df %>%
    mutate(State = ifelse(State_row, str_replace_all(Objective, " ", ""), NA)) %>%
    mutate(State = str_sub(State, 7, -1)) %>%
    fill(State) %>%
    select(Objective, State, everything())
}

#' Add column Indicating Rows With Focus Name
#'
#'
#' @param a dataframe
#' @export
add_focus <- function(df) {

  mod_df <- df %>%
    mutate(focus_row = str_detect(Objective, "[0-9]+-[0-9]+")) %>%
    select(Objective, focus_row, everything())

  mod_df
}

#' Adds Column with Name of Condition of Health Focus
#'
#' TODO: perhaps there can be a generalization to handle add_focus_name,
#'  add_state_names
#' @param df a dataframe
#' @export
add_focus_name <- function(df) {

  if( !("focus_row" %in% names(df)) )
    stop("focus_row should be column of df", call. = FALSE)

  mod_df <- df %>%
    mutate(focus_name = ifelse(focus_row, Objective, NA)) %>%
    fill(focus_name) %>%
    select(Objective, focus_name, everything())

  mod_df
}

#' Form Columns for "Year" and "Percentages"
#'
#'
#' @importFrom stringr str_detect
#' @importFrom tidyr gather_ separate extract_numeric
#' @importFrom magrittr `%>%`
#' @importFrom dplyr mutate
#'
#' @export
form_years <- function(df) {

  year_cols <- names(df) %>% str_detect("[0-9]+") %>% which
  year_colnames <- names(df)[year_cols]

  df %>%
    gather_("Year", "Value", year_colnames) %>%
    separate(Year, into = c("Year", "Type"), sep = 4) %>%
    mutate(Value = extract_numeric(Value))
}

