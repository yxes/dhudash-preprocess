
#' Obtain Health Conditions by Gender
#'
#'
#'
#' @importFrom tidyr spread
#' @importFrom dplyr filter
#' @importFrom readxl read_excel
#' @param fname filename, as a string
#' @examples
#' mental <- get_gender_comp("../../jhuDash_pkg/raw_data/NFOCUS03ST.XLS")
#'
get_gender_comp <- function(fname) {

  cond_focus <- read_excel(fname, skip = 1)
  cond_focus <- cond_focus %>% fix_col_names %>% rm_tail

  mod_cond <- cond_focus %>%
    add_focus %>%
    add_focus_name %>%
    add_states %>%
    add_state_names %>%
    add_gender

  fin_cond <- mod_cond %>%
    form_years %>%
    clean_cols %>%
    filter(!is.na(focus_name), !is.na(Value)) %>%
    spread(Type, Value)

  fin_cond
}
