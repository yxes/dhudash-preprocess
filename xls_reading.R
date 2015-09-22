

#' Function to replace empty column names with filler names
#' @param df a dataframe
fill_names <- function(df) {
  emp_cols <- which(names(df) == "")
  names(df)[emp_cols] <- paste0("X", seq_along(emp_cols))
  df
    }


### add Focus. select states, gender from rows

# load packages
suppressPackageStartupMessages({
  library(plyr)
  library(dplyr)
  library(pryr)
  #library(tidyr)
  library(stringr)
  library(ggplot2)
  library(magrittr)
  library(devtools)
  library(readr)
  library(magrittr)
  library(beepr)
  library(readxl)
})

detach("package:dplyr", unload = T)
dev_mode()
library(dplyr)

# load data
xl_focus <- read_excel("NFOCUS18ST.XLS", skip=1)

# fix column names; instead, should use [number]-[number]
year_cols <- xl_focus %>% names %>% str_detect("[0-9]+") %>% which
names(xl_focus)[year_cols]

names(xl_focus)[-1] <- rep(names(xl_focus)[year_cols], each=3) %>% 
  paste0(c("rounded", "raw", "error"))


# TODO: 



xl2 <- xl_focus %>% 
  rm_space_names %>% 
  mutate(Objective = str_replace_all(Objective, " ", "")) %>% 
  mutate(State_row = str_detect(Objective, "STATE")) %>% 
  mutate(State = ifelse(State_row, str_sub(Objective, 7, -1), NA)) %>% 
  select(Objective, State, State_row, everything()) %>% 
  mutate(Gender = ifelse(str_detect(Objective, "Male"), "Male", "NA")) %>% 
  mutate(Gender = ifelse(str_detect(Objective, "Female"), "Female", Gender))

xl2 %>% glimpse

xl2 <- xl2 %>% 
  mutate(which_focus = str_detect(Objective, "Suicide")) %>% 
  mutate(Focus = ifelse(which_focus, Objective, NA)) %>% 
  fill(State, Focus) %>% 
  filter(Gender != "NA" | State_row) %>% 
  select(Focus, Objective, State, Gender, everything(), -which_focus)
  
xl2 %>% 
  mutate(Objective = ifelse(State_row, "Overall", Objective)) %>% 
  select(-State_row, -Gender) %>% 
  gather(Type, Percentage, -c(Focus:State)) %>% 
  separate(Type, c("Year", "Type"), sep = 4) %>% 
  mutate(Percentage = as.numeric(Percentage), Year = as.numeric(Year)) %>% 
  filter(!is.na(Percentage), Type != "rounded") %>% 
  spread(Objective, Percentage) %>% 
  arrange(Focus, State, Year, desc(Type)) 

xl_final <- .Last.value 
xl_final %>% write_csv("mental_test.csv")


#####
# xl 19: Nutritional Focus

#' 
#' Removes empty columns, and renames columns
#' 
#' @param df
#' 
fix_col_names <- function(df) {
  
  df <- rm_space_names(df)
  empty_cols <- which(names(df) == "")
  df <- df[, -empty_cols]
  
  year_cols <- names(df) %>% str_detect("[0-9]+") %>% which
  names(df)[-1] <- rep(names(df)[year_cols], each=3) %>% 
    paste0(c("rounded", "raw", "error"))
  
  df
}

#' Remove documentation at tail end of excel doc
#' 
#' 
#' @param num_rows number of rows to remove from end
rm_tail <- function(df, num_rows = 6) {
  last_rows <- seq(to = nrow(df), by = 1, length.out = num_rows)
  df[-last_rows, ]
}



#' 
#' 
#' 
#' 
add_gender <- function(df) {
  
  mod_df <- df %>% mutate( gender_row = str_detect(Objective, "(Fe)?[Mm]ale") ) %>% 
    select(Objective, gender_row, everything())
  mod_df
}


#' 
#' TODO: MAKE "add_" FUNCTIONS ADD ADDITIONAL COLUMNS DENOTING INCLUSION?
#' Assumes there is a column named "Objective"
#'  
add_states <- function(df) {
  
  mod_df <- df %>% mutate(State_row = str_detect(Objective, "STATE")) %>% 
    select(Objective, State_row, everything())
  mod_df  
}

#' 
#' 
#' ASSUMES State_row COLUMN IS PRESENT
#' 
add_state_names <- function(df) {
  
  df %>% 
    mutate(State = ifelse(State_row, str_replace_all(Objective, " ", ""), NA)) %>% 
    mutate(State = str_sub(State, 7, -1)) %>% 
    fill(State) %>% 
    select(Objective, State, everything())
}

add_focus <- function(df) {
  
  mod_df <- df %>% 
    mutate(focus_row = str_detect(Objective, "[0-9]+-[0-9]+")) %>% 
    select(Objective, focus_row, everything())
  
  mod_df
}

#' 
#' NB: ASSUMES focus_row is present
#' TODO: perhaps there can be a generalization to handle add_focus_name, 
#'  add_state_names
#' 
add_focus_name <- function(df) {
  
  mod_df <- df %>% 
    mutate(focus_name = ifelse(focus_row, Objective, NA)) %>% 
    fill(focus_name) %>% 
    select(Objective, focus_name, everything())

  mod_df
}


form_years <- function(df) {
  
  year_cols <- names(df) %>% str_detect("[0-9]+") %>% which
  year_colnames <- names(df)[year_cols]
  
  df %>% 
    gather_("Year", "Value", year_colnames) %>% 
    separate(Year, into = c("Year", "Type"), sep = 4) %>% 
    mutate(Value = extract_numeric(Value))
}


clean_cols <- function(df) {
  df %>% select(-ends_with("_row"))
}


nut_focus <- read_excel("NFOCUS19ST.XLS", skip = 1)
nut_focus <- nut_focus %>% fix_col_names %>% rm_tail

mod_nut <- nut_focus %>% 
  add_focus %>% 
  add_focus_name %>%
  add_states %>% 
  add_state_names %>% 
  add_gender

fin_nut <- mod_nut %>% 
  form_years %>% 
  clean_cols %>% 
  filter(!is.na(focus_name), !is.na(Value)) %>% 
  spread(Type, Value)

