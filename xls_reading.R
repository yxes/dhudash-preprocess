

#' Function to replace empty column names with filler names
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
  library(tidyr)
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
year_cols <- xl_focus %>% names %>% str_detect("[1234567890]") %>% which
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
  glimpse




