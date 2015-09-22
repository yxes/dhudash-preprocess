
xl_focus <- read_excel("NFOCUS18ST.XLS", skip=1)

fill_names <- function(df) {
  emp_cols <- which(names(df) == "")
  names(df)[emp_cols] <- paste0("X", seq_along(emp_cols))
  df
    }


# xl_focus <- xl_focus  %>% rm_fill_names

### add Focus. select states, gender from rows

# fix column names; instead, should use [number]-[number]
year_cols <- xl_focus %>% names %>% str_detect("[1234567890]") %>% which
names(xl_focus)[year_cols]

names(xl_focus)[-1] <- rep(names(xl_focus)[year_cols], each=3) %>% 
  paste0(c("overall", "raw", "error"))

# TODO: 



xl2 <- xl_focus %>% 
  rm_space_names %>% 
  mutate(Objective = str_replace_all(Objective, " ", "")) %>% 
  mutate(State_row = str_detect(Objective, "STATE")) %>% 
  mutate(State = ifelse(State_row, str_sub(Objective, 7, -1), NA)) %>% 
  select(Objective, State, State_row, everything()) %>% 
  fill(State) %>% 
  mutate(Gender = ifelse(str_detect(Objective, "Male"), "Male", "NA")) %>% 
  mutate(Gender = ifelse(str_detect(Objective, "Female"), "Female", Gender))

xl2 %>% glimpse


  mutate(Focus = )
  filter(Gender != "NA" | State_row)





