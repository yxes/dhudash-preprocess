
library(readxl)
library(magrittr)
library(devtools)
library(stringr)
library(tidyr)
library(dplyr)

# # following code (uncommented) was used to generate "suicides" dataset
# source("../R/gender_comp.R")
#
#
# suicides <- get_gender_comp("../data-raw/NFOCUS18ST.XLS")
# suicides <- read_excel("data-raw/NFOCUS18ST.XLS", skip = 1) %>%
#   fix_col_names %>% rm_tail
#
# devtools::use_data(suicides)

