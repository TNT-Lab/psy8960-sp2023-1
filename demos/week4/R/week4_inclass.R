library(tidyverse)
library(data.table)
library(microbenchmark)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

microbenchmark(data_df <- read.csv("../data/chicagohr.csv"))
microbenchmark(data_tbl <- read_csv("../data/chicagohr.csv", show_col_types = FALSE))
microbenchmark(data_dt <- data.table::fread("../data/chicagohr.csv"))

View(as_tibble(mtcars))

mtcars_tbl <- as_tibble(mtcars) %>% 
  View
mtcars_tbl <- mtcars %>%
  as_tibble %>%
  View



