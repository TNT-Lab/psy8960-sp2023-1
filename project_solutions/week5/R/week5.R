# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(lubridate)

# Data Import
Adata_tbl <- read_delim("../data/Aparticipants.dat", delim="-", col_name=c("casenum","parnum","stimver","datadate","qs"))
Anotes_tbl <- read_csv("../data/Anotes.csv")
Bdata_tbl <- read_tsv("../data/Bparticipants.dat", col_name=c("casenum","parnum","stimver","datadate",paste0("q",1:10)))
Bnotes_tbl <- read_tsv("../data/Bnotes.txt")

# Data Cleaning
Aclean_tbl <- Adata_tbl %>%
  separate(qs, paste0("q",1:5)) %>%
  mutate(datadate = mdy_hms(datadate),
         mutate(across(contains("q"), as.integer))) %>%
  inner_join(Anotes_tbl) %>%
  filter(is.na(notes))
ABclean_tbl <- Bdata_tbl %>%
  mutate(datadate = mdy_hms(datadate),
         mutate(across(contains("q"), as.integer))) %>%
  inner_join(Bnotes_tbl) %>%
  filter(is.na(notes)) %>%
  bind_rows(Aclean_tbl, .id="lab") %>%
  select(-notes)