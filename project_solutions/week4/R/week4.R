# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)

# Data Import
import_tbl <- read_delim("../data/week4.dat", delim="-", col_names=(c("casenum","parnum","stimver","datadate","qs")))
glimpse(import_tbl)
wide_tbl <- separate(import_tbl, qs, into=paste0("q",1:5))
wide_tbl[,paste0("q",1:5)] <- sapply(wide_tbl[,paste0("q",1:5)], as.integer)
wide_tbl[["datadate"]] <- as.POSIXct(wide_tbl[["datadate"]], format="%b %d %Y, %H:%M:%S")
wide_tbl$q1[wide_tbl$q1 == 0] <- NA
wide_tbl$q2[wide_tbl$q2 == 0] <- NA
wide_tbl$q3[wide_tbl$q3 == 0] <- NA
wide_tbl$q4[wide_tbl$q4 == 0] <- NA
wide_tbl$q5[wide_tbl$q5 == 0] <- NA
wide_tbl <- drop_na(wide_tbl, q2)
long_tbl <- pivot_longer(wide_tbl, q1:q5)

# Alternative
wide_tbl[,paste0("q",1:5)][wide_tbl[,paste0("q",1:5)]==0] <- NA

