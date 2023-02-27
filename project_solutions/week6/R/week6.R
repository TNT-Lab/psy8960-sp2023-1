# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(stringi)



# Data Import
citations <- stri_read_lines("../data/citations.txt", encoding="latin1")
citations_txt <- citations[!stri_isempty(citations)]
length(citations) - length(citations_txt)
str_length(citations_txt) %>% mean

# Data Cleaning
tibble(citations_txt) %>%
  sample_n(10)
citations_tbl <- tibble(line = 1:length(citations_txt), cite = citations_txt) %>%
  mutate(cite = str_replace_all(cite,"'|\"", ""),
         year = as.integer(str_extract(cite, "\\d{4}")),
         page_start = as.integer(str_match(cite, "(\\d+)-\\d+")[,2]),
         perf_ref = str_detect(str_to_lower(cite), "performance"),
         title = str_match(cite, "\\d{4}\\)\\. ([^\\.?]+)")[,2],
         first_author = str_extract(cite, "^\\w+, \\w\\.? ?\\w?\\.? ?\\w?\\."))
sum(!is.na(citations_tbl$first_author))  

# Test Code
sample_n(citations_tbl, 10)
stri_enc_detect(stri_flatten(citations[45000:46000])) 
