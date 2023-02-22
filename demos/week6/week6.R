example <- "line1
line2"
example <- "line1\nline2"
example <- "this is one\\two"
example <- "he said, \"this thing\""
example <- 'he said "this thing"'
example <- "😃"
writeLines(example)

library(tidyverse)
mtcars_cor <- mtcars %>% cor
format(mtcars_cor, format="f", digits=2)
formatC(mtcars_cor, format="f", digits=2)

tibble(stringi::stri_enc_list()) %>% View

example <- c("yes","no")
length(example)
nchar(example)
str_length(example)

x <- c(1:3, NA, 5:11)
paste("Participant", x)
str_c("Participant", x)

str_detect(c("catmandu","dog","cat"), "cat")

library(rebus)
"Gender: " %R% capture(ANY_CHAR)

str_detect(some_string, "Gender:\\t(.)")

example <- "This is a sentence about Mr. Darcy. This is another sentence. This is too."
