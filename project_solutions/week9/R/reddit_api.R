# Script Settings and Resources
library(tidyverse)
library(jsonlite)

# Data Import and Cleaning
rstats_list <- fromJSON("https://www.reddit.com/r/rstats/.json", flatten = T)
rstats_original_tbl <- rstats_list$data$children
rstats_tbl <- rstats_original_tbl %>%
  select(post = data.title,
         upvotes = data.ups,
         comments = data.num_comments)

# Visualization
ggplot(rstats_tbl, 
       aes(x = upvotes, y = comments)) +
  geom_point() +
  geom_smooth(method="lm")

# Analysis
vote_comment_test <- cor.test(
  rstats_tbl$upvotes,
  rstats_tbl$comments
)
vote_comment_test

# Publication
# The correlation between upvotes and comments was r(23) = .08, p = .71. This test was not statistically significant.
paste0(
  "The correlation between upvotes and comments was r(",
  vote_comment_test$parameter,
  ") = ",
  str_remove(
    format(
      round(vote_comment_test$estimate,2), 
      nsmall=2),
    "^0"),
  ", p = ",
  str_remove(
    format(
      round(vote_comment_test$p.value,2), 
      nsmall=2),
    "^0"),
  ". This test was ",
  ifelse(vote_comment_test$p.value > .05, "not", ""),
  " statistically significant."
)
