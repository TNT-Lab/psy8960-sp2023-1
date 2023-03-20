library(tidyverse)
library(httr)

requests_tbl <- tibble(x=1:10, y=11:20)
responses_tbl <- tibble(raw = character())
get_response <- function(my_x, my_y) {
  print(paste("x is now", my_x, "and y is now", my_y))
  response <- GET("http://scraping.tntlab.org/add.php", 
                  user_agent("UMN Researcher abcde001@umn.edu"),
                              query = list(
                              x = my_x,
                              y = my_y,
                              format = "csv"
                              )
  )
  print(paste("Just received data from", response$url))
  Sys.sleep(2)
  if(http_error(response)) return(tibble(raw="HTTP Error"))
  else return(tibble(raw=content(response, as="text")))
}

for (row in 1:nrow(requests_tbl)) {
  responses_tbl <- bind_rows(responses_tbl, 
                         get_response(requests_tbl[row,"x"], 
                                      requests_tbl[row,"y"]))
}
