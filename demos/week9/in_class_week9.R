library(tidyverse)
api_tbl <- read_csv("https://scraping.tntlab.org/add.php?x=8&y=12&format=csv",
                    col_names=c("x","y","sum"))

library(rvest)
msnbc_download <- read_html("https://msnbc.com")
msnbc_elements <- html_elements(msnbc_download, 
                                css=".styles_teaseTitle__H4OWQ a , .lbp-card__headline , .styles_headline__ice3t a , .styles_headline__vGca_ a , .styles_headline__ZoJUw a , .smorgasbord-meta-content__headline a"
)
msnbc_text <- html_text(msnbc_elements)
