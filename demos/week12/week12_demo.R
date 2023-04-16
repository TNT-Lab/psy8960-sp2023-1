library(tidyverse)
library(tm)
library(qdap)
library(textstem)
library(RWeka)
library(wordcloud)
library(tidytext)

shakespeare <- "SHALL I compare thee to a summer's day?
Thou art more lovely and more temperate.
Rough winds do shake the darling buds of May,
And summer's lease hath all too short a date:
Sometime too hot the eye of heaven shines,
And often is his gold complexion dimm'd;
And every fair from fair sometimes declines,
By chance, or nature's changing course, untrimm'd;
But thy eternal summer shall not fade
Nor lose possession of that fair thou ow'st;
Nor shall Death brag thou wand'rest in his shade,
When in eternal lines to time thou grow'st.
So long as men can breathe or eyes can see,
So long lives this, and this gives life to thee."

shakespeareVector <- str_split_1(shakespeare, "\n")
shakespeareCorpusRaw <- VCorpus(VectorSource(shakespeareVector))

shakespeareCorpusRaw$content[[6]]$content
shakespeareCorpus$content[[6]]$content

shakespeareCorpus <- shakespeareCorpusRaw %>%
  tm_map(content_transformer(replace_abbreviation)) %>%
  tm_map(content_transformer(replace_contraction)) %>%
  tm_map(content_transformer(str_to_lower)) %>%
  tm_map(removeNumbers) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeWords, stopwords("en")) %>%
  tm_map(stripWhitespace) %>%
  tm_map(stemDocument, language="english")


# 1-gram
DTM <- DocumentTermMatrix(shakespeareCorpus)

# n-grams (3 here)
myTokenizer <- function(x) { 
  NGramTokenizer(x, Weka_control(min=1, max=3)) 
}
DTM <- DocumentTermMatrix(
  shakespeareCorpus,
  control = list(tokenize = myTokenizer))
DTM_tbl <- as_tibble(as.matrix((DTM)))

DTM_slim <- DTM %>%
  removeSparseTerms(.95)
# 81 3 var, 89 9 var, <.95
DTM_slim_tbl <- as_tibble(as.matrix((DTM_slim)))

wordCounts <- colSums(DTM_tbl)
wordNames <- names(wordCounts)                          
wordcloud(wordNames, wordCounts)

get_sentiments() %>% View
