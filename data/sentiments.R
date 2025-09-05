### Sentiment Analysis Between Texas & US Declarations of Independence
## HS7345 US Borderlands
# September 6, 2025

# Load packages
library(tidyverse)
install.packages("syuzhet")
library(syuzhet)
library(RColorBrewer)
install.packages("wordcloud")
install.packages("tm")
library(wordcloud)
library(tm)
library(ggplot2)
library(ggthemes)

# Load the texts
bgdl_text_string <- get_text_as_string("bgdl-declaration1813.txt")
tx_declaration_string <- get_text_as_string("tx-declaration.txt")
us_declaration_string <- get_text_as_string("us-declaration.txt")

# Get tokens of each declaration (essentially list of words)
bgdl_text_words <- get_tokens(bgdl_text_string)
tx_text_words <- get_tokens(tx_declaration_string)
us_text_words <- get_tokens(us_declaration_string)

# Get sentiment scores for each declaration with NRC Sentiment Lexicon
sentiment_score_bgdl <- get_nrc_sentiment(bgdl_text_string)
sentiment_score_tx <- get_nrc_sentiment(tx_text_words)
sentiment_score_us <- get_nrc_sentiment(us_text_words)


