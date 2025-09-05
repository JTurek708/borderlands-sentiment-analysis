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
library(scales)

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

# Emotion bar charts
# 1813 Declaration of Independence
plot_df <- sentiment_score_bgdl[, 1:8] %>%
  as.data.frame() %>%
  { tibble(
    emotion = names(.),
    share = colSums(.) / sum(as.matrix(.))
  )
      } %>%
  mutate(
    emotion = emotion |>
      gsub("_", " ", x = _) |>
      stringr::str_to_title()
  )
p <- ggplot(plot_df, aes(x = reorder(emotion, share), y=share, fill=emotion))+
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = percent(share, accuracy = 0.1)),
            vjust = -0.35, size = 3.5) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.08))
  )+
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Bernardo Gutierrez de Lara",
    subtitle = "1813 Declaration of Mexican Independnece",
    caption = "Analysis by Jack Turek with 'syuzhet' R package.",
    x = "Emotion", y = NULL
  )+
  theme_fivethirtyeight(base_size = 12)+
  theme(
    plot.title      = element_text(face = "bold", size = 16, margin = margin(b = 4)),
    plot.subtitle   = element_text(size = 11, margin = margin(b = 8)),
    plot.caption    = element_text(size = 9, color = "gray35"),
    axis.title.x    = element_text(margin = margin(t = 8)),
    axis.text.x     = element_text(size = 10),
    panel.grid.major.x = element_blank()
  )
p
# 602 by 432
# Texas Declaration of Independence
plot_df <- sentiment_score_tx[, 1:8] %>%
  as.data.frame() %>%
  { tibble(
    emotion = names(.),
    share = colSums(.) / sum(as.matrix(.))
  )
  } %>%
  mutate(
    emotion = emotion |>
      gsub("_", " ", x = _) |>
      stringr::str_to_title()
  )
p <- ggplot(plot_df, aes(x = reorder(emotion, share), y=share, fill=emotion))+
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = percent(share, accuracy = 0.1)),
            vjust = -0.35, size = 3.5) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.08))
  )+
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Texas Rebels Declare Independence",
    subtitle = "1836 Declaration of Texan Independnece",
    caption = "Analysis by Jack Turek with 'syuzhet' R package.",
    x = "Emotion", y = NULL
  )+
  theme_fivethirtyeight(base_size = 12)+
  theme(
    plot.title      = element_text(face = "bold", size = 16, margin = margin(b = 4)),
    plot.subtitle   = element_text(size = 11, margin = margin(b = 8)),
    plot.caption    = element_text(size = 9, color = "gray35"),
    axis.title.x    = element_text(margin = margin(t = 8)),
    axis.text.x     = element_text(size = 10),
    panel.grid.major.x = element_blank()
  )
p

# US Declaration of Independence
plot_df <- sentiment_score_us[, 1:8] %>%
  as.data.frame() %>%
  { tibble(
    emotion = names(.),
    share = colSums(.) / sum(as.matrix(.))
  )
  } %>%
  mutate(
    emotion = emotion |>
      gsub("_", " ", x = _) |>
      stringr::str_to_title()
  )
p <- ggplot(plot_df, aes(x = reorder(emotion, share), y=share, fill=emotion))+
  geom_col(width = 0.7, show.legend = FALSE) +
  geom_text(aes(label = percent(share, accuracy = 0.1)),
            vjust = -0.35, size = 3.5) +
  scale_y_continuous(
    labels = percent_format(accuracy = 1),
    expand = expansion(mult = c(0, 0.08))
  )+
  scale_fill_brewer(palette = "Set3") +
  labs(
    title = "Colonies Rebel Against British Crown",
    subtitle = "1776 Declaration of US Independnece",
    caption = "Analysis by Jack Turek with 'syuzhet' R package.",
    x = "Emotion", y = NULL
  )+
  theme_fivethirtyeight(base_size = 12)+
  theme(
    plot.title      = element_text(face = "bold", size = 16, margin = margin(b = 4)),
    plot.subtitle   = element_text(size = 11, margin = margin(b = 8)),
    plot.caption    = element_text(size = 9, color = "gray35"),
    axis.title.x    = element_text(margin = margin(t = 8)),
    axis.text.x     = element_text(size = 10),
    panel.grid.major.x = element_blank()
  )
p
