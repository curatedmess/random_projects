# William Faulkner's 1,288-word sentence from Absalom, Absalom!
# The sentence is decoded using the Universal POS tags by an
# NLP model available through the udpipe R package

# load libraries ----------------------------------------------------------
library(tidyverse)
library(showtext)
library(rvest)
library(udpipe)
library(pals)

# add font ----------------------------------------------------------------
font_add_google(name = "Roboto Mono", family = "Roboto Mono")
font <- "Roboto Mono"

font_add_google(name = "Sriracha", family = "Sriracha")
font_title <- "Sriracha"

font_add_google(name = "Roboto", family = "Roboto")
font_other <- "Roboto"


# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

options(scipen = 999)

# load language model -----------------------------------------------------
#model <- udpipe_download_model(language = "english")
udmodel_english <- udpipe_load_model(file = 'english-ewt-ud-2.5-191206.udpipe')


# WEB SCRAPE -----------------------------------------------------
# get data using rvest for screen scraping html ---------------------------
# url
url <- "https://www.openculture.com/2019/03/when-william-faulkner-set-the-world-record-for-writing-the-longest-sentence-in-literature.html"
web_data <- read_html(url)

# get data and create df --------------------------------------------------
sentence_raw <- web_data %>%
    html_elements(xpath = '//*[@id="wrap"]/div[2]/div/div[3]/div/blockquote') %>%
    html_text2()

df_sentence <- data.frame(sentence_raw)


# run through the model to get language tags ------------------------------
annotate <- udpipe_annotate(udmodel_english, df_sentence$sentence_raw)


# create data frame -------------------------------------------------------
df <- data.frame(annotate) %>% 
  mutate(token_id2 = row_number()) %>% 
  select(doc_id, token_id, token_id2, token, upos) %>% 
  mutate(token_id2 = as.numeric(token_id2)) %>% 
  mutate(length = str_length(upos)) %>% 
  mutate(row_length = accumulate(length, ~ifelse(.x + .y <= 75, .x + .y, .y)), row_id = cumsum(length == row_length)) %>% 
  group_by(row_id, token_id2) %>% 
  mutate(xend = row_length) %>% # end length
  mutate(xstart = xend - length) %>%  # start length
  ungroup()

# create plot -------------------------------------------------------------
df %>% 
  ggplot() +
  geom_text(aes(x = (xstart + xend)/2, y = row_id, label = upos, color = upos), family = font, fontface = "bold", size = 6) +
  scale_x_continuous(expand = c(0, 0), limits = c(-1, 76)) +
  scale_color_manual(values=as.vector(glasbey(15))) +
  scale_y_reverse() +
  theme_void() +
  theme(plot.title = element_text(family = font_title, size = 52, hjust = 0.5, face = "bold", color = "#000000", margin = margin(b = 15)),
        plot.title.position = "plot",
        plot.subtitle = element_text(family = font_other, size = 24, hjust = 0.5, color = "#000000", lineheight = 1.1, margin = margin(b = 3)),
        plot.caption.position = "plot",
        plot.caption = element_text(size = 22, family = font_other, color = "#000000", hjust = 0.5, margin = margin(t = 3)),
        legend.position = "none",
        plot.margin = unit(c(3, 3, 3, 3), "cm"),
        panel.background = element_rect(color = NA, fill = "#FFFFFF"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
  labs(title = "Anatomy of a Long Sentence",
       subtitle = "Decoding William Faulkner's 1,288-word sentence from\nAbsalom, Absalom! with universal parts-of-speech tags",
       caption = "Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("a_long_sentence_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 14, height = 26)


