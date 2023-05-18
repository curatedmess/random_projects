# Estimated number of players for the top 10 video games
# Data Source is twinfinite.net
# https://twinfinite.net/features/most-played-games/

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(tidyr)
library(showtext)
library(rvest)
library(glue)
library(igraph)
library(ggraph)
library(ggfx)
library(ggtext)

# add font ----------------------------------------------------------------
font_add_google(name = "Chivo", family = "Chivo")
font_t <- "Chivo"
font_add_google(name = "Rubik", family = "Rubik")
font <- "Rubik"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# WEB SCRAPE -----------------------------------------------------
# get data using rvest for screen scraping html ---------------------------
# url
url <- "https://twinfinite.net/features/most-played-games/"
web_data <- read_html(url)

# get data and create df --------------------------------------------------
games <- vector() 

for (i in 1:10) {
  web_data %>%
    html_elements(xpath = glue('//*[@id="mvp-content-main"]/h2[{i}]')) %>%
    html_text2()
} -> games[i]

df <- data.frame(games) %>% 
  mutate(rank = gsub("\\..*", "", games)) %>%
  mutate(viewers = gsub(".* – ", "", games)) %>% 
  mutate(game = sub(".*\\.(.*) – .*", "\\1", games)) %>% 
  separate(viewers, into = c("n", "units"), sep = " ") %>% 
  mutate(total_monthly = sum(as.numeric(n)) * 1000000) %>% 
  mutate(total_daily_avg = round(total_monthly / 30, 0)) %>% 
  mutate(source = "total") %>% 
  mutate(perc = as.numeric(n) / 1355)

# create dendrogam lines --------------------------------------------------
# create edge -------------------------------------------------------------
edge_list1 <- df %>% 
  select(source, game, rank, n, perc) %>% 
  unique %>% 
  rename(from = game, to = source) %>% 
  mutate(rank = as.numeric(rank)) %>% 
  mutate(n = as.numeric(n))

# create vertices for size -------------------------------------------------
vertices <- data.frame(name = unique(c(as.character(edge_list1$from), as.character(edge_list1$to))), value = runif(11))

vertices$height = edge_list1$rank[match(vertices$name, edge_list1$from)] %>% 
  replace_na(0)

vertices$n = edge_list1$n[match(vertices$name, edge_list1$from)] %>% 
  replace_na(1355)

vertices$perc = edge_list1$perc[match(vertices$name, edge_list1$from)] %>% 
  replace_na(1)

# create data frame for dendrogram ----------------------------------------
dendro <- graph_from_data_frame(edge_list1, vertices = vertices)

sigma <- 20
expand <- 7
neon <- "#00dfff"

# create plot -------------------------------------------------------------
dendro %>% 
  ggraph(layout = 'dendrogram', circular = FALSE) + 
  as_group(geom_edge_diagonal0(color = "#FFFFFF"),
           geom_segment(aes(x = x, xend = x, y = -2.01, yend = 0.01, color = ifelse(name == "total", NA, "#FFFFFF")), linewidth = 8.5),
           id = "bar") +
  with_outer_glow("bar", colour = neon, sigma = sigma, expand = expand) +
  geom_segment(aes(x = x, xend = x, y = -2, yend = 0, color = ifelse(name == "total", NA, "#000000")), linewidth = 7.5) +
  geom_segment(aes(x = x, xend = x, y = (-2 * perc), yend = 0, color = ifelse(name == "total", NA, neon)), linewidth = 7.5) +
  with_outer_glow(geom_point(data = df, aes(x = 4.5, y = 1.8), size = 45, color = "#FFFFFF"),
                  colour = neon, sigma = sigma, expand = expand) +
  geom_text(aes(x = x, y = -2, label = ifelse(name == "total", "", name)), family = font_t, size = 2.5, hjust = 0, vjust = 0.5, color = "#FFFFFF") +
  geom_text(aes(x = x, y = (-2 * perc) - 0.05, label = ifelse(name == "total", "", paste0(scales::percent(perc)))), family = font_t, size = 2.5, hjust = 1, vjust = 0.5, color = neon) +
  geom_point(data = df, aes(x = 4.5, y = 1.8), size = 43.5, color = neon) +
  geom_text(data = df, aes(x = 5, y = 1.8, label = "Estimated"),family = font, size = 2.5, hjust = 0.5, color = "#000000") +
  annotate("richtext", x = 3.95, y = 1.8, label = "Avg. Monthly <b>Players</b>", family = font, size = 2.5, hjust = 0.5, color = "#000000", fill = NA, label.color = NA) +
  geom_text(data = df, aes(x = 4.5, y = 1.8, label = scales::label_number(accuracy = 0.01, scale_cut = scales::cut_short_scale())(total_monthly)), family = font_t, fontface = "bold", size = 9, hjust = 0.5, color = "#000000") +
  annotate("richtext", x = 2.5, y = 1.8, label = "As of May 2023, these popular<br><span style='font-size:16pt;'>video games</span> have<br>more than a <span style='font-size:14pt; color: #00dfff;'>billion</span> monthly<br><span style='font-size:18pt; color: #00dfff;'><b>PLAYERS</b></span> combined!", family = font, size = 3, vjust = "top", hjust = 0.5, color = "#FFFFFF", fill = NA, label.color = NA, lineheight = 2.5) +
  annotate("richtext", x = -0.75, y = -1, label = "% Players by Game - Share of Total", family = font, size = 2.5, hjust = 0.5, color = "#FFFFFF", fill = NA, label.color = NA, lineheight = 1) +
  scale_color_identity() +
  scale_y_continuous(limits = c(-2.1, 2.5)) +
  coord_flip() +
  theme_void() +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(size = 7.5, family = font, color = "#FFFFFF", hjust = 1, margin = margin(t = 20)),
        # axis.text = element_text(size = 7.5, family = font, color = "#FFFFFF", hjust = 0.5),
        legend.position = "none",
        plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm"),
        plot.background = element_rect(color = NA, fill = "#000000")) +
  labs(caption = "Data source: twinfinite.net | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("video_game_players_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)

