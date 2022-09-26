# Superfund
# Data source is epa.gov

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(janitor)
library(showtext)
library(ggshadow)
library(ggtext)
library(maps)

# add font ----------------------------------------------------------------
font_add_google(name = "Playfair Display", family = "Playfair Display")
font_add_google(name = "Roboto", family = "Roboto")
font1 <- "Playfair Display"
font2 <- "Roboto"

# turn on showtext --------------------------------------------------------
showtext_auto()

# load data ---------------------------------------------------------------
superfund_raw <- readr::read_csv("superfund_08222022.csv")

date <- as.Date("2022-08-22")

# create df ---------------------------------------------------------------
df <- superfund_raw %>% 
  clean_names() %>% 
  filter(!state %in% c("AK", "HI", "PR", "GU", "VI")) %>%
  select(site_id, latitude, longitude, npl_status) %>% 
  mutate(npl_status = mdy(npl_status)) %>% 
  mutate(days = trunc(as.Date(npl_status) %--% date) / days(1))

# check out some of the averages and counts -------------------------------
df_avg <- superfund_raw %>% 
  clean_names() %>% 
  filter(!state %in% c("AK", "HI", "PR", "GU", "VI")) %>%
  select(site_id, state, npl_status) %>% 
  mutate(npl_status = mdy(npl_status)) %>% 
  mutate(days = trunc(as.Date(npl_status) %--% date) / days(1)) %>% 
  group_by(state) %>% 
  count(state)
  #summarise(mean = mean(days))

# create plot -------------------------------------------------------------
df %>% 
  ggplot() +
  geom_polygon(data = map_data("state"), aes(x = long, y = lat, group = group), size = 0.33, color = "#F2F2F2") +
  geom_glowpoint(aes(x = longitude, y = latitude), stroke = 0, size = 0.9, alpha = 0.4, color = "#ffe36d", shadowsize = 0.2, shadowalpha = 0.3, shadowcolor = "#f8983f") +
  annotate(geom = "text", x = -122, y = 24.5, label = "Superfund", hjust = "left", family = font1, fontface = "bold", size = 9.8, color = "#000000") +
  annotate("segment", x = -122, xend = -101, y = 21.5, yend = 21.5, color = "#000000", size = 0.24) +
  annotate("segment", x = -122, xend = -101, y = 18, yend = 18, color = "#000000", size = 0.24) +
  annotate(geom = "text", x = -122, y = 21, label = "Comprehensive Environmental Response,\nCompensation, and Liability Act of 1980", hjust = "left", vjust= "top", family = font2, size = 2.6, color = "#000000") +
  annotate("rect", xmin = -81.75, xmax = -73, ymin = 14, ymax = 15, fill = "#ffe36d", alpha = 0.9) +
  annotate(geom = "text", x = -122, y = 17, label = "The Environmental Protection Agency (EPA) directs the investigation and cleanup of sites\ncontaminated with hazardous substances. As of 8.22.2022, there are 1,303 locations listed\non the National  Priorities List designated as the worst contaminated sites requiring long\nterm efforts to remediate.", hjust = "left", vjust= "top", family = font2, size = 3, color = "#000000") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0), limits = c(10, 54)) +
  coord_map(clip="off") +
  theme_void() +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(size = 7, family = font2, color = "#000000", hjust = 1),
        plot.margin = unit(c(0.75, 0.75, 0.75, 0.75), "cm"),
        plot.background = element_rect(color = "#F2F2F2", fill = "#F2F2F2")) +
  labs(caption = "\n\n\nData: epa.gov | Design: Ryan Hart")
  
# save plot ---------------------------------------------------------------
ggsave(paste0("superfund_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)



