# #NationalPizzaDay | February 9, 2024
# Data Source is from www.scrapehero.com

# load libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(rvest)
library(ggpattern)
library(geomtextpath)

# add font ----------------------------------------------------------------
font_add_google(name = "Passion One", family = "Passion One")
font_t <- "Passion One"

font_add_google(name = "Open Sans", family = "Open Sans")
font <- "Open Sans"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# WEB SCRAPE -----------------------------------------------------
# get data using rvest for screen scraping html ---------------------------
# url
url_pizza <- "https://www.scrapehero.com/location-reports/10-largest-pizza-chains-in-the-usa/"
web_data_pizza <- read_html(url_pizza)


# get data and create df --------------------------------------------------
pizza_raw <- web_data_pizza %>%
  html_nodes(xpath = '/html/body/section[2]/div/div/div[2]/div/div[2]/section') %>%
  html_table()

# wrangle and create df ---------------------------------------------------
df_pizza <- data.frame(pizza_raw) %>% 
  select(X3, X4) %>% 
  slice(1:5) %>% 
  mutate(X4 = as.numeric(gsub(",","", X4))) %>% 
  #https://r-charts.com/part-whole/pie-chart-labels-outside-ggplot2/
  mutate(csum = rev(cumsum(rev(X4))), 
         pos = X4/2 + lead(csum, 1),
         pos = if_else(is.na(pos), X4/2, pos),
         percentage = X4/sum(X4))

# create list of image URLs -----------------------------------------------
images <- c("https://upload.wikimedia.org/wikipedia/commons/thumb/e/ee/Hunt_Brothers_Pizza_logo.svg/440px-Hunt_Brothers_Pizza_logo.svg.png", 
           "https://upload.wikimedia.org/wikipedia/commons/thumb/3/3e/Domino%27s_pizza_logo.svg/440px-Domino%27s_pizza_logo.svg.png",
           "https://upload.wikimedia.org/wikipedia/commons/thumb/5/5d/Pizza_Hut_classic_logo.svg/220px-Pizza_Hut_classic_logo.svg.png", 
           "https://upload.wikimedia.org/wikipedia/en/thumb/7/7e/Little_Caesars_logo.svg/500px-Little_Caesars_logo.svg.png",
           "https://upload.wikimedia.org/wikipedia/commons/thumb/9/92/Papa_Johns_logo.svg/440px-Papa_Johns_logo.svg.png"
           )

# create plot -------------------------------------------------------------
df_pizza %>% 
  ggplot(aes(x = "", y = X4, fill = X3)) +
  geom_col_pattern(aes(pattern_filename = rev(as.factor(X4))), width = 1, pattern = "image", pattern_aspect_ratio = 1, fill = "#D7C0A3", color = "#000000", linewidth = 1) +
  geom_textpath(aes(x = 1.6, y = pos, label = scales::comma(X4), size = 2, angle = 90), family = font_t) +
  coord_polar("y", start = 0) +
  scale_pattern_filename_manual(values = images) +
  theme_void() +
  theme(plot.title = element_text(family = font_t, hjust = 0.5, size = 26, color = "#000000", margin = margin(b = 5)),
        plot.subtitle = element_text(family = font, hjust = 0.5, size = 14, color = "#000000", margin = margin(b = 5)),
        plot.caption = element_text(family = font, hjust = 0.5, size = 8, color = "#000000", margin = margin(t = 5)),
        plot.caption.position = "plot",
        legend.position = "none",
        plot.margin = unit(c(1, 1, 1, 1), "cm"),
        panel.background = element_rect(color = NA, fill = "#FFFFFF"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
  labs(title = "Top 5 Largest Pizza Chains in 2024",
       subtitle = "by number of store locations in the United States",
       caption = "#NationalPizzaDay | Data: scrapehero.com | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("pizza_stores_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)

