# #MayThe4thBeWithYou | Star Wars Movies
# Data Source is imdb and amazon

# libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(cropcircles)
library(ggimage)

# add font ----------------------------------------------------------------
font_add_google(name = "Oswald", family = "Oswald")
font_add_google(name = "Source Sans Pro", family = "Source Sans Pro")
font_t <- "Oswald"
font <- "Source Sans Pro"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)


# create list of image URLs -----------------------------------------------
images <- c("https://m.media-amazon.com/images/I/51yHBMzxszL._AC_UF894,1000_QL80_.jpg", 
           "https://m.media-amazon.com/images/I/51BGV8AJ4RL._AC_UF894,1000_QL80_.jpg", 
           "https://m.media-amazon.com/images/I/51RHXMVH9YL._AC_UF894,1000_QL80_.jpg", 
           "https://m.media-amazon.com/images/I/919GTKxZZVL._AC_UF1000,1000_QL80_.jpg", 
           "https://m.media-amazon.com/images/I/51pPOQbrk3L._AC_UF894,1000_QL80_.jpg", 
           "https://m.media-amazon.com/images/I/81aA7hEEykL._AC_UF1000,1000_QL80_.jpg", 
           "https://m.media-amazon.com/images/M/MV5BYmU1NDRjNDgtMzhiMi00NjZmLTg5NGItZDNiZjU5NTU4OTE0XkEyXkFqcGdeQXVyNzkwMjQ5NzM@._V1_.jpg", 
           "https://m.media-amazon.com/images/M/MV5BOWZlMjFiYzgtMTUzNC00Y2IzLTk1NTMtZmNhMTczNTk0ODk1XkEyXkFqcGdeQXVyNTAyODkwOQ@@._V1_.jpg", 
           "https://m.media-amazon.com/images/M/MV5BOTAzODEzNDAzMl5BMl5BanBnXkFtZTgwMDU1MTgzNzE@._V1_.jpg", 
           "https://m.media-amazon.com/images/M/MV5BMjQ1MzcxNjg4N15BMl5BanBnXkFtZTgwNzgwMjY4MzI@._V1_FMjpg_UX1000_.jpg",
           "https://m.media-amazon.com/images/M/MV5BMDljNTQ5ODItZmQwMy00M2ExLTljOTQtZTVjNGE2NTg0NGIxXkEyXkFqcGdeQXVyODkzNTgxMDg@._V1_.jpg")

# create data -------------------------------------------------------------
star_wars <- data.frame(order = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), 
                        name = c("Star Wars: Episode I – The Phantom Menace",
                                 "Star Wars: Episode II – Attack of the Clones",
                                 "Star Wars: Episode III – Revenge of the Sith",
                                 "Solo: A Star Wars Story",
                                 "Rogue One: A Star Wars Story",
                                 "Star Wars: Episode IV – A New Hope",
                                 "Star Wars: Episode V – The Empire Strikes Back",
                                 "Star Wars: Episode VI – Return of the Jedi",
                                 "Star Wars: The Force Awakens",
                                 "Star Wars: The Last Jedi",
                                 "Star Wars: The Rise of Skywalker"),
                        images = circle_crop(images, border_size = 10, just = "center")) 

# variables
num_points <- 11
w <- 3
h <- 4

# create points
df_points <- tibble(id = seq(1, num_points, 1)) %>% 
  mutate(x = ifelse(id %% 2 == 0, rep(w:1, by = 1, w), rep(1:w, by = 1, w))) %>% 
  mutate(y = rep(h:1, each = w, length.out = nrow(.)))

# merge
df <- df_points %>% 
  left_join(star_wars, by = c("id" = "order"))
  
# create plot -------------------------------------------------------------
df %>% 
  ggplot(aes(x = x, y = y)) +
  geom_line(aes(group = y), linewidth = 1.25) +
  geom_curve(aes(x = 3, y = 4, xend = 3, yend = 3), curvature = -1.25, ncp = 4500, linewidth = 1.25) +
  geom_curve(aes(x = 1, y = 3, xend = 1, yend = 2), curvature = 1.25, ncp = 4500, linewidth = 1.25) +
  geom_curve(aes(x = 3, y = 2, xend = 3, yend = 1), curvature = -1.25, ncp = 4500, linewidth = 1.25) +
  geom_image(aes(image = images), size = 0.2, by = "height", asp = 1.8) +
  annotate("text", x = 1, 4.5, label = "START", family = font, size = 3, fontface = "bold") +
  annotate("text", x = 2, 0.5, label = "FINISH", family = font, size = 3, fontface = "bold") +
  scale_y_continuous(limits = c(0.5, 4.5)) +
  scale_x_continuous(limits = c(0.5, 3.5)) +
  coord_cartesian(clip = "off") +
  theme_void() +
  theme(plot.title = element_text(family = font_t, size = 32, hjust = 0.5, face = "bold", color = "#000000"),
          plot.title.position = "plot",
          plot.subtitle = element_text(family = font, size = 14, hjust = 0.5, color = "#000000", margin = margin(t = 5, b = 30)),
          plot.caption.position = "plot",
          plot.caption = element_text(size = 9, family = font, color = "#000000", hjust = 0.5, margin = margin(t = 30)),
          legend.position = "none",
          panel.background = element_rect(color = NA, fill = "#FFFFFF"),
          plot.background = element_rect(color = NA, fill = "#FFFFFF"),
          aspect.ratio = 1/1.8) +
    labs(title = "Star Wars Movies",
         subtitle = "Chronological Order",
         caption = "#MayThe4thBeWithYou | Data: IMDb & Amazon | Design: Ryan Hart")

# save plot
ggsave(paste0("star_wars_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6/1)
