# Merry Christmas 2022
# updated version of a Christmas Tree plot created in 2021

# load libraries ----------------------------------------------------------
library(tidyverse)
library(showtext)
library(ggtext)
library(gganimate)

# add font
font_add_google(name = "Ribeye", family = "Ribeye")
font_add_google(name = "Inter", family = "Inter")
font <- "Ribeye"
font2 <- "Inter"

# turn on showtext
showtext_auto()
showtext_opts(dpi = 320)

# load file with xy points for tree shape ---------------------------------
data <- readr::read_csv("O_Christmas_Tree_Lights.csv")

# create function ---------------------------------------------------------
make_lights <- function(blinking) {
  
  map_df(blinking, function(i) {
    set.seed(i)
    data %>%
      mutate(c = factor(sample(1:4, 706, replace = TRUE))) %>%
      mutate(a = runif(706, min = 0.3, max = 0.9)) %>%
      mutate(s = runif(706, min = 0.07, max = 1.8)) %>%
      mutate(group_num = i)
    
  }) -> lights
  
  return(lights)
  
}

initial_seed = 74
blinking <- sample(1:706, 35, replace = FALSE)

# create the df -----------------------------------------------------------
df <- make_lights(blinking)

# create plot -------------------------------------------------------------
tree <- ggplot() +
  geom_point(aes(x = 260, y = 20), shape = "\u2605", size = 8, color = "#E6C797") +
  annotate(geom = "segment", x = 260, xend = 260, y = 45, yend = 20, color = "#E6C797") +
  geom_point(data = df, aes(x = X, y = Y, group = group_num, color = as.factor(c)), alpha = df$a, size = df$s, shape = 16) +
  scale_color_manual(values = c("#0A53DE", "#24C024", "#FBF21A", "#EA0D0D")) +
  scale_y_continuous(limits = c(780, -5), trans = scales::reverse_trans()) +
  scale_x_continuous(limits = c(-60, 580)) +
  annotate(geom = "text", x = 260, y = 745, label = "Merry Christmas", color = "#E6C797", family = font, size = 9, hjust = 0.5) +
  annotate(geom = "text", x = 260, y = 778, label = "\n\nChristmas 2022 | Ryan Hart", color = "#F2F2F2", family = font2, size = 2, hjust = 0.5) +
  theme_void() +
  theme(legend.position = "none",
        plot.margin = unit(c(0, 0, 0.25, 0), "cm"),
        plot.background = element_rect(color = NA, fill = "#28282B"),
        panel.background = element_rect(color = NA, fill = "#28282B"))

# create animation object -------------------------------------------------
animation <- tree + transition_states(group_num) + ease_aes("linear")

# animate ----------------------------------------=------------------------
animate(animation, width = 360, height = 432, res = 72, fps = 25, duration = 10, end_pause = 20)

# create gif --------------------------------------------------------------
anim_save("merry_christmas_2022.gif")

