# Population of bike delivery workers in NYC
# Data Source is census.gov

# inspiration
# https://www.cnn.com/2023/05/05/business/delivery-workers-new-york-bathrooms-ebikes/index.html

# libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)

# add font ----------------------------------------------------------------
font_add_google(name = "Source Code Pro", family = "Source Code Pro")
font <- "Source Code Pro"

font_add_google(name = "Gluten", family = "Gluten")
font_t <- "Gluten"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

options(scipen = 999)

# load data ---------------------------------------------------------------
data <- readr::read_csv("SUB-IP-EST2021-POP.csv")

# wrangle data ------------------------------------------------------------
df <- data %>% 
  select(1:2) %>% 
  add_row(Location = "Bike Delivery Workers", Population = 65000) %>%
  mutate(Location = toupper(Location)) %>% 
  filter(Population > 0) %>% 
  arrange(desc(Population)) %>% 
  mutate(rank = row_number()) %>% 
  filter(rank > 574 & rank < 619) %>% 
  mutate(alpha = ifelse(rank == 589, 1, (100 - abs(rank - 589))/ 100))
  
# plot data ---------------------------------------------------------------
df %>%
  ggplot() +
  geom_text(aes(x = 10.5, y = -rank, label = paste0(rank, ": ", Location), fontface = ifelse(rank == 589, "bold", "plain"), alpha = alpha), size = 2.75, family = font, hjust = 0) +
  annotate("text", x = 0, y = -582, hjust = 0, vjust = "top", label = "If the estimated 65,000 NYC\nbike delivery workers were\nto populate their own U.S.\ncity, they'd rank 589 out of\n19,487 incorporated places,\naccording to July 1, 2021\npopulation census data.", family = font_t, size = 4, fontface = "bold") +
  scale_color_identity() +
  scale_x_continuous(limits = c(0, 20)) +
  theme_void() +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(size = 8, family = font, hjust = 0.5, margin = margin(t = 20)),
        legend.position = "none",
        plot.margin = unit(c(0.5, 0.25, 0.5, 0.25), "cm"),
        plot.background = element_rect(color = NA, fill =  "#C0D6E4")) +
  labs(caption = "Data: census.gov | Design: Ryan Hart")

# save plot ---------------------------------------------------------------
ggsave(paste0("bike_delivery_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)





