# #PiDay - 3.14.2024
# First 100 digits

library(tidyverse)
library(showtext)
library(ggtext)

# add font ————————————————————————————————————————————————————————————————
font_add_google(name = "Sriracha", family = "Sriracha")
font2 <- "Sriracha"

font_add_google(name = "Roboto", family = "Roboto")
font3 <- "Roboto"

# turn on showtext ————————————————————————————————————————————————————————
showtext_auto()
showtext_opts(dpi = 320)

options(scipen = 999)


# use this as a starting point for the morse code -------------------------
# https://stackoverflow.com/questions/70856576/r-creating-a-conversion-function-for-morse-code

# create df of digits to morse code ---------------------------------------
morse <- structure(list(
  character = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9"), 
  code = c("- - - - -", "• - - - -", "• • - - -", "• • • - -", "• • • • -", "• • • • •", "- • • • •", "- - • • •", "- - - • •", "- - - - •")), 
  row.names = c(NA, 9L), 
  class = "data.frame")

# morse code function -----------------------------------------------------
text_to_morse <- function(text, morse_df) {
  
  # split
  characters <- strsplit(text, NULL)[[1]]
  
  # lookup
  morse_codes <- morse_df$code[match(characters, morse_df$character)]
  
  return(morse_codes)
}

# create 100 digits of pi -------------------------------------------------
code <- "1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679"

# replace digit with morse code -------------------------------------------
morse_codes <- text_to_morse(code, morse)

# create data frame of morse codes ----------------------------------------
morse_df <- data.frame(morse_code = unlist(morse_codes)) %>% 
  mutate(row_num = row_number())

# create data from for 3. -------------------------------------------------
df <- data.frame(x = c(-2.5), digits = c("3."))

# create the plot ---------------------------------------------------------
ggplot() +
  geom_text(data = morse_df, aes(y = "", x = row_num, label = morse_code), family = font3, size = 5, color = "#000000", angle = 90, hjust = 0, fontface = "bold") +
  geom_text(data = df, aes(y = "", x = x, label = digits), family = font2, size = 10, color = "#000000", vjust = 0) +
  coord_radial(r_axis_inside = TRUE, inner.radius = 0.4, expand = FALSE, rotate_angle = TRUE) +
  scale_x_continuous(limits = c(-3, 103)) +
  theme_void() +
  theme(plot.caption = element_markdown(family = font3, hjust = 0.5, size = 7, color = "#000000", margin = margin(t = 5), lineheight = 4),
        plot.caption.position = "plot",
        legend.position = "none",
        plot.margin = unit(c(0.5, 0.5, 1, 0.5), "cm"),
        panel.background = element_rect(color = NA, fill = "#FFFFFF"),
        plot.background = element_rect(color = NA, fill = "#FFFFFF")) +
  labs(caption = "<span style = 'font-family:Sriracha;font-size:20px;'>100 digits of π</span><br>3.14.2024 | Design: Ryan Hart")

# save plot ———————————————————————————————————————————————————————————————
ggsave(paste0("pi_2024_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)

