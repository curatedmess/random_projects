# Merry Christmas 2023
# U.S. Christmas/Holiday Stamps
# #SWDChallenge | December 2023 | visualize holiday traditions

# the following code could probably be streamlined/improved, the flow is
# admittedly a bit of stream of conscious as I thought of different ideas

# libraries ---------------------------------------------------------------
library(tidyverse)
library(showtext)
library(DescTools)
library(ggfx)

# add font ----------------------------------------------------------------
font_add_google(name = "Inter", family = "Inter")
font <- "Inter"

font_add_google(name = "Chonburi", family = "Chonburi")
font_t <- "Chonburi"

font_add_google(name = "Caveat", family = "Caveat")
font_s <- "Caveat"

# turn on showtext --------------------------------------------------------
showtext_auto()
showtext_opts(dpi = 320)

# set seed ----------------------------------------------------------------

# John 3:16
set.seed(316)

# CREATE STAMP SHAPES -----------------------------------------------------

# Function to create squares ----------------------------------------------
create_squares <- function(xCenter, yCenter, size = 0.5) {
  lt <- c(xCenter - size, yCenter - size)
  rt <- c(xCenter + size, yCenter - size)
  rb <- c(xCenter + size, yCenter + size)
  lb <- c(xCenter - size, yCenter + size)
  
  data.frame(x = c(lt[1], rt[1], rb[1], lb[1], lt[1]), 
             y = c(lt[2], rt[2], rb[2], lb[2], lt[2]))
}

# set the point variable --------------------------------------------------
num_points <- 8

# Function to create points around lines ----------------------------------
create_line_points <- function(x_start, y_start, x_end, y_end, num_points) {
  normalized_index <- seq_along(1:num_points) / num_points  # Normalize index to achieve evenly spaced points
  data.frame(
    x = x_start + normalized_index * (x_end - x_start),
    y = y_start + normalized_index * (y_end - y_start)
  )
}

# initialize empty data frames --------------------------------------------
all_points <- data.frame()
squares_df <- data.frame()

# create grid of squares and points ---------------------------------------
my_grid <- crossing(x = 1:3, y = 1:3)

for (i in seq_along(my_grid$x)) {
  xCenter <- my_grid$x[i]
  yCenter <- my_grid$y[i]
  squares <- create_squares(xCenter, yCenter)
  
  # Create points along the segments of the square
  for (j in 1:(nrow(squares) - 1)) {
    x_start <- squares[j, "x"]
    y_start <- squares[j, "y"]
    x_end <- squares[j + 1, "x"]
    y_end <- squares[j + 1, "y"]
    
    points_along_segment <- create_line_points(x_start, y_start, x_end, y_end, num_points)
    
    # add square_id to points
    square_id <- paste0("Square_", i)
    points_along_segment$square_id <- square_id
    
    all_points <- bind_rows(all_points, points_along_segment)
  }
}

all_points_squares <- all_points

# create shape to cut out outside shape of shapes -------------------------
rectangle_df <- data.frame(
  xmin = min(all_points$x),
  xmax = max(all_points$x),
  ymin = min(all_points$y),
  ymax = max(all_points$y)
)

# get centroids for squares -----------------------------------------------

# initialize empty data frame ---------------------------------------------
centroid_data <- data.frame()

# function to create centroids --------------------------------------------
for (i in seq_along(my_grid$x)) {
  xCenter <- my_grid$x[i]
  yCenter <- my_grid$y[i]
  squares <- create_squares(xCenter, yCenter)
  
  # calculate centroid of the square
  centroid_x <- (max(squares$x) + min(squares$x)) / 2
  centroid_y <- (max(squares$y) + min(squares$y)) / 2
  
  # store centroid and square id
  square_id <- paste0("Square_", i)
  centroid_data <- bind_rows(centroid_data, data.frame(square_id = square_id, centroid_x, centroid_y))
}


# get the boundary for the point data -------------------------------------
min_x <- min(all_points$x)
max_x <- max(all_points$x)
min_y <- min(all_points$y)
max_y <- max(all_points$y)

# calculate the grid of points --------------------------------------------
df_points <- expand_grid(x = seq(from = min_x, to = max_x, length.out = 300), y = seq(from = min_y, to = max_y, length.out = 300))

jittered_points <- df_points %>%
  mutate(x = x + runif(n(), -0.1, 0.1), y = y + runif(n(), -0.1, 0.1))

# CREATE THE CHRISTMAS TREE SHAPES ----------------------------------------

# Function to create a Christmas tree
create_christmas_tree <- function(centroid_x, centroid_y, square_id) {
  tree <- data.frame(
    x = c(centroid_x - 0.3, centroid_x, centroid_x + 0.3),
    y = c(centroid_y - 0.3, centroid_y + 0.3, centroid_y - 0.3)
  )
  
  trunk <- data.frame(
    x = c(centroid_x, centroid_x),
    y = c(centroid_y - 0.4, centroid_y - 0.28),
    square_id = square_id
  )
  
  return(list(tree = tree, trunk = trunk))
}

# initialize empty data frames --------------------------------------------
tree_data <- data.frame()
trunk_data <- data.frame()
points_tree <- data.frame()

# create trees and trunks -------------------------------------------------
for (i in seq(nrow(centroid_data))) {
  
  # get centroid coordinates and square ID
  centroid_x <- centroid_data$centroid_x[i]
  centroid_y <- centroid_data$centroid_y[i]
  square_id <- centroid_data$square_id[i]  # Get square_id here
  
  # create Christmas tree data
  christmas_tree_data <- create_christmas_tree(centroid_x, centroid_y, square_id)
  
  # store tree and trunk data
  tree_data <- bind_rows(tree_data, christmas_tree_data$tree)
  trunk_data <- bind_rows(trunk_data, christmas_tree_data$trunk)
  
  # create tree of points for the current tree
  points_tree_i <- data.frame(PtInPoly(jittered_points, christmas_tree_data$tree)) %>% 
    filter(pip == 1)
  
  # add tree ID information
  points_tree_i$tree_id <- i
  
  # store points_tree data
  points_tree <- bind_rows(points_tree, points_tree_i)
}

# create two colors for the tree ------------------------------------------
points_tree_colors <- points_tree %>% 
  mutate(highlight = ifelse(runif(n()) < 0.06, 1, 2))

# sample points for the christmas lights ----------------------------------
points_tree_lights <- points_tree %>% 
  sample_frac(0.11)

# CREATE THE SNOWFALL SHAPES ----------------------------------------------

# function to create squares ----------------------------------------------
create_squares2 <- function(xCenter, yCenter, size = 0.4) {
  lt <- c(xCenter - size, yCenter - size)
  rt <- c(xCenter + size, yCenter - size)
  rb <- c(xCenter + size, yCenter + size)
  lb <- c(xCenter - size, yCenter + size)
  
  data.frame(x = c(lt[1], rt[1], rb[1], lb[1], lt[1]), 
             y = c(lt[2], rt[2], rb[2], lb[2], lt[2]))
}


# initialize empty data frames --------------------------------------------
points_squares <- data.frame()

# create the gird ---------------------------------------------------------
my_grid2 <- crossing(x = 1:3, y = 1:3)

# loop through each square
for (i in seq_along(my_grid2$x)) {
  xCenter <- my_grid2$x[i]
  yCenter <- my_grid2$y[i]
  squares <- create_squares2(xCenter, yCenter)
  
  # generate points within the square using PtInPoly
  points_square_i <- data.frame(PtInPoly(jittered_points, squares)) %>%
    filter(pip == 1)
  
  # add square ID information
  square_id <- paste0("Square_", i)
  points_square_i$square_id <- square_id
  
  # store points within squares data
  points_squares <- bind_rows(points_squares, points_square_i)
}

# reduce points,  create alpha and size variables -------------------------
points_squares_df <- points_squares %>% 
  sample_frac(0.005) %>% 
  mutate(alpha = runif(n(), min = 0.3, max = 0.8)) %>% 
  mutate(size = runif(n(), min = 0.5, max = 2))

# create list of possible stamp colors ------------------------------------
background_colors <- c("#CAA906", "#9BB2BA", "#0F160F", "#762D35")

# update data frame to include background colors --------------------------
my_grid <- my_grid %>%
  rowwise() %>%
  mutate(random_color = sample(background_colors, size = 1))

# CREATE STAMP LABELS -----------------------------------------------------

# create title ------------------------------------------------------------
merry_christmas <- centroid_data %>% 
  mutate(label = "Merry Christmas")

# create USA --------------------------------------------------------------
usa_2023 <- centroid_data %>% 
  mutate(label = "USA 2023")

# create FOREVER ----------------------------------------------------------
forever <- centroid_data %>% 
  mutate(label = "FOREVER")

# create plot -------------------------------------------------------------
ggplot() +
  
  # stamp base stuff
  geom_tile(data = my_grid, aes(x = x, y = y, fill = random_color), color = "#FFFFFF", linewidth = 8) +
  geom_tile(data = my_grid, aes(x = x, y = y), fill = NA, color = "#c8c8c8", linewidth = 0.3) +
  geom_point(data = all_points, aes(x = x, y = y), color = "#c8c8c8", size = 3) +
  geom_rect(data = rectangle_df, aes(xmin = xmin - 0.1, xmax = xmax + 0.1, ymin = ymin - 0.1, ymax = ymax + 0.1), fill = NA, color = "#c8c8c8", linewidth = 9.95) +
  geom_point(data = points_squares_df, aes(x, y, alpha = alpha, size = size), color = "#FFFFFF", stroke = 0) +
  
  # geom_point(data = centroid_data, aes(x = centroid_x, y = centroid_y), color = "red", size = 10) +
  # geom_text(data = centroid_data, aes(x = centroid_x, y = centroid_y, label = square_id), family = font, color = "white", size = 3) +
  
  # labels
  geom_text(data = merry_christmas, aes(x = centroid_x, y = centroid_y + 0.35, label = label), family = font_t, color = "#FFFFFF", size = 3.25) +
  geom_text(data = usa_2023, aes(x = centroid_x - 0.4, y = centroid_y - 0.39, label = label), family = font, fontface = "bold", color = "#FFFFFF", size = 1.35, hjust = 0) +
  geom_text(data = forever, aes(x = centroid_x + 0.24, y = centroid_y - 0.39, label = label), family = font, fontface = "bold", color = "#FFFFFF", size = 1.35, hjust = 0) +
  
  # tree image
  geom_polygon(data = trunk_data, aes(x = x, y = y, group = square_id), fill = NA, color = "#6B492B", linewidth = 2) +
  geom_point(data = points_tree_colors, aes(x = x, y = y, color = factor(highlight)), size = 0.8, shape = 4) +
  with_outer_glow(geom_point(data = points_tree_lights, aes(x = x, y = y), color = "#efead2", size = 0.01, shape = 8), colour = "#fbf8f0", sigma = 2, expand = 1) +
  
  scale_alpha_identity() +
  scale_size_identity() +
  scale_fill_identity() +
  scale_color_manual(values = c("#466f4b", "#295849")) +
  coord_equal(clip = "off") +
  theme_void() +
  theme(plot.caption = element_text(family = font_s, hjust = 0.5, size = 15, color = "#000000", margin = margin(t = 0)),
        plot.caption.position = "plot",
        legend.position = "none",
        plot.margin = unit(c(0.5, 0.5, 0.75, 0.5), "cm"),
        panel.background = element_rect(color = NA, fill = "#c8c8c8"),
        plot.background = element_rect(color = NA, fill = "#c8c8c8")) +
  labs(caption = "Merry Christmas, Ryan")

# save plot ---------------------------------------------------------------
ggsave(paste0("merry_christmas_stamps_", format(Sys.time(), "%d%m%Y"), ".png"), dpi = 320, width = 6, height = 6)

