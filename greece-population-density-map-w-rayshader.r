# libraries we'll need
library(tidyverse)
library(ggplot2)

# Load the sf library to read .gpkg data
library(sf)

# Use the stars package to rasterize geometries
library(stars)

# Package for producing 2D and 3D data visualizations
library(rayshader)

# Color palettes based on objects from the Metropolitan Museum of Art in New York!
library(MetBrewer)

# Toolbox for selecting and manipulating individual colors or color palettes
library(colorspace)

install.packages("PrettyCols")

# Load kontour data
data <- st_read("/kaggle/input/greece-population-density-for-400m-h3-hexagons/kontur_population_GR_20231101.gpkg")

glimpse(data)

# combine the xmin and ymin values into a vector
bottom_left <- st_point(c(bb[["xmin"]], bb[["ymin"]])) |> 
# use st_crs to retrieve coordinate reference system from object
# use st_sfc to create simple feature geometry list column
  st_sfc(crs = st_crs(data))

bottom_right <- st_point(c(bb[["xmax"]], bb[["ymin"]])) |> 
  st_sfc(crs = st_crs(data))

# check by plotting points

data |> 
# create a graph
  ggplot() +
# add a spatial geometry layer to the plot
  geom_sf() +
  geom_sf(data = bottom_left) +
  geom_sf(data = bottom_right, color = "red")

# Calculate the width of the aspect ratio

width <- st_distance(bottom_left, bottom_right)

# Calculate the height of the aspect ratio

top_left <- st_point(c(bb[["xmin"]], bb[["ymax"]])) |> 
  st_sfc(crs = st_crs(data))

height <- st_distance(bottom_left, top_left)

# handle conditions of width or height being the longer side

if (width > height) {
  w_ratio <- 1
  h_ratio <- height / width
} else {
  h_ration <- 1
  w_ratio <- width / height
}

# convert to raster so we can then convert to matrix

size <- 5000

greece_rast <- st_rasterize(data, 
                             nx = floor(size * w_ratio),
                             ny = floor(size * h_ratio))

# create a matrix
mat <- matrix(greece_rast$population, 
              nrow = floor(size * w_ratio),
              ncol = floor(size * h_ratio))

# create color palette
pal <- "day_night"

night <- "#4e82b4"
day <- "#fff5b6"
    
  
colors <- c(night, day, "white")
swatchplot(colors)
  
texture <- grDevices::colorRampPalette(colors, bias = 3)(256)
swatchplot(texture)

# plot to 3d

mat |>
  height_shade(texture = texture) |>
  plot_3d(heightmap = mat,
          zscale = 100 / 5,
          solid = FALSE,
          shadowdepth = 0,
          phi = 90, 
          zoom = 1, 
          theta = 0, 
          background = "white")

render_camera(theta = -20, phi = 45, zoom = .8)

outfile <- "/kaggle/working/final_plot.png"

render_highquality(
    filename = outfile,
    interactive = FALSE,
    light = TRUE,
    lightdirection = c(270, 270, 260, 260),
    lightcolor = c(night, "white", day, "white"),
    lightintensity = c(600, 50, 1000, 50),
    lightaltitude = c(10, 80, 10, 80),
    width = 2000,
    height = 2000
)
