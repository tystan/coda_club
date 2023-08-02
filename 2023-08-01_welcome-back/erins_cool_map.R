
### code thanks to Erin!

### if you need to install the packages:
# install.packages("ggplot2")
# install.packages("ozmaps")
# install.packages("sf")

library(ggplot2)
library(ozmaps)
library(sf)

# Download the map data for Australia and split into States 
# can also choose LGAs if you want 
aus <- ozmap_states
# sf_oz <- ozmap_data("states")

# create random data table - you can modify or import your own dataframe :) 
rand_data <- data.frame(
  NAME = c(
    'New South Wales', 'Victoria', 'Queensland', 
    'Western Australia', 'South Australia', 'Tasmania', 
    'Australian Capital Territory', 'Northern Territory'
  ),
  Value = c(10, 1, 7, 12, 8, 5, 3, 2)
)

# Merge the data with the map data
merged <- merge(aus, rand_data, by.x = "NAME")

erins_plot <-
  ggplot() + 
  geom_sf(data = merged, 
          aes(fill = Value)) +
  coord_sf(crs = "+proj=lcc +lon_0=135 +lat_0=-30 +lat_1=-10 +lat_2=-45 +datum=WGS84") +
  labs(fill = "Clinic density") +
  scale_fill_gradient(low = "lightblue", high = "magenta")

# look at the plot:
erins_plot

# saving a png file for the github as smaller size than tiff
ggsave(
  filename = "2023-08-01_welcome-back/erins_cool_map.png", 
  plot = erins_plot,
  units = "in", width = 8, height = 8, dpi = 300
)
### Use the below commented line to plot the heat map and save as tiff file (16 MB)
# ggsave("2023-08-01_welcome-back/Wip1.tiff", units = "in", width = 8, height = 8, dpi = 300)



