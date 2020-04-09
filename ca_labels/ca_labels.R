library(dplyr)
library(sf)
library(tmap)

# Read in CA counties GeoJSON
ca <- st_read("../data/caCountiesNoChannelIslands.geojson")
head(ca)

# Drop and rename some columns in ca dataframe, only want to keep the 3rd and 6th column
ca <-select(ca,-c(1,2,4,5)) %>% 
  rename("county" = "NAME")
head(ca)

# Create outline of CA for possible use
ca_outline <- st_union(ca)

# Vector to subset dataframe with
filter_vector <- c('Siskiyou', 'Humboldt', 'Mendocino', 'Napa', 'Solano', 'Sonoma', 'Marin', 'Contra Costa')

# Subset ca data frame from filter_vector
ca_subset <- subset(ca, county %in% filter_vector)

# Plot a map
# main ca layer
ca_map <- tm_shape(ca) +
  tm_fill("white") +
  tm_borders('black', lwd = .5) +
  # subsetted ca layer
  tm_shape(ca_subset) +
  tm_fill("lightblue") +
  tm_borders('black', lwd=.5) +
  tm_shape(ca) +
  tm_text(
    'county',
    size = .5,
    col = 'black'
  ) + 
  # specify some layout parameters
  tm_layout(frame = FALSE, bg.color = '#f3ebe1')

# Save map to file and specify the size be 8.5x11"
tmap_save(ca_map, "../data/output/r_ca_labels_color_polys.png", 8.5,11)
# Save to SVG so we can then adjust labels in somehething like InkScape
tmap_save(ca_map, "../data/output/r_ca_labels_color_polys.svg", 8.5,11)
