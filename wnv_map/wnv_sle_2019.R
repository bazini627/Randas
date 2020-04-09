library(rvest)
library(tmap)
library(sf)
library(dplyr)

# Read in SLE/WNV tables for 2019
sle <- read_html("http://www.westnile.ca.gov/web_reports.php?report=sle&option=print&year=2019")
wnv <- read_html("http://westnile.ca.gov/case_counts.php?year=2019&option=print")

# Convert table to data frames
sle <- sle %>%
  html_node("table") %>%
  html_table(header = TRUE)

wnv <- wnv %>%
  html_node("table") %>%
  html_table(header = TRUE)

# Only care about rows 2 to 60 so slice those off of each data frame
sle <- slice(sle, 2:59)
wnv <- slice(wnv, 2:59)

# Change the column names
sle <-
  sle %>% rename(
    HumanCases = 'Human Cases',
    MosquitoSamples = 'Mosquito Samples',
    SentinelChickens = 'Sentinel Chickens'
  )
wnv <-
  wnv %>% rename(
    HumanCases = 'Human Cases',
    DeadBirds = 'Dead Birds',
    MosquitoSamples = 'Mosquito Samples',
    SentinelChickens = 'Sentinel Chickens'
  )

# Change County column to a factor to be joined later with county geojson
sle$County <- as.factor(sle$County)
wnv$County <- as.factor(wnv$County)

# Convert dashes to zeros
sle[2:4] <-
  lapply(sle[2:4], function(col)
    as.numeric(gsub("-", 0, col)))
wnv[2:5] <-
  lapply(wnv[2:5], function(col)
    as.numeric(gsub("-", 0, col)))

# Output data frame to csv
write.csv(sle, "../data/output/sleData2019.csv", row.names = FALSE)
write.csv(wnv, "../data/output/wnvData2019.csv", row.names = FALSE)

# Read in CA county geojson
ca <- st_read("../data/caCountiesNoChannelIslands.geojson")

# Drop some columns in ca dataframe and pipe to rename the NAME column to County to join to sle/wnv data frames
ca <- select (ca, -c(COUNTYNS, ALAND, AWATER)) %>%
  rename(County = NAME)

# Join the sle/wnv and ca dataframes on County column
sle_ca_joined <- inner_join(sle, ca, by = "County")
wnv_ca_joined <- inner_join(wnv, ca, by = "County")

# Make joined dataframes sf objects so they can be plotted with tmap
sle_ca_joined <- st_as_sf(sle_ca_joined)
wnv_ca_joined <- st_as_sf(wnv_ca_joined)

# Change CRS to CA projection To CA Albers (EPSG 3310)
st_transform(sle_ca_joined, 3310)
st_transform(wnv_ca_joined, 3310)

# Bubble Map For WNV Human
wnv_human_bubble <- tm_shape(wnv_ca_joined) +
  tm_polygons(
    col = '#333333',
    ) +
  tm_bubbles(
    col = "salmon",
    size = "HumanCases",
    scale = 5,
    title.size = "Positive Specimens",
    alpha = 0.5,
    border.col = 'black',
  ) +
  tm_text(
    "HumanCases",
    size = "HumanCases",
    root = 5,
    legend.size.show = FALSE,
    col = 'black'
  ) +
  tm_layout(
    main.title = "California WNV Positive Human Cases 2019 YTD",
    main.title.position = c("center"),
    main.title.size = 1,
    main.title.color = 'white',
    legend.show = FALSE,
    frame = FALSE,
    bg.color = 'black'
  ) +
  tm_credits(
    "Data Source:westnile.ca.gov\nCRS:California Albers (EPSG:3310)",
    size = .5 ,
    position = c("left", "bottom"),
    col = 'white'
  )

# Save to png
tmap_save(wnv_human_bubble, "../data/output/r_wnvHumanBubble2019.png")

