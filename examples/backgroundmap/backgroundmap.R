# Generate geodata and map templates for Ukrainian regional analyses

# Init ------------------------------------------------------------

library(tidyverse)
library(sf)
library(rnaturalearth)

# Constants -------------------------------------------------------

# input and output paths
paths <- list()
paths$input <- list(
  glob = './backgroundmap/00-global_functions.R',
  # district level geodata (sf) of Ukraine
  ukrgeo = './backgroundmap/ukrgeo.rds',
  # positions of cities to highlight
  cities = './backgroundmap/map_city_highlight.csv'
)
paths$output <- list(
  maptemplates = './backgroundmap/maptemplates.rds'
)

# global objects
source(paths$input$glob)

# list containers for analysis artifacts
maptemplates <- list()

# Import Ukrainian geo-data ---------------------------------------

# district outlines in SF format
maptemplates$ukrgeo <-
  readRDS(paths$input$ukrgeo) %>%
  rename(region_id = id_inc)

ggplot(maptemplates$ukrgeo) +
  geom_sf(fill = NA, lwd = 0.5, color = 'black') +
  theme_void()

# lat-lon coordinates of Ukrainian cities to highlight on map
maptemplates$cities <- read_csv(paths$input$cities)

# Create background map centered on Ukraine -----------------------

maptemplates$background <-
  ne_countries(
    type = 'countries', returnclass = 'sf', scale = 'medium'
  ) %>%
  st_set_crs(4326) %>%
  st_crop(
    y = st_bbox(
      c(xmin = 20.6, xmax = 40.6, ymax = 40.7, ymin = 52.5),
      crs = st_crs(4326)
    )
  ) %>%
  st_transform(crs = st_crs(3857)) %>%
  st_crop(
    y = c(xmin = 2293182*1.06, ymin = 5419133*1.02,
          xmax = 4519571, ymax = 6891042)
  )

ggplot(maptemplates$background) +
  geom_sf() +
  theme_void()

# Create spatial outline of Ukraine -------------------------------

maptemplates$outline <-
  maptemplates$ukrgeo %>%
  # avoid rendering artifacts
  st_buffer(0.0001) %>%
  st_make_valid() %>%
  summarise(id = 'ukr') %>%
  st_union()

ggplot(maptemplates$outline) +
  geom_sf(fill = NA, lwd = 0.5, color = 'black') +
  theme_void()

# Create city layer -----------------------------------------------

maptemplates$cities <- st_as_sf(
  maptemplates$cities,
  coords = c("longitude", "latitude"),
  crs = st_crs(4326)
) %>%
  st_transform(crs = st_crs(3857))

ggplot(maptemplates$cities) +
  geom_sf(
    fill = NA, lwd = 0.5, color = 'black',
    data = maptemplates$outline
  ) +
  geom_sf() +
  geom_sf_text(aes(label = city), hjust = -0.1, vjust = -0.1) +
  theme_void()

# Show all layers at once -----------------------------------------

ggplot() +
  # background
  geom_sf(
    data = maptemplates$background
  ) +
  # district outlines
  geom_sf(
    fill = NA, lwd = 0.1, color = 'grey70',
    data = maptemplates$ukrgeo
  ) +
  # ukraine outline
  geom_sf(
    fill = NA, lwd = 1, color = 'grey50',
    data = maptemplates$outline
  ) +
  # cities
  geom_sf(
    data = maptemplates$cities
  ) +
  geom_sf_text(
    aes(label = city), hjust = -0.1, vjust = -0.1,
    data = maptemplates$cities
  ) +
  theme_void()

# Export ----------------------------------------------------------

saveRDS(maptemplates, paths$output$maptemplates)
