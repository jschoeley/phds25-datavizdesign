# Generate geodata and map templates for Ukrainian regional analyses

# Init ------------------------------------------------------------

library(tidyverse)
library(sf)
library(rnaturalearth)

# Constants -------------------------------------------------------

# input and output paths
paths <- list()
paths$input <- list(
  # district level geodata (sf) of Ukraine
  ukrgeo = './basemap/ukrgeo.rds',
  # positions of cities to highlight
  cities = './basemap/map_city_highlight.csv'
)
paths$output <- list(
  maptemplates = './basemap/maptemplates.rds'
)

# list containers for analysis artifacts
maptemplates <- list()

# Create region layer ---------------------------------------------

# district outlines in SF format
maptemplates$ukrgeo <-
  readRDS(paths$input$ukrgeo) %>%
  # just to be explicit we state that the coordinates are expressed
  # in latitude-longitude format, i.e. https://epsg.io/4326
  st_set_crs(4326) %>%
  rename(region_id = id_inc) %>%
  # project to Pseudo Mercator as used by OSM or Google Maps
  # https://epsg.io/3857
  st_transform(crs = st_crs(3857))
  
ggplot(maptemplates$ukrgeo) +
  geom_sf()

# Create background map centered on Ukraine -----------------------

maptemplates$background <-
  ne_countries(
    type = 'countries', returnclass = 'sf', scale = 'medium'
  ) %>%
  # just to be explicit we state that the coordinates are expressed
  # in latitude-longitude format, i.e. https://epsg.io/4326
  st_set_crs(4326) %>%
  # project to Pseudo Mercator as used by OSM or Google Maps
  # https://epsg.io/3857
  st_transform(crs = st_crs(3857)) %>%
  # crop to Ukraine and surrounding countries
  # coordinates of bounding box expressed in Mercator coordinates
  st_crop(
    y = c(xmin = 2430773, ymin = 5527516,
          xmax = 4519571, ymax = 6891042)
  )

ggplot(maptemplates$background) +
  geom_sf()

# Create spatial outline of Ukraine -------------------------------

maptemplates$outline <-
  maptemplates$ukrgeo %>%
  # avoid rendering artifacts
  st_buffer(10) %>%
  st_make_valid() %>%
  summarise(id = 'ukr') %>%
  st_union()

ggplot(maptemplates$outline) +
  geom_sf(fill = NA, lwd = 2.5, color = 'black') +
  #geom_sf(data = maptemplates$ukrgeo)
  theme_void()

# Create city layer -----------------------------------------------

# lat-lon coordinates of Ukrainian cities to highlight on map
maptemplates$cities <- read_csv(paths$input$cities)

maptemplates$cities <-
  # convert the data frame of lat-lon coordinates to an sf object
  # and project it to Mercator
  st_as_sf(
    maptemplates$cities,
    coords = c("longitude", "latitude"),
    crs = st_crs(4326)
  ) %>%
  st_transform(crs = st_crs(3857))

ggplot(maptemplates$cities) +
  geom_sf() +
  geom_sf_text(aes(label = city), hjust = -0.1, vjust = -0.1) +
  theme_void()

# Show all layers at once -----------------------------------------

backgroundmap <-
  ggplot() +
  # background
  geom_sf(
    data = maptemplates$background
  ) +
  # district outlines
  geom_sf(
    fill = 'grey95', lwd = 0.1, color = 'grey70',
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
ggsave(
  plot = backgroundmap,
  device = svglite::svglite, fix_text_size = FALSE,
  filename = 'backgroundmap/backgroundmap.svg',
  width = 175, units = 'mm'
)
