# Generate a basemap for France and combine it with TFR data

# Init ------------------------------------------------------------

library(tidyverse)
library(sf)
library(eurostat)
library(rnaturalearth)

# Constants -------------------------------------------------------

# input and output paths
paths <- list()
paths$input <- list(
  # positions of cities to highlight
  cities = './interactivetfr/frenchtfr/frenchcities.csv'
)
paths$output <- list(
  maptemplates = './interactivetfr/frenchtfr/shinydata.rds'
)

# list containers for analysis artifacts
maptemplates <- list()

# bounding box which crops to mainland France and surrounding countries
# coordinates of bounding box expressed in Pseudo-Mercator coordinates
# https://epsg.io/3857
bounding_box <- c(xmin = -550000, ymin = 5150000,
                  xmax = 953000, ymax = 6710000)

# Create region layer ---------------------------------------------

# district outlines in SF format
maptemplates$regions <-
  # just to be explicit we state that the coordinates are expressed
  # in latitude-longitude format, i.e. https://epsg.io/4326
  eurostat::get_eurostat_geospatial(
    nuts_level = '3', crs = 4326, resolution = 03
  ) |>
  # select only french regions
  filter(
    CNTR_CODE %in% 'FR'
  ) |>
  # project to Pseudo Mercator as used by OSM or Google Maps
  # https://epsg.io/3857
  st_transform(crs = st_crs(3857)) |>
  # crop to bounding box
  st_crop(y = bounding_box)

ggplot(maptemplates$regions) +
  geom_sf()

# Create background layer for map of France -----------------------

maptemplates$background <-
  # use the naturalearth package to download global geodata
  ne_countries(
    type = 'countries', returnclass = 'sf', scale = 'medium'
  ) %>%
  # just to be explicit we state that the coordinates are expressed
  # in latitude-longitude format, i.e. https://epsg.io/4326
  st_set_crs(4326) %>%
  # project to Pseudo Mercator as used by OSM or Google Maps
  # https://epsg.io/3857
  st_transform(crs = st_crs(3857)) %>%
  # crop to France and surrounding countries
  # coordinates of bounding box expressed in Mercator coordinates
  st_crop(y = bounding_box)

ggplot(maptemplates$background) +
  geom_sf()

# Create outline of France ----------------------------------------

# make a union of all regions belonging to France and return
# the outline of the resulting polygon
maptemplates$outline <-
  maptemplates$regions %>%
  summarise(id = 'fr') %>%
  st_union()

ggplot(maptemplates$outline) +
  geom_sf(fill = NA, lwd = 1, color = 'black') +
  theme_void()

# Create city layer -----------------------------------------------

# lat-lon coordinates of Ukrainian cities to highlight on map
maptemplates$cities <- read_csv(paths$input$cities)

maptemplates$cities <-
  # convert the data frame of lat-lon coordinates to an sf object
  # and project it to Mercator
  st_as_sf(
    maptemplates$cities,
    # names of the longitude and latitude variables in the cities
    # data, specifying longitude first
    coords = c("longitude", "latitude"),
    crs = st_crs(4326)
  ) %>%
  # project to pseudo Mercator
  st_transform(crs = st_crs(3857))

ggplot(maptemplates$cities) +
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
    fill = 'grey95', lwd = 0.1, color = 'grey70',
    data = maptemplates$regions
  ) +
  # france outline
  geom_sf(
    fill = NA, lwd = 1, color = 'grey50',
    data = maptemplates$outline
  ) +
  # cities
  geom_sf(
    data = maptemplates$cities,
    shape = 21, color = 'white', fill = 'black',
    size = 2.5
  ) +
  geom_sf_text(
    aes(label = city), hjust = -0.12, vjust = -0.12,
    color = 'white',
    data = maptemplates$cities
  ) +
  geom_sf_text(
    aes(label = city), hjust = -0.1, vjust = -0.1,
    data = maptemplates$cities
  ) +
  theme_void()

# Merge basemap with actual data ----------------------------------

# download data on fertility by NUTS-3 region
fert <- eurostat::get_eurostat('demo_r_find3')

# format data
fert <-
  fert |>
  mutate(year = lubridate::year(TIME_PERIOD)) |>
  filter(
    #year == 2023,
    # only select NUTS-3 regions (they have a five character long id)
    str_length(geo) == 5,
    indic_de == 'TOTFERRT'
  ) |>
  rename(tfr = values)

# merge with geodata on french NUTS 3 regions
maptemplates$fert <-
  left_join(maptemplates$regions, fert, by = 'geo')

# plot French tfr
frenchtfr <-
  ggplot() +
  # background
  geom_sf(
    data = maptemplates$background
  ) +
  # district outlines
  geom_sf(
    fill = 'grey95', lwd = 0.1, color = 'grey70',
    data = maptemplates$regions
  ) +
  # district tfr
  geom_sf(
    aes(fill = tfr), color = NA,
    data = maptemplates$fert
  ) +
  # france outline
  geom_sf(
    fill = NA, lwd = 0.8, color = 'black',
    data = maptemplates$outline
  ) +
  # cities
  geom_sf(
    data = maptemplates$cities,
    shape = 21, color = 'white', fill = 'black',
    size = 2.5
  ) +
  geom_sf_label(
    aes(label = city),
    hjust = -0.1, vjust = -0.1,
    alpha = 0.6,
    # no label outline
    label.size = 0,
    data = maptemplates$cities
  ) +
  scale_fill_viridis_c() +
  theme_void()
frenchtfr

# Export ----------------------------------------------------------

saveRDS(maptemplates, paths$output$maptemplates)
