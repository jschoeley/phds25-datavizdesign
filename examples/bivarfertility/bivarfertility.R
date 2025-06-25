# Generate a bivariate map of German regional TFR vs mean age at birth

# Init ------------------------------------------------------------

library(tidyverse)
library(sf)
library(eurostat)
library(rnaturalearth)
library(biscale)
library(cowplot)

# Constants -------------------------------------------------------

# bounding box which crops to mainland Germany and surrounding countries
# coordinates of bounding box expressed in Pseudo-Mercator coordinates
# https://epsg.io/3857
bounding_box <- c(xmin = 594374, ymin = 5931513,
                  xmax = 1702405, ymax = 7378313)

# Create region layer ---------------------------------------------

# district outlines in SF format
regions <-
  # just to be explicit we state that the coordinates are expressed
  # in latitude-longitude format, i.e. https://epsg.io/4326
  eurostat::get_eurostat_geospatial(
    nuts_level = '3', crs = '4326', resolution = '03'
  ) |>
  # select only german regions
  filter(
    CNTR_CODE %in% 'DE'
  ) |>
  # project to Pseudo Mercator as used by OSM or Google Maps
  # https://epsg.io/3857
  st_transform(crs = st_crs(3857)) |>
  # crop to bounding box
  st_crop(y = bounding_box)

ggplot(regions) +
  geom_sf()

# Create basemap --------------------------------------------------

basemap <-
  # use the naturalearth package to download global geodata
  ne_countries(
    type = 'countries', returnclass = 'sf', scale = 'medium'
  ) |>
  # just to be explicit we state that the coordinates are expressed
  # in latitude-longitude format, i.e. https://epsg.io/4326
  st_set_crs(4326) |>
  # project to Pseudo Mercator as used by OSM or Google Maps
  # https://epsg.io/3857
  st_transform(crs = st_crs(3857)) |>
  # crop to France and surrounding countries
  # coordinates of bounding box expressed in Mercator coordinates
  st_crop(y = bounding_box)

ggplot(basemap) +
  geom_sf()

# Outline of Germany ----------------------------------------------

# make a union of all regions belonging to Germany and return
# the outline of the resulting polygon
outline <-
  regions %>%
  summarise(id = 'de') %>%
  st_union()

ggplot(outline) +
  geom_sf(fill = NA, lwd = 1, color = 'black') +
  theme_void()

# Merge regions with fertility data -------------------------------

# download data on fertility by NUTS-3 region
fert <- eurostat::get_eurostat('demo_r_find3')

# format data
fert <-
  fert |>
  mutate(year = lubridate::year(TIME_PERIOD)) |>
  filter(
    year == 2022,
    # only select NUTS-3 regions (they have a five character long id)
    str_length(geo) == 5,
    str_sub(geo, 1, 2) == 'DE',
    indic_de %in% c('TOTFERRT', 'AGEMOTH')
  ) |>
  select(geo, indic_de, values) |>
  spread(key = indic_de, value = values)

# merge with geodata on german NUTS 3 regions
fert <-
  left_join(regions, fert, by = 'geo')

# Define bivariate color scale ------------------------------------

data <- bi_class(
  fert,
  x = TOTFERRT,
  y = AGEMOTH,
  style = "quantile",
  dim = 3
)

breaks <-
  bi_class_breaks(
    data,
    x = TOTFERRT,
    y = AGEMOTH,
    style = "quantile", 
    dim = 3,
    dig_lab = 2,
    split = TRUE
  )

# Plot bivariate map ----------------------------------------------

bivarmap <-
  ggplot() +
  # basemap
  geom_sf(
    data = basemap
  ) +
  # district tfr & mean age at birth
  geom_sf(
    aes(fill = bi_class),
    data = data,
    color = 'white', linewidth = 0.1, show.legend = FALSE
  ) +
  # outline
  geom_sf(
    fill = NA, lwd = 0.6, color = 'black',
    data = outline
  ) +
  bi_scale_fill(pal = 'BlueGold', dim = 3) +
  theme_void()

legend <-
  bi_legend(
    pal = 'BlueGold',
    dim = 3,
    xlab = 'TFR ',
    ylab = 'Mean age at birth ',
    size = 8,
    breaks = breaks
  ) +
  theme(plot.background = element_blank())

bivarfertility <-
  ggdraw() +
  draw_plot(bivarmap, 0, 0, 1, 1) +
  draw_plot(legend, 0.65, 0.65, 0.3, 0.3)

# Export ----------------------------------------------------------

ggsave(
  filename = './bivarfertility/bivarfertility.svg',
  plot = bivarfertility,
  device = svglite::svglite,
  width = 175,
  fix_text_size = FALSE,
  units = 'mm'
)
