# Bubble grid map of European population count changes
# Jonas Schöley (2018)

# Init ------------------------------------------------------------

library(eurostat)      # eurostat data
library(rnaturalearth) # worldwide map data
library(tidyverse)     # tidy data transformation
library(lubridate)     # date and time support
library(sf)            # simple features GIS

# Download data ---------------------------------------------------

# download eurostat data of population counts by NUTS-3 region
euro_pop <-
  get_eurostat('demo_r_pjanaggr3', stringsAsFactors = FALSE) %>%
  filter(sex == 'T',
         str_length(geo) == 5, # NUTS-3
         age == 'TOTAL')

# download geospatial data for NUTS-3 regions
euro_nuts3_sf <-
  get_eurostat_geospatial(output_class = 'sf', year = '2021',
                          resolution = '60', nuts_level = 3) %>%
  st_transform(crs = 3035)

# download geospatial data for European and Asian countries
backgroundmap <-
  ne_countries(continent = c('europe', 'asia'), returnclass = 'sf') %>%
  st_transform(crs = 3035)

# calculate difference in absolute population numbers from 2012 to 2017
euro_pop_diff <-
  euro_pop %>%
  filter(year(TIME_PERIOD) %in% c(2012, 2017)) %>%
  spread(TIME_PERIOD, values) %>%
  mutate(pop_diff = `2017-01-01` - `2012-01-01`) %>%
  drop_na()

# Interpolate population data to regular grid ---------------------

# divide the european continent into a 150 by 150 cell grid
euro_grid <-
  st_make_grid(euro_nuts3_sf, n = 150)

# interpolate data to grid
popgrid_sf <-
  euro_nuts3_sf %>%
  # join geodata with population data
  left_join(y = euro_pop_diff, by = c('id' = 'geo')) %>%
  # for st_interpolate_aw to work, only a single variable is allowed
  select(pop_diff) %>%
  # sum population differences from polygons into regular grid cells
  # using areal weighting. total population difference is
  # preserved (extensive=TRUE)
  st_interpolate_aw(to = euro_grid, extensive = TRUE) %>%
  # this results in larger points being plotted below smaller points
  arrange(abs(pop_diff))
  
# Bubble grid map -------------------------------------------------

bubblegrid_plot <-
  popgrid_sf |>
  # add centroids of each grid cell for plotting
  st_centroid() |>
  ggplot() +
  geom_sf(data = backgroundmap, color = 'white', fill = 'grey95') +
  geom_sf(aes(size = abs(pop_diff),
              fill = ifelse(pop_diff >= 0, 'pos', 'neg')),
          shape = 21, color = 'grey95', show.legend = 'point') +
  coord_sf(xlim = c(2.5e6, 7e6), ylim = c(1.35e6,5.55e6), datum = NA) +
  scale_size_area(
    'Population change\n2012 to 2017\ndecline red, increase blue',
    max_size = 8,
    breaks = c(1e3, 1e4, 1e5, 5e5),
    labels = c('1,000', '10,000', '100,000', '500,000'),
    guide = guide_legend(override.aes = list(color = 'black',
                                             fill = 'black'))
  ) +
  scale_fill_manual(values = c(pos = '#2166AC', neg = '#B2182B'),
                    guide = FALSE) +
  theme_void() +
  theme(legend.position = c(0.83, 0.7)) +
  labs(caption = 'Data: Eurostat')

bubblegrid_plot

# Choropleth grid -------------------------------------------------

# choropleth-grid-map
breaks = c(-Inf, -1e5, -1e4, 0, 1e4, 1e5, Inf)
labels = c('-100,000 or less',
           '-10,000 to -100,000', '0 to -10,000',
           '0 to 10,000', '10,000 to 100,000',
           '100,000 or more')

plot_choro_grid <-
  popgrid_sf |>
  ggplot() +
  geom_sf(data = backgroundmap, color = 'white', fill = 'grey95') +
  geom_sf(aes(fill = cut(pop_diff, breaks, labels)), color = NA) +
  coord_sf(xlim = c(2.5e6, 7e6), ylim = c(1.35e6,5.55e6), datum = NA) +
  scale_fill_brewer(name = 'Population change\n2012 to 2017',
                    type = 'div', palette = 5,
                    breaks = labels, # to omit the NA level
                    guide = guide_legend(reverse = TRUE)) +
  theme_void() +
  theme(legend.position = c(0.83, 0.7)) +
  labs(caption = 'Data: Eurostat')

# Export ----------------------------------------------------------

ggsave(
  './bubblegrid/bubblegrid.svg', bubblegrid_plot,
  device = svglite::svglite,
  fix_text_size = FALSE,
  width = 7, height = 7
)
