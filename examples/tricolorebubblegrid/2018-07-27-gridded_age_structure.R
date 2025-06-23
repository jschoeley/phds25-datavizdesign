# Grid interpolation of European regional age structure
# Jonas Schöley
# 2018-07-27

# Init --------------------------------------------------------------------

library(eurostat)      # eurostat data
library(rnaturalearth) # worldwide map data
library(tidyverse)     # tidy data transformation
library(lubridate)     # date and time support
library(sf)            # simple features GIS

# Download data -----------------------------------------------------------

# download the data on pop counts by age at NUTS-3 level
euro_age <-
  get_eurostat('demo_r_pjanaggr3', stringsAsFactors = FALSE) %>%
  # filter NUTS-3, 2015, total population
  filter(sex == 'T', str_length(geo) == 5,  year(time) == 2015,
         age %in% c('Y_LT15', 'Y15-64', 'Y_GE65', 'TOTAL')) %>%
  select(age, geo, values) %>%
  spread(age, values) %>%
  rename(total = TOTAL, age65plus = Y_GE65, age0to15 = Y_LT15, age15to65 = `Y15-64`)

# download geospatial data for NUTS-3 regions
# and project to crs 3035
euro_nuts3_sf <-
  get_eurostat_geospatial(output_class = 'sf',
                          resolution = '60', nuts_level = 3) %>%
  st_transform(crs = 3035)

# download geospatial data for European, Asian and African countries
eura <-
  ne_countries(continent = c('europe', 'asia', 'africa'), returnclass = 'sp') %>%
  fortify() %>%
  rename(longitude = long, latitude = lat)

# divide the european continent into various grids
# 100 by 100 km rectangular grid
euro_grid_rect_100x100km <-
  st_make_grid(euro_nuts3_sf, crs = 3035, what = 'polygons', cellsize = 100e3)
# 50 by 50 km rectangular grid
euro_grid_rect_50x50km <-
  st_make_grid(euro_nuts3_sf, crs = 3035, what = 'polygons', cellsize = 50e3)
# 25 by 25 km rectangular grid
euro_grid_rect_25x25km <-
  st_make_grid(euro_nuts3_sf, crs = 3035, what = 'polygons', cellsize = 25e3)
# 10,000 km2 hex grid
# hexagons parametrized by minimal diameter (d) in st_make_grid() function
# therefore if area A is given:
# d = sqrt(2)*sqrt(A)/3^(1/4)
euro_grid_hex_10000km2 <-
  st_make_grid(euro_nuts3_sf, crs = 3035, what = 'polygons',
               cellsize = sqrt(2)*sqrt(100e3^2)/3^(1/4), square = FALSE)
# 2500 km2 hex grid
euro_grid_hex_2500km2 <-
  st_make_grid(euro_nuts3_sf, crs = 3035, what = 'polygons',
               cellsize = sqrt(2)*sqrt(50e3^2)/3^(1/4), square = FALSE)

# SANITY CHECK: are the grid cell areas identical?
range(st_area(euro_grid_rect_100x100km))
range(st_area(euro_grid_rect_50x50km))
range(st_area(euro_grid_rect_25x25km))
range(st_area(euro_grid_hex_10000km2))
range(st_area(euro_grid_hex_2500km2))

# interpolate population counts by age over the grid
# and extract grid centroids
euro_grid_age_sf <-
  left_join(euro_nuts3_sf, euro_age, by = 'geo') %>%
  gather(key, value, total, age65plus, age0to15, age15to65) %>%
  group_by(key) %>%
  do(
    # area weighted grid interpolation preserving totals
    bind_rows(
      rect_100x100km = select(., value) %>%
        st_interpolate_aw(to = euro_grid_rect_100x100km, extensive = TRUE) %>%
        st_centroid(),
      rect_50x50km = select(., value) %>%
        st_interpolate_aw(to = euro_grid_rect_50x50km, extensive = TRUE) %>%
        st_centroid(),
      rect_25x25km = select(., value) %>%
        st_interpolate_aw(to = euro_grid_rect_25x25km, extensive = TRUE) %>%
        st_centroid(),
      hex_10000km2 = select(., value) %>%
        st_interpolate_aw(to = euro_grid_hex_10000km2, extensive = TRUE) %>%
        st_centroid(),
      hex_2500km2 = select(., value) %>%
        st_interpolate_aw(to = euro_grid_hex_2500km2, extensive = TRUE) %>%
        st_centroid(),
      .id = 'grid_type'
    )
  ) %>%
  # projection flag got lost, reset it
  st_as_sf(crs = 3035) %>%
  spread(key, value)

# SANITY CHECK: do the totals make sense?
# should be approx 0
euro_grid_age_sf %>%
  # calculate population shares by grid cell
  mutate(diff = age0to15+age15to65+age65plus - total) %>%
  {range(.$diff)}

# average European age structure in 2015
euro_center <- with(na.omit(euro_age),
                    c(p_age0to15 = sum(age0to15)/sum(total),
                      p_age15to65 = sum(age15to65)/sum(total),
                      p_age65plus = sum(age65plus)/sum(total)))

# add population shares by age and
# differences from European average
euro_grid_age_sf <-
  euro_grid_age_sf %>%
  mutate(p_age0to15 = age0to15/total,
         p_age15to65 = age15to65/total,
         p_age65plus = age65plus/total,
         d_age0to15 = p_age0to15-euro_center['p_age0to15'],
         d_age15to65 = p_age15to65-euro_center['p_age15to65'],
         d_age65plus = p_age65plus-euro_center['p_age65plus'])

# prepare clean data frame
euro_grid_age_df <-
  euro_grid_age_sf %>%
  # project back to spherical (lon-lat) coordinates
  st_transform(4326) %>%
  # add lon-lat coordinates of grid centroids
  cbind(st_coordinates(.)) %>%
  # convert to standard data frame
  as_data_frame() %>%
  select(-geometry) %>%
  rename(grid_id = Group.1, longitude = X, latitude = Y) %>%
  arrange(grid_type, desc(total))

# SANITY CHECK: do the differences make sense?
# should be approx 0
euro_grid_age_df %>%
  mutate(sum = d_age0to15 + d_age15to65 + d_age65plus) %>%
  {range(.$sum)}

write_csv(euro_grid_age_df, 'euro_grid_age.csv')

# Gridded population age structure ----------------------------------------

# color code the compositional deviation from european avg
euro_grid_age_df$col <-
  tricolore::Tricolore(euro_grid_age_df, legend = FALSE,
                       p1 = 'p_age65plus', p2 = 'p_age15to65', p3 = 'p_age0to15',
                       center = rev(prop.table(euro_center)), spread = 2.9,
                       contrast = .5, lightness = 1, chroma = 1, hue = 2/12)

# bubble-grid-map
euro_grid_age_df %>% filter(grid_type == 'hex_2500km2') %>%
  ggplot(aes(x = longitude, y = latitude)) +
  geom_polygon(aes(group = group), data = eura, color = 'white', fill = 'grey95') +
  geom_point(aes(col = col, size = total), show.legend = FALSE) +
  coord_map(xlim = c(-30, 50), ylim = c(35, 70), projection = 'lambert', 43, 62) +
  scale_size_area(
    'Population size',
    max_size = 3,
    breaks = c(1e3, 1e4, 1e5, 5e5),
    labels = c('1,000', '10,000', '100,000', '500,000')) +
  scale_color_identity() +
  theme_void() +
  labs(caption = 'Data: Eurostat')

