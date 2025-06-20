# Identify pairs of DHS regions which share an international border
# Jonas Schöley
# 2019-09-19

# Init ------------------------------------------------------------

library(tidyverse)
library(sf)
library(rnaturalearth)

# Import shapefiles -----------------------------------------------

# DHS regions merged into a single simple features data frame
dhs_regions <-
  paste0(
    './dhsborders/data/', list.files('./dhsborders/data/', pattern = '.+shp$')
  ) %>%
  map(
    ~ st_read(.) %>%
      select(., ISO, CNTRYNAMEE, REG_ID, REGNAME) %>%
      st_cast(to = 'MULTIPOLYGON') %>%
      mutate_at(vars(-geometry), ~as.character(.))
  ) %>%
  data.table::rbindlist(
    use.names = TRUE, fill = TRUE, idcol = FALSE
  ) %>%
  st_as_sf() %>%
  st_transform(
    '+proj=ortho +lat_0=10 +lon_0=19 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs'
  ) %>%
  filter(
    # some regions are marked as NA or excluded,
    # we shall exclude them as well
    !(is.na(REG_ID) | REG_ID == 'Excluded')
  ) %>%
  # unique region id
  mutate(
    region_id = 1:n()
  )

# geodata for african countries
africa <-
  ne_countries(
    continent = c('africa'), returnclass = 'sf', scale = 50
  ) %>%
  st_transform(
    '+proj=ortho +lat_0=10 +lon_0=19 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs'
  )

# Identify border region pairs ------------------------------------

# all unique pairs of DHS regions that share
# an international border
dhs_neighbours <-
  dhs_regions %>%
  # which regions touch each other?
  st_intersects(sparse = FALSE) %>%
  # no self intersections and no double counting of pairs
  `&`(lower.tri(.)) %>%
  apply(1, which) %>%
  {tibble(
    regionindex_1 = rep(1:length(.), times = sapply(., length)),
    regionindex_2 = unlist(.),
    cntryid_1 = dhs_regions[regionindex_1,][['ISO']],
    cntryid_2 = dhs_regions[regionindex_2,][['ISO']],
    dhsregionid_1 = dhs_regions[regionindex_1,][['REG_ID']],
    dhsregionid_2 = dhs_regions[regionindex_2,][['REG_ID']],
    regionname_1 = dhs_regions[regionindex_1,][['REGNAME']],
    regionname_2 = dhs_regions[regionindex_2,][['REGNAME']]
  )} %>%
  # count only pairs that are in different countries
  filter(
    cntryid_1 != cntryid_2
  ) %>%
  mutate(
    pairid = 1:n()
  )

dhs_neighbours %>%
  write_excel_csv(
    './dhsborders/dhs_region_pairs_with_international_border.csv'
  )

# Plot border regions -----------------------------------------------------

# simplify dhs geodata for plotting
dhs_regions_simple <-
  dhs_regions %>%
  st_simplify(dTolerance = 1000)

# all DHS regions that share an international border
ggplot(dhs_regions_simple) +
  geom_sf(
    data = dhs_regions_simple,
    color = 'black',
    fill = 'grey90',
    lwd = 0.1
  ) +
  geom_sf(
    data = dhs_regions_simple[dhs_neighbours[['regionindex_1']],],
    color = 'black',
    fill = 'red',
    lwd = 0.1
  ) +
  geom_sf(
    data = dhs_regions_simple[dhs_neighbours[['regionindex_2']],],
    color = 'black',
    fill = 'red',
    lwd = 0.1
  ) +
  geom_sf(
    fill = NA, color = 'black', lwd = 0.3,
    data = africa
  ) +
  coord_sf(datum = NA) +
  theme_void() +
  labs(title = 'DHS regions sharing an international border with other DHS regions')

pdf('dhs_region_pairs_with_international_border.pdf')
for (i in 1:nrow(dhs_neighbours)) {
  cat(i, sep = '\n')
  
  regname1 <- dhs_neighbours[i,][['regionname_1']]
  regname2 <- dhs_neighbours[i,][['regionname_2']]
  country1 <- dhs_neighbours[i,][['cntryid_1']]
  country2 <- dhs_neighbours[i,][['cntryid_2']]
  label1 <- paste0(regname1, ' (', country1, ')')
  label2 <- paste0(regname2, ' (', country2, ')')
  
  pl <-
    ggplot(dhs_regions_simple) +
    geom_sf(
      data = dhs_regions_simple,
      color = 'black',
      fill = 'grey90',
      lwd = 0.1
    ) +
    geom_sf(
      data = dhs_regions_simple[dhs_neighbours[i,][['regionindex_1']],],
      color = 'black',
      fill = 'red',
      lwd = 0.1
    ) +
    geom_sf(
      data = dhs_regions_simple[dhs_neighbours[i,][['regionindex_2']],],
      color = 'black',
      fill = 'blue',
      lwd = 0.1
    ) +
    geom_sf(
      fill = NA, color = 'black', lwd = 0.3,
      data = africa
    ) +
    annotate(
      'text',
      x = -1.6e6, y = -1e6,
      label = label1, color = 'red',
      hjust = 1
    ) +
    annotate(
      'text',
      x = -1.6e6, y = -1.3e6,
      label = label2, color = 'blue',
      hjust = 1
    ) +
    coord_sf(datum = NA) +
    theme_void() +
    labs(caption = dhs_neighbours[i,][['pairid']])
  
  print(pl)
  
}
dev.off()
