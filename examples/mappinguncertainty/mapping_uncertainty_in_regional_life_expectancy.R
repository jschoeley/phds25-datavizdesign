#'---
#' title: Mapping uncertainty in regional life-expectancy changes
#' author: Jonas Schöley
#' date: 2021-06-17
#'---

# Init ------------------------------------------------------------

library(tidyverse)
library(lubridate)
library(eurostat)
library(sf)
library(rnaturalearth)
# map simplification tools, requires dependencies external to R
# sudo apt install protobuf-compiler libprotobuf-dev libjq-dev libv8-dev
library(rmapshaper)

# Constants -------------------------------------------------------

dat <- list()
fig <- list()

cnst <- list()
cnst <- within(cnst, {
  map_limits = c(xmin = 25e5, xmax = 75e5, ymin = 13.5e5, ymax = 54.5e5)
  crs = 3035
  n_sim = 100
  width = unit(8, 'cm')
  height = width*0.8375
})

cnst$age_codes <- tribble(
  ~code, ~age_start, ~age_width,
  'TOTAL',  -1 , NA,
  'UNK',    -99, NA,
  'Y_LT5',  0  , 5,
  'Y5-9',   5  , 5,
  'Y10-14', 10 , 5,
  'Y15-19', 15 , 5,
  'Y20-24', 20 , 5,
  'Y25-29', 25 , 5,
  'Y30-34', 30 , 5,
  'Y35-39', 35 , 5,
  'Y40-44', 40 , 5,
  'Y45-49', 45 , 5,
  'Y50-54', 50 , 5,
  'Y55-59', 55 , 5,
  'Y60-64', 60 , 5,
  'Y65-69', 65 , 5,
  'Y70-74', 70 , 5,
  'Y75-79', 75 , 5,
  'Y80-84', 80 , 5,
  'Y85-89', 85 , 5,
  'Y_GE90', 90 , Inf  
)

# Data for background map -----------------------------------------

# download geospatial data for European, Asian and African countries
# for use as background map
dat$background_map <-
  ne_countries(
    continent = c('europe', 'asia', 'africa'),
    returnclass = 'sf', scale = 50
  ) %>%
  # re-project
  st_transform(crs = cnst$crs) %>%
  # pseudo-buffer regions to avoid self-intersection errors
  st_buffer(0) %>%
  # crop to Europe
  st_crop(
    xmin = cnst$map_limits[['xmin']], xmax = cnst$map_limits[['xmax']],
    ymin = cnst$map_limits[['ymin']], ymax = cnst$map_limits[['ymax']]
  )

saveRDS(dat$background_map, './dat/background_map.rds')

# European NUTS-3 geodata -----------------------------------------

# download geodata on nuts-3 regions
dat$euro_geo_nuts3 <-
  get_eurostat_geospatial(
    output_class = 'sf', resolution = '60', nuts_level = 3, year = 2016
  ) %>%
  st_transform(crs = cnst$crs) %>%
  st_buffer(0) %>%
  st_crop(
    xmin = cnst$map_limits[['xmin']], xmax = cnst$map_limits[['xmax']],
    ymin = cnst$map_limits[['ymin']], ymax = cnst$map_limits[['ymax']]
  ) %>%
  # simplify to save space
  ms_simplify(keep = 0.05, keep_shapes = TRUE) %>%
  select(id, name = NUTS_NAME, geometry)

saveRDS(dat$euro_geo_nuts3, './dat/euro_geo_nuts3.rds')

# Download regional deaths and population -------------------------

# death counts by nuts-3 region, year, sex, and age
dat$deaths <- get_eurostat('demo_r_magec3')

# subset to total death counts and NUTS-3 regions
# with numeric age groups
dat$deaths_harmonized <-
  dat$deaths %>%
  mutate(year = year(time)) %>%
  left_join(cnst$age_codes, by = c('age' = 'code')) %>%
  filter(sex == 'T', str_length(geo) == 5, age_start >= 0) %>%
  select(geo, year, age_start, age_width, deaths = values)

# population counts by nuts-3 region, year, sex, and age
dat$population <- get_eurostat('demo_r_pjangrp3')

# subset to total population counts and NUTS-3 regions
# with numeric age groups
dat$population_harmonized <-
  dat$population %>%
  mutate(year = year(time)) %>%
  left_join(cnst$age_codes, by = c('age' = 'code')) %>%
  filter(sex == 'T', str_length(geo) == 5, age_start >= 0) %>%
  select(geo, year, age_start, age_width, population = values)

# join death counts and population numbers
dat$popdeath <- full_join(dat$population_harmonized, dat$deaths_harmonized)

# Calculate regional life-expectancy and uncertainty --------------

# simple piecewise-exponential life-table
CalculateLifeTable <-
  function (df, x, nx, Dx, Ex) {
    
    require(dplyr)
    
    df %>%
      transmute(
        x = {{x}},
        nx = {{nx}},
        mx = {{Dx}}/{{Ex}},
        px = exp(-mx*{{nx}}),
        qx = 1-px,
        lx = head(cumprod(c(1, px)), -1),
        dx = c(-diff(lx), tail(lx, 1)),
        Lx = ifelse(mx==0, lx*nx, dx/mx),
        Tx = rev(cumsum(rev(Lx))),
        ex = Tx/lx
      )
    
  }

# for each nuts-3 region in the years 2014 and 2017 with complete
# data, sample a number of replicated age specific total death counts
# via the Poisson distribution
dat$popdeath_sim <-
  dat$popdeath %>%
  filter(year %in% c(2014, 2017)) %>%
  group_by(geo) %>%
  mutate(drop = any(is.na(deaths) | is.na(population))) %>%
  ungroup() %>%
  filter(drop == FALSE) %>%
  expand_grid(id_sim = 1:cnst$n_sim) %>%
  group_by(geo, year, age_start) %>%
  mutate(death_total_sim = rpois(cnst$n_sim, deaths)) %>%
  arrange(id_sim, geo, year, age_start)

# for each region, year, and replication calculate a life-expectancy
# we use data.table for speed
library(data.table)
CalculateLifeTableDT <- function (.SD) {
  CalculateLifeTable(.SD, age_start, age_width, death_total_sim, population) %>%
    filter(x == 0) %>% pull(ex)
}
dat$popdeath_sim <- as.data.table(dat$popdeath_sim)
dat$e0sim <- dat$popdeath_sim[
  , .(e0 = CalculateLifeTableDT(.SD)),
  by = .(id_sim, geo, year)
]

# Calculate e0 differences ----------------------------------------

dat$nuts3_e0diff1417_simulated <-
  dat$e0sim %>%
  select(id_sim, geo, year, e0) %>%
  pivot_wider(names_from = year, values_from = e0, names_prefix = 'e0') %>%
  mutate(
    e0_diff = e02017 - e02014
  )

saveRDS(
  dat$nuts3_e0diff1417_simulated,
  './dat/nuts3_e0diff1417_simulated.rds',
  compress = 'xz'
)

dat$nuts3_e0diff1417_significance <-
  dat$nuts3_e0diff1417_simulated %>%
  group_by(geo) %>%
  summarise(
    e0_diff_avg = mean(e0_diff),
    e0_diff_se = sd(e0_diff),
    e0_diff_z = e0_diff_avg/e0_diff_se,
    e0_diff_pr =
      ifelse(e0_diff_avg>0, sum(e0_diff>0)/cnst$n_sim, sum(e0_diff<=0)/cnst$n_sim)
  )

saveRDS(
  dat$nuts3_e0diff1417_significance,
  './dat/nuts3_e0diff1417_significance.rds',
  compress = 'xz'
)

# Join compositional data with geodata ------------------------------------

dat$background_map <- readRDS('dat/background_map.rds')
dat$euro_geo_nuts3 <- readRDS('dat/euro_geo_nuts3.rds')
dat$nuts3_e0diff1417_significance <- readRDS('dat/nuts3_e0diff1417_significance.rds')
dat$nuts3_e0diff1417_simulated <- readRDS('dat/nuts3_e0diff1417_simulated.rds')

dat$sf_nuts3_e0diff1417_significance <-
  dat$euro_geo_nuts3 %>%
  left_join(dat$nuts3_e0diff1417_significance, by = c(id = 'geo'))

dat$sf_nuts3_e0diff1417_simulated <-
  dat$euro_geo_nuts3 %>%
  left_join(dat$nuts3_e0diff1417_simulated, by = c(id = 'geo'))

# Raw change ------------------------------------------------------

# plot the years of life-expectancy change from 2014 to 2017 by nuts-3
# region using a discrete divergent color scale.

e0_diff_breaks <- c(-Inf, -0.5, 0, 0.5, Inf)

fig$change <-
  dat$sf_nuts3_e0diff1417_simulated %>%
  filter(id_sim %in% 1:9) %>%
  mutate(effect = cut(e0_diff, breaks = e0_diff_breaks)) %>%
  ggplot() +
  geom_sf(data = dat$background_map, fill = 'white', color = NA) +
  geom_sf(aes(fill = effect), color = NA) +
  geom_sf(data = dat$background_map, fill = NA, size = 0.3) +
  scale_fill_brewer(type = 'div', palette = 'BrBG') +
  facet_wrap(~id_sim, nrow = 3) +
  coord_sf(expand = FALSE, datum = NA) +
  guides(fill = guide_colorsteps()) +
  labs(fill = 'Years of life-\nexpectancy change') +
  theme(
    #legend.position = c(0.8, 0.8),
    legend.background = element_blank(),
    panel.background = element_rect(colour = 'black', fill = 'lightblue'),
    plot.background = element_blank(),
    plot.margin = unit(rep(0.1,4), 'cm')
  )
fig$change

ggsave(
  './out/alternative_outcome_facets.png', fig$change,
  width = cnst$width*1.4, height = cnst$height,
  dpi = 300
)

# Z scores --------------------------------------------------------

# plot the Z-scores of the life-expectancy difference between 2014 and
# 2017 by nuts-3 region using a discrete divergent color scale

z_breaks <- c(-Inf, -2, -1, 0, 1, 2, Inf)

fig$zscore <-
  dat$sf_nuts3_e0diff1417_significance %>%
  mutate(effect = cut(e0_diff_z, breaks = z_breaks)) %>%
  ggplot() +
  geom_sf(data = dat$background_map, fill = 'white', color = NA) +
  geom_sf(aes(fill = effect), color = NA) +
  geom_sf(data = dat$background_map, fill = NA, size = 0.3) +
  scale_fill_brewer(type = 'div', palette = 'BrBG') +
  coord_sf(expand = FALSE, datum = NA) +
  guides(fill = guide_colorsteps()) +
  labs(fill = 'Z-score of life-\nexpectancy change') +
  theme(
    legend.position = c(0.8, 0.8),
    legend.background = element_blank(),
    panel.background = element_rect(colour = 'black', fill = 'lightblue'),
    plot.background = element_blank(),
    plot.margin = unit(rep(0.1,4), 'cm')
  )
fig$zscore

ggsave(
  './out/zscore.png', fig$zscore,
  width = cnst$width, height = cnst$height,
  dpi = 300
)

# Significance cut-off --------------------------------------------

# plot life-expectancy changes for those regions where we are
# more than 90% certain about the direction of the change

e0_diff_breaks <- c(-Inf, -0.5, 0, 0.5, Inf)

fig$cutoff <-
  dat$sf_nuts3_e0diff1417_significance %>%
  filter(e0_diff_pr >= 0.90) %>%
  mutate(effect = cut(e0_diff_avg, breaks = e0_diff_breaks)) %>%
  ggplot() +
  geom_sf(data = dat$background_map, fill = 'white', color = NA) +
  geom_sf(aes(fill = effect), color = NA) +
  geom_sf(data = dat$background_map, fill = NA, size = 0.3) +
  scale_fill_brewer(type = 'div', palette = 'BrBG') +
  coord_sf(expand = FALSE, datum = NA) +
  guides(fill = guide_colorsteps()) +
  labs(fill = 'Years of life-\nexpectancy change') +
  theme(
    legend.position = c(0.8, 0.8),
    legend.background = element_blank(),
    panel.background = element_rect(colour = 'black', fill = 'lightblue'),
    plot.background = element_blank(),
    plot.margin = unit(rep(0.1,4), 'cm')
  )
fig$cutoff

ggsave(
  './out/cutoff.png',
  width = cnst$width, height = cnst$height,
  dpi = 300
)

# Value-by-alpha --------------------------------------------------

# use lighter and more desaturated colors for life-expectancy changes
# with uncertain direction

e0_diff_breaks <- c(-Inf, -0.5, 0, 0.5, Inf)

# create divergent color scale with 4 effect levels
# and 2 certainty levels
certain_colors <- RColorBrewer::brewer.pal(4, 'BrBG')
uncertain_colors <-
  prismatic::clr_alpha(certain_colors, alpha = 0.2)
uncertainty_palette_4x2 <- tibble(
  id = c(sapply(1:2, paste0, 1:4)),
  rgb = c(uncertain_colors, certain_colors)
)

# generate legend
fig$valuebyalpha_legend <- expand_grid(
  certainty = 1:2,
  effect = 1:4
) %>%
  mutate(rgb = uncertainty_palette_4x2$rgb) %>%
  ggplot() +
  geom_tile(aes(y = certainty, x = effect, fill = rgb)) +
  scale_fill_identity() +
  scale_y_continuous(breaks = c(1, 2), labels = c('<90%', '>90%')) +
  scale_x_continuous(
    breaks = c(1.5, 2.5, 3.5), labels = c('-0.5', '0', '+0.5'),
    position = 't'
  ) +
  coord_fixed(0.5) +
  labs(
    y = 'Certainty', x = 'Effect size',
    title = 'Years of life-\nexpectancy change'
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.background = element_blank(),
    plot.margin = unit(c(0,0,0,0),'mm')
  )

# Rescale [0,1] range vector to range [xmin, xmax]
ReScaleInv <- function (x, xmin, xmax) {
  x*(xmax-xmin) + xmin
}

# generate map
fig$valuebyalpha <-
  dat$sf_nuts3_e0diff1417_significance %>%
  # map the colors to the regions based on effect size and certainty
  mutate(
    # dim the colors if we are less than 90% certain about
    # the direction of the result
    dim = ifelse(e0_diff_pr < 0.9, 1, 2),
    # discretized effect size
    effect = cut(e0_diff_avg, breaks = e0_diff_breaks, labels = 1:4),
    # rgb color
    color = factor(
      paste0(dim, effect),
      levels = uncertainty_palette_4x2$id,
      labels = uncertainty_palette_4x2$rgb
    ) %>% as.character()
  ) %>%
  ggplot() +
  geom_sf(data = dat$background_map, fill = 'white', color = NA) +
  geom_sf(aes(fill = color), color = NA) +
  geom_sf(data = dat$background_map, fill = NA, size = 0.3) +
  # add legend to map
  annotation_custom(
    ggplotGrob(fig$valuebyalpha_legend),
    xmin =
      ReScaleInv(0.6, xmin = cnst$map_limits[['xmin']],
                 xmax = cnst$map_limits[['xmax']]),
    xmax =
      ReScaleInv(0.95, xmin = cnst$map_limits[['xmin']],
                 xmax = cnst$map_limits[['xmax']]),
    ymin =
      ReScaleInv(0.54, xmin = cnst$map_limits[['ymin']],
                 xmax = cnst$map_limits[['ymax']]),
    ymax =
      ReScaleInv(0.99, xmin = cnst$map_limits[['ymin']],
                 xmax = cnst$map_limits[['ymax']])
  ) +
  scale_fill_identity() +
  coord_sf(expand = FALSE, datum = NA) +
  theme(
    panel.background = element_rect(colour = 'black', fill = 'lightblue'),
    plot.background = element_blank(),
    plot.margin = unit(rep(0.1,4), 'cm')
  )
fig$valuebyalpha

ggsave(
  './out/valuebyalpha.png', fig$valuebyalpha,
  width = cnst$width, height = cnst$height,
  dpi = 300
)

# Value suppressing uncertainty -----------------------------------

e0_diff_breaks <- c(-Inf, -0.5, 0, 0.5, Inf)

# this is an alternative color scheme where the step-size of the
# color scale is modified in addition to the saturation and lightness
certain_colors <- RColorBrewer::brewer.pal(4, 'BrBG')
less_certain_colors <-
  prismatic::clr_alpha(certain_colors[c(1,1,4,4)], alpha = 0.2)
least_certain_colors <-
  rep('grey95', 4)
vsu_palette <- tibble(
  id = c(sapply(1:3, paste0, 1:4)),
  rgb = c(
    least_certain_colors,
    less_certain_colors,
    certain_colors
  )
)

# generate legend
fig$vsu_legend <- expand_grid(
  certainty = 1:3,
  effect = 1:4
) %>%
  mutate(rgb = vsu_palette$rgb) %>%
  ggplot() +
  geom_tile(aes(y = certainty, x = effect, fill = rgb)) +
  scale_fill_identity() +
  scale_y_continuous(breaks = c(1.5, 2.5), labels = c('75%', '90%')) +
  scale_x_continuous(breaks = c(1.5, 2.5, 3.5), labels = c('-0.5', '0', '+0.5'),
                     position = 't') +
  coord_fixed(0.5) +
  labs(
    y = 'Certainty', x = 'Effect size',
    title = 'Years of life-\nexpectancy change'
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    plot.background = element_blank(),
    plot.margin = unit(c(0,0,0,0),'mm')
  )

# Rescale [0,1] range vector to range [xmin, xmax]
ReScaleInv <- function (x, xmin, xmax) {
  x*(xmax-xmin) + xmin
}

# generate map
fig$vsu <-
  dat$sf_nuts3_e0diff1417_significance %>%
  # map the colors to the regions based on effect size and certainty
  mutate(
    # dim the colors if we are less than 90% certain on the direction of
    # the result
    dim = .bincode(e0_diff_pr, c(0, 0.75, 0.9, 1)),
    # discretized effect size
    effect = cut(e0_diff_avg, breaks = e0_diff_breaks, labels = 1:4),
    # rgb color
    color = factor(
      paste0(dim, effect),
      levels = vsu_palette$id,
      labels = vsu_palette$rgb
    ) %>% as.character()
  ) %>%
  ggplot() +
  geom_sf(data = dat$background_map, fill = 'white', color = NA) +
  geom_sf(aes(fill = color), color = NA) +
  geom_sf(data = dat$background_map, fill = NA, size = 0.3) +
  # add legend to map
  annotation_custom(
    ggplotGrob(fig$vsu_legend),
    xmin =
      ReScaleInv(0.6, xmin = cnst$map_limits[['xmin']],
                 xmax = cnst$map_limits[['xmax']]),
    xmax =
      ReScaleInv(0.95, xmin = cnst$map_limits[['xmin']],
                 xmax = cnst$map_limits[['xmax']]),
    ymin =
      ReScaleInv(0.52, xmin = cnst$map_limits[['ymin']],
                 xmax = cnst$map_limits[['ymax']]),
    ymax =
      ReScaleInv(0.97, xmin = cnst$map_limits[['ymin']],
                 xmax = cnst$map_limits[['ymax']])
  ) +
  scale_fill_identity() +
  coord_sf(expand = FALSE, datum = NA) +
  theme(
    panel.background = element_rect(colour = 'black', fill = 'lightblue'),
    plot.background = element_blank(),
    plot.margin = unit(rep(0.1,4), 'cm')
  )
fig$vsu

ggsave(
  './out/vsu.png', fig$vsu,
  width = cnst$width, height = cnst$height,
  dpi = 300
)

# Alternative outcome plots ---------------------------------------

library(gganimate)

e0_diff_breaks <- c(-Inf, -0.5, 0, 0.5, Inf)

fig$alternativeoutcomes <-
  dat$sf_nuts3_e0diff1417_simulated %>%
  mutate(effect = cut(e0_diff, breaks = e0_diff_breaks)) %>%
  ggplot() +
  geom_sf(data = dat$background_map, fill = 'white', color = NA) +
  geom_sf(aes(fill = effect), color = NA) +
  transition_manual(id_sim) +
  geom_sf(data = dat$background_map, fill = NA, size = 0.3) +
  scale_fill_brewer(type = 'div', palette = 1) +
  coord_sf(expand = FALSE, datum = NA) +
  guides(fill = guide_colorsteps()) +
  theme(
    legend.position = c(0.8, 0.8),
    legend.background = element_blank(),
    panel.background = element_rect(colour = 'black', fill = 'lightblue'),
    plot.background = element_blank(),
    plot.margin = unit(rep(0.1,4), 'cm')
  )

fig$alternativeoutcomes_anim <- animate(
  fig$alternativeoutcomes,
  nframes = 50, fps = 5, width = 1000, height = 1000*0.8375
)
anim_save('./out/alternativeoutcomes.gif')

save(
  euro_example,
  file = './data-raw/euro_example.RData',
  compress = 'xz',
  version = 2
)

