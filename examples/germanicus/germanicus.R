# Animated Geo-Waffleplots of German intensive care utilization
# Jonas Schöley cc-by

# Init ------------------------------------------------------------

library(tidyverse)
library(gganimate)

# Constants -------------------------------------------------------

# input and output paths
paths <- list()
paths$input <- list(); paths$input <- within(paths$input, {
  tmpdir = './germanicus/tmp'
  zeitreihe_bundeslaender_path = './germanicus/zeitreihe-bundeslaender.csv'
  stategrid = './germanicus/stategrid.bmp'
  statekey = './germanicus/state_key.csv'
})
paths$output <- list(
  intensiv = './germanicus/'
)

# constants specific to this analysis
cnst <- list(); cnst <- within(cnst, {
  start = '2021-10-01'
  statekey = read_csv(paths$input$statekey)
  language = 'en'
})

# list containers for analysis artifacts
dat <- list()
fig <- list()

# Load data -------------------------------------------------------

# https://www.intensivregister.de/#/aktuelle-lage/downloads
dat$states <- read_csv(paths$input$zeitreihe_bundeslaender_path)

# Setup state grid ------------------------------------------------

# state grid layout is encoded in 24-bit bmp with state codes encoded
# in red channel values 1 to 16 in alphabetic order
# each state has as many pixels as it's maximum reported number of ICU
# beds between 2021-07-01 and 2021-12-04
library(bmp)
dat$stategrid <- bmp::read.bmp(paths$input$stategrid)[,,1]
dat$stategrid_long <-
  expand.grid(
    y = -(1:nrow(dat$stategrid)),
    x = 1:ncol(dat$stategrid)
  ) %>%
  mutate(
    state_id = c(dat$stategrid)
  ) %>%
  arrange(state_id, -y, x) %>%
  group_by(state_id) %>%
  mutate(
    bed_id = n():1
  ) %>%
  ungroup() %>%
  filter(state_id != 0)
# germany's simplified outline is encoded in the green channel
dat$germany_outline <- bmp::read.bmp(paths$input$stategrid)[,,2]
dat$germany_outline_long <-
  expand.grid(
    y = -(1:nrow(dat$stategrid)),
    x = 1:ncol(dat$stategrid)
  ) %>%
  mutate(
    order = c(dat$germany_outline)
  ) %>%
  filter(order != 0) %>%
  arrange(order)

# Prepare data for figure -----------------------------------------

fig$intensiv <- list()

fig$intensiv$cnst <- list(
  color_free = 'grey75',
  color_noncovid = '#00A18D',
  color_covid = '#F6B715',
  color_text = 'grey90',
  color_source = 'grey50',
  scaler = 1.8,
  family_title = 'Roboto Slab',
  family_labels = 'Roboto Condensed',
  fill_germany = 'grey15',
  fill_background = 'grey10',
  color_germany = NA,
  color_region_labels = 'grey90',
  size_germany = 3,
  color_waffle_grid = 'grey10',
  size_tiles = 0.2
)

fig$intensiv$data <-
  dat$states %>%
  left_join(cnst$statekey, by = c('Bundesland' = 'state_divi')) %>%
  select(
    date = Datum,
    state_id,
    occupied_total = Belegte_Intensivbetten,
    occupied_covid = Aktuelle_COVID_Faelle_ITS,
    free = Freie_Intensivbetten
  ) %>%
  mutate(
    date = lubridate::as_date(date),
    capacity = occupied_total + free,
    share_occupied = occupied_total/capacity,
    occupied_other = occupied_total - occupied_covid
  ) %>%
  filter(date >= cnst$start) %>%
  # expand to very long format for geowaffles
  group_by(date, state_id) %>%
  group_modify(~{
    tibble(
      occupied = c(rep(2, .x$occupied_covid), rep(1, .x$occupied_other), rep(0, .x$free)),
      share_occupied = .x$share_occupied
    ) %>%
      mutate(bed_id = 1:n())
  }) %>%
  ungroup() %>%
  mutate(
    color = case_when(occupied == 0 ~ fig$intensiv$cnst$color_free,
                      occupied == 1 ~ fig$intensiv$cnst$color_noncovid,
                      occupied == 2 ~ fig$intensiv$cnst$color_covid)
  ) %>%
  left_join(dat$stategrid_long, by = c('state_id', 'bed_id')) %>%
  left_join(cnst$statekey, by = 'state_id')

fig$intensiv$label_positions <-
  fig$intensiv$data %>%
  group_by(state_long, state_short) %>%
  summarise(x = min(x, na.rm = T), y = max(y, na.rm = T)) %>%
  mutate(state_long = ifelse(state_short == 'ST', 'Sachsen-\nAnhalt', state_long)) %>%
  ungroup()

# Test if pixels in grid are enough -------------------------------

# This must return TRUE, otherwise,
# edit grid to accomodate additional
# ICU beds
left_join(
  dat$stategrid_long %>%
    group_by(state_id) %>%
    summarise(max1 = max(bed_id)),
  fig$intensiv$data %>%
    group_by(date, state_id) %>%
    summarise(n = n()) %>%
    group_by(state_id) %>%
    summarise(max2 = max(n))
) %>%
  mutate(OK = max2 <= max1) %>%
  pull(OK) %>% all()

# Render animated geowaffle plot ----------------------------------

fig$intensiv$labels <- list(
  explain1 = c(
    de = '1 Quadrat = 1 ITS Platz',
    en = '1 square = 1 ICU bed'
  ),
  explain2 = c(
    de = 'ITS Plätze sind begrenzt.',
    en = 'ICU beds are limited.'
  ),
  explain3 = c(
    de = 'Zur Versorgung von\nNotfällen müssen Betten\nfreigehalten werden.',
    en = 'Some beds must be\nkept free to take care\nof emergencies.'
  ),
  explain4 = c(
    de = 'Es stehen weniger Plätze\nfür nicht COVID Fälle\nzur Verfügung...',
    en = 'ICU beds for non-COVID\npatients get more limited...'
  ),
  explain5 = c(
    de = '...je mehr COVID Fälle\nbehandelt werden.',
    en = '...as more COVID patients\nenter intensive care.'
  ),
  source = c(
    de = paste('cc-by Jonas Schöley, Ph.D. | @jschoeley | Daten: DIVI-Intensivregister zeitreihe-bundeslaender.csv', Sys.Date()),
    en = paste('cc-by Jonas Schöley, Ph.D. | @jschoeley | Data: DIVI-Intensivregister zeitreihe-bundeslaender.csv', Sys.Date())
  ),
  title = c(
    de = 'Belegung der Intensivbetten am {format(lubridate::as_date(frame_time), format = "%e. %b %y")}',
    en = 'Occupation of German ICU beds as of {format(lubridate::as_date(frame_time), format = "%b %e %Y")}'
  )
)
# select language
fig$intensiv$labels <- lapply(fig$intensiv$labels, function (x) x[cnst$language])

fig$intensiv$plot <-
  fig$intensiv$data %>%
  #filter(date >= '2021-12-10') %>%
  filter(date >= cnst$start) %>%
  ggplot() +
  geom_polygon(
    aes(x = x, y = y),
    fill = fig$intensiv$cnst$fill_germany,
    color = fig$intensiv$cnst$color_germany,
    size = fig$intensiv$cnst$size_germany,
    data = dat$germany_outline_long
  ) +
  geom_tile(
    aes(x = x, y = y, fill = color,
        group = interaction(state_id, bed_id)),
    color = fig$intensiv$cnst$color_waffle_grid,
    size = fig$intensiv$cnst$size_tiles
  ) +
  geom_text(
    aes(x = x, y = y, label = state_long),
    hjust = 0, vjust = 0,
    family = fig$intensiv$cnst$family_labels,
    size = 2.7*fig$intensiv$cnst$scaler,
    lineheight = 0.8,
    color = fig$intensiv$cnst$color_region_labels,
    data = fig$intensiv$label_positions, nudge_y = 1
  ) +
  annotate(
    'text', x = 2, y = -12, vjust = 0, hjust = 0,
    size = 3*fig$intensiv$cnst$scaler,
    family = fig$intensiv$cnst$family_labels,
    label = fig$intensiv$labels$explain1,
    color = fig$intensiv$cnst$color_region_labels
  ) +
  annotate(
    'text', x = 172, y = -170, vjust = 0, hjust = 0,
    size = 3*fig$intensiv$cnst$scaler,
    family = fig$intensiv$cnst$family_labels,
    color = fig$intensiv$cnst$color_region_labels,
    lineheight = 0.8,
    label = fig$intensiv$labels$explain2
  ) +
  annotate(
    'text', x = 172, y = -170-20, vjust = 0, hjust = 0,
    size = 3*fig$intensiv$cnst$scaler,
    family = fig$intensiv$cnst$family_labels,
    color = fig$intensiv$cnst$color_free, lineheight = 0.8,
    label = fig$intensiv$labels$explain3
  ) +
  annotate(
    'text', x = 172, y = -190-20, vjust = 0, hjust = 0,
    size = 3*fig$intensiv$cnst$scaler,
    family = fig$intensiv$cnst$family_labels,
    color = fig$intensiv$cnst$color_noncovid, lineheight = 0.8,
    label = fig$intensiv$labels$explain4
  ) +
  annotate(
    'text', x = 172, y = -205-20, vjust = 0, hjust = 0,
    size = 3*fig$intensiv$cnst$scaler,
    family = fig$intensiv$cnst$family_labels,
    color = fig$intensiv$cnst$color_covid, lineheight = 0.8,
    label = fig$intensiv$labels$explain5
  ) +
  annotate(
    'text', x = 10, y = -249, vjust = 0, hjust = 0,
    size = 2.5*fig$intensiv$cnst$scaler,
    family = fig$intensiv$cnst$family_labels,
    color = fig$intensiv$cnst$color_source, lineheight = 0.8,
    label = fig$intensiv$labels$source, alpha = 0.1
  ) +
  labs(title = fig$intensiv$labels$title) +
  coord_equal(expand = FALSE, xlim = c(1,dim(dat$stategrid)[2]),
              ylim = c(-dim(dat$stategrid)[1], -1)) +
  scale_fill_identity() +
  theme_void() +
  theme(
    plot.title =
      element_text(family = fig$intensiv$cnst$family_title,
                   size = 10*fig$intensiv$cnst$scaler,
                   face = 'bold',
                   vjust = -2, hjust = 0),
    plot.background =
      element_rect(fill = fig$intensiv$cnst$fill_background, color = NA),
    panel.background = 
      element_rect(fill = fig$intensiv$cnst$fill_background, color = NA),
    text = element_text(color = fig$intensiv$cnst$color_text)
  ) +
  transition_time(date) +
  enter_appear() +
  exit_disappear()
fig$intensiv$plot

animate(
  fig$intensiv$plot,
  height = nrow(dat$stategrid)*1.2,
  width = ncol(dat$stategrid)*1.2,
  units = 'mm', res = 100,
  nframes = fig$intensiv$data$date %>% unique() %>% length() + 10, duration = 20,
  rewind = FALSE, end_pause = 10,
  bg = fig$intensiv$cnst$fill_background
)

anim_save('gicu.gif', path = paths$output$germanicus)
