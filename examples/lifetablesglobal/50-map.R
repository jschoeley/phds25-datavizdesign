# Plots

# Init ------------------------------------------------------------

#remotes::install_github('coolbutuseless/ggpattern')
library(yaml)
library(tidyverse)
library(ggpattern)
library(rnaturalearth)
library(sf)

# Constants -------------------------------------------------------

# input and output paths
setwd('.')
paths <- list()
paths$input <- list(
  config = './cfg/config.yaml',
  global = './src/00-global.R',
  region = './cfg/who_regions.csv',
  forecasts = './tmp/40-forecast.rds'
)
paths$output <- list(
  e0deficit = './out/50-e0deficit.rds'
)

# global configuration
config <- read_yaml(paths$input$config)

# global objects and functions
source(paths$input$global)

# constants specific to this analysis
cnst <- within(list(), {
  region = filter(
    read_csv(paths$input$region),
    country_code_iso2 %in% config$skeleton$regions
  )
})

# Function --------------------------------------------------------

# return a vector of mean and quantiles
QuantileWithMean <- function (x, prob = cnst$quantiles) {
  x <- x[!(is.na(x)|is.nan(x)|is.infinite(x))]
  q <- quantile(x, prob = prob, names = FALSE, na.rm = TRUE)
  m <- mean(x, na.rm = TRUE)
  result <- c(m, q)
  names(result) <- c('mean', paste0('q', prob))
  return(result)
}

# Input -----------------------------------------------------------

forecast <- readRDS(paths$input$forecasts)

# e0deficit -------------------------------------------------------

e0deficit <- list()

# life expectancy deficit
e0deficit$raw$delta <-
  forecast['0','20/21','Total',,'ex','actual',] -
  forecast['0','20/21','Total',,'ex','expected',]

# CIs
e0deficit$raw$ci <- apply(
  e0deficit$raw$delta,
  c('region'),
  QuantileWithMean,
  c(0.05, 0.95),
  simplify = TRUE
)
names(dimnames(e0deficit$raw$ci))[1] <- 'quantile'

e0deficit$raw$df <-
  as.data.frame.table(e0deficit$raw$ci, stringsAsFactors = FALSE) %>%
  pivot_wider(names_from = quantile, values_from = Freq)

e0deficit$config <-
  list(
    breaks = c(-Inf, -4, -3, -2, -1, -0.5, 0, Inf),
    labels = c('>4 years', '3 to 4', '2 to 3', '1 to 2',
               '0.5 to 1', '0 to 0.5', 'no deficit'),
    color_e0deficit =
      c('>4 years'   = '#7a0177',
        '3 to 4'     = '#c51b8a',
        '2 to 3'     = '#f768a1',
        '1 to 2'     = '#fa9fb5',
        '0.5 to 1'   = '#fcc5c0',
        '0 to 0.5'   = '#feebe2',
        'no deficit' = '#f0f9e8',
        'unknown' = 'grey'),
    color_water = '#D0E7F2',
    crs = '+proj=robin +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs'
  )

e0deficit$data <-
  e0deficit$raw$df %>%
  mutate(
    mean_discrete = cut(
      mean,
      e0deficit$config$breaks,
      labels = e0deficit$config$labels,
      include.lowest = TRUE, right = TRUE
    ),
    significant = ifelse(q0.95 < 0 | q0.05 > 0, 'yes', 'no')
  )

# download global NUTS-1 geodata
geodat <-
  ne_countries(scale = 'medium', type = 'countries', returnclass = 'sf')

# reproject to robinson
geodat <-
  geodat %>%
  st_transform(
    geodat, 
    crs = e0deficit$config$crs
  )

# bounding box for the worldmap
bounding_box <-
  st_make_grid(
    st_bbox(c(xmin = -180,
              xmax = 180,
              ymax = 90,
              ymin = -90), crs = st_crs(4326)),
    n = 100) %>%
  st_union() %>%
  st_transform(e0deficit$config$crs)

# Join coverage info with map -------------------------------------

e0deficit$data <- left_join(
  geodat, e0deficit$data , by = c('iso_a2' = 'region')
) %>%
  mutate(
    mean_discrete =
      fct_explicit_na(mean_discrete, 'unknown'),
    mean_discrete = fct_drop(mean_discrete)
  )


# plot map
e0deficit$plot <-
  e0deficit$data %>% 
  ggplot() +
  geom_sf(
    data = bounding_box,
    linetype = 'solid',
    fill = e0deficit$config$color_water,
    color = NA
  ) +
  geom_sf(
    aes(
      fill = mean_discrete
    ),
    size = 0.1
  ) +
  geom_sf_pattern(
    aes(), fill = NA, pattern = 'stripe',
    pattern_fill = 'black', pattern_density = 0.1,
    pattern_spacing = 0.01, pattern_alpha = 0.2,
    size = 0, color = NA,
    . %>% filter(significant == 'no')
  ) +
  theme_void() +
  scale_fill_discrete(type = e0deficit$config$color_e0deficit[
    levels(e0deficit$data$mean_discrete)]) +
  labs(
    title = 'Life expectancy deficits 20/21',
    subtitle = 'Derived from WPP2022 via Poisson-Lee-Carter counterfactual. Hatching indicates non-significant deficit.',
    fill = 'LE deficit'
  ) +
  guides(fill = guide_legend(override.aes = list(color = NA)))
# scale_fill_gradient2(
#   low = 'darkred', mid = 'white', high = 'lightblue',
#   midpoint = 0,
#   limits = c(-4, 0.2), oob = scales::squish
# )# +
# scale_fill_gradient2(
#   low = 'lightblue', mid = 'white', high = 'darkred',
#   midpoint = 0,
#   limits = c(0.5, 2), oob = scales::squish, trans = 'log10'
# )# +
#facet_grid(year~age)
e0deficit$plot

# export results of analysis
ggsave(
  'e0deficit.svg',
  e0deficit$plot,
  device = 'svg',
  path = 'out',
  width = 16
)

saveRDS(e0deficit, 'out/50-e0deficit.rds')
