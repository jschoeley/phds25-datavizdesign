# ewsexratio: Plot Lexis Surface of Mortality Sex Ratios
# Jonas Schöley
#
# Using mortality rates for England and Wales across period, age,
# and sex provided by HMD (mortality.org), we calculate the
# male-female-mortality-ratios and plot it as a Lexis surface, a
# heat-map over time and age.
# 
# A plot like this has been published in
# Schöley & Willekens (2017). Visualizing compositional data on the
# Lexis surface. Demographic Research. 10.4054/DemRes.2017.36.21.
# https://www.demographic-research.org/articles/volume/36/21

# Init ------------------------------------------------------------

library(ggplot2)

# Load data -------------------------------------------------------

ewsexratiodata <- read.csv('ewsexratio.csv')

# Setup discrete color scale for continuous data ------------------

# mortality rate sex ratio breaks for discrete colour scale
breaks <- c(0, 1/2 , 100/175, 100/150, 100/125, 100/101,
            101/100, 125/100, 150/100, 175/100, 2/1, Inf)
labels <- c('>100% excess\nfemale mortality',
            '75 to 100%',
            '50 to 75%',
            '25 to 50%',
            '1 to 25%',
            'Equal mortality',
            # these spaces at the end are significant
            # because I want the resulting factor levels to be unique
            '1 to 25% ',
            '25 to 50% ',
            '50 to 75% ' ,
            '75 to 100% ',
            '>100% excess\nmale mortality')

# discretize sex ratio
ewsexratiodata$mortality_sex_ratio_disc <- cut(
  ewsexratiodata$mortality_sex_ratio, breaks, labels,
  include.lowest = TRUE
)

# Plot Lexis surface ----------------------------------------------

# plot mortality sex ratio Lexis surface
ewsexratioplot <-
  ggplot(ewsexratiodata) +
  # heatmap
  geom_raster(
    aes(
      x = year, y = age,
      fill = mortality_sex_ratio_disc
    ),
    position = position_nudge(x = 0.5, y = 0.5)
  ) +
  # Lexis grid
  geom_hline(yintercept = seq(10, 100, 10),
             alpha = 0.2, lty = 'dotted') +
  geom_vline(xintercept = seq(1910, 2020, 10),
             alpha = 0.2, lty = 'dotted') +
  geom_abline(intercept = seq(-100, 110, 10)-1910,
              alpha = 0.2, lty = 'dotted') +
  # scales
  scale_fill_brewer(name = NULL, type = 'div', palette = 5, drop = FALSE) +
  scale_x_continuous('Year', expand = c(0.02, 0),
                     breaks = seq(1900, 2020, 10)) +
  scale_y_continuous('Age', expand = c(0, 0),
                     breaks = seq(0, 100, 10)) +
  guides(fill = guide_legend(reverse = TRUE)) +
  # coord
  coord_equal() +
  # theme
  theme_void() +
  theme(
    axis.text = element_text(colour = 'black'),
    axis.text.y = element_text(),
    axis.text.x = element_text()
  )
ewsexratioplot
ggsave(
  'ewsexratio.svg', ewsexratioplot,
  device = svglite::svglite,
  width = 7, height = 7
)
