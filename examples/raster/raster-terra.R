# Geospatial raster data using terra

library(terra) # raster operations

# import global population raster:
# a matrix of numbers, each corresponding 
# to a rectangle on a grid defined by latitude
# (north-south) and longitude (east-west)
pop_array <- readRDS('./raster/pop_array.rds')
dim(pop_array)

# Convert matrix to raster object ---------------------------------

# in order to take a matrix and convert it to raster data,
# you need to translate the spatial representation implicit in your
# matrix:
#   - rows corresponding to single longitude, eastwards from London
#   - columns to latitude, north to south order,
# to the representation expected by the raster object:
#   - rows corresponding to latitude north to south
#   - columns to longitude eastwards from -180 to 180
# Work is needed to re-arrange the matrix accordingly

# separate matrix into eastern and western hemisphere and glue it back
# together with London in the middle.
dim(pop_array)
pop_array_eastern <- pop_array[dimnames(pop_array)$lon >= 0,]
image(log1p(pop_array_eastern))
pop_array_western <- pop_array[dimnames(pop_array)$lon < 0,]
image(log1p(pop_array_western))
pop_array_recentered <- rbind(pop_array_western, pop_array_eastern)
image(log1p(pop_array_recentered))

pop_rast <- rast(
  nrows = ncol(pop_array_recentered), # north-south
  ncol = nrow(pop_array_recentered), # east-west
  # lat-lon bounding box
  xmin = -180,
  xmax = 180,
  ymin = -90,
  ymax = 90,
  # we need to tell R that the grid is defined via lat-lon coordinates
  # this coordinate reference system has a standardized name:
  # EPSG:4326 a.k.a. WGS 84, see https://epsg.io/4326
  crs = 'EPSG:4326'
)

# the raster is stored as an ordered vector with first all latitudes -90,
# then all latitudes x=-89 etc.
values(pop_rast) <- as.vector(pop_array_recentered)

plot(log1p(pop_rast))

# Crop to region of interest --------------------------------------

# Crop the data such that it mostly shows Australia.
# use https://www.latlong.net/ to figure out the bounding box

# crop to Australia
bounding_box <- ext(
  # lon
  110, 155,
  # lat
  -45, -5
)

pop_rast_crop <- crop(pop_rast, bounding_box)
plot(log1p(pop_rast_crop))

# Reproject the raster --------------------------------------------

# united nations logo projection
pop_rast_project <- project(pop_rast, 'ESRI:102016')
pop_rast_project
plot(log1p(pop_rast_project))

# project to mercator
pop_rast_project <- project(
  crop(pop_rast, c(-180, 180, -70, 85)),
  'EPSG:3857'
)
plot(log1p(pop_rast_project), legend = FALSE, axes=FALSE)

# project to plate carré
pop_rast_project <- project(
  crop(pop_rast, c(-180, 180, -70, 85)),
  'EPSG:32662'
)
plot(log1p(pop_rast_project), legend = FALSE, axes=FALSE)
