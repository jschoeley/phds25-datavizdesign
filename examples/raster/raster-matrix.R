# Geospatial raster data

# import global population raster:
# a matrix of numbers, each corresponding 
# to a rectangle on a grid defined by latitude
# (north-south) and longitude (east-west)
pop_array <- readRDS('./raster/pop_array.rds')
dim(pop_array)
summary(c(pop_array))

# Quick plot of a matrix ------------------------------------------

# we can have a quick look at matrices with the "image" function
# the log1p - log(x+1) - transform allows us to see the population
# densities across varying orders of magnitude
image(log1p(pop_array))

# watch out. the image() function does not plot the matrix
# in the same orientation as it is printed to the console.
# in order to have the first row and column plot at the upper left
# corner and the last row and column in the lower right corner
# (just like the matrix prints to console),
# you need to re-order the matrix. We write a wrapper function for
# that:
PlotMatrix <- function (X) {
  image(t(X[nrow(X):1,]))
}

PlotMatrix(log1p(pop_array))

# Crop to region of interest --------------------------------------

# Crop the data such that it mostly shows Australia.
# use https://www.latlong.net/ to figure out the bounding box

# crop to Australia
bounding_box <- list(
  minlat = -45, maxlat = -5, # south-north
  minlon = 110, maxlon = 155 # west-east
)

# select those elements of a matrix which are within the bounding box
numeric_lat <- as.numeric(dimnames(pop_array)$lat)
range(numeric_lat)
numeric_lon <- as.numeric(dimnames(pop_array)$lon)
range(numeric_lon)
lon_index <-
  numeric_lon > bounding_box$minlon &
  numeric_lon < bounding_box$maxlon
lat_index <-
  numeric_lat > bounding_box$minlat &
  numeric_lat < bounding_box$maxlat

dim(pop_array)
dimnames(pop_array)
PlotMatrix(log1p(t(pop_array[lon_index, lat_index])))

# Only show areas with a population density of more than
# a million people per grid cell.
binary_pop_array <- pop_array >= 1e6
PlotMatrix(t(binary_pop_array))
