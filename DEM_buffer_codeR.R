#Code to build nodata values round a raster for use in DECIPHeR
# Author: Nicola Ellis

setwd('D:/Exeter_2017-2020/Modelling/Land_cover_rasters')
library(raster)
library(rgdal)
library(RColorBrewer)

DEM <- raster("lc_100_reclass.tif")
NAvalue(DEM) <- -9999
NAvalue(DEM) #double check this
BUFFER <- raster("param_grid_LC.asc") #original raster for reference

#Plot to check right
plot(DEM, col = topo.colors(100))
extent(DEM) #CHECK THESE
extent(BUFFER)

#change extents
xmin_new <- extent(DEM)[1] -6
xmax_new <- extent(DEM)[2] +4
ymin_new <- extent(DEM)[3] -4
ymax_new <- extent(DEM)[4] +4

new_extent <- extent(xmin_new, xmax_new, ymin_new, ymax_new)
new_raster <- extend(DEM, new_extent, value = -9999)

extent(new_raster) #CHECK THESE MATCH BEFORE PROCEEDING
extent(BUFFER)

#Final checks
plot(new_raster, col = topo.colors(100))
summary(new_raster)
NAvalue(new_raster) <- -9999

writeRaster(new_raster, "lc_100", format = "ascii")

readr::read_lines('param_grid_LC.asc') %>%
  tolower() %>%
  readr::write_lines('param_grid_LC.asc')
