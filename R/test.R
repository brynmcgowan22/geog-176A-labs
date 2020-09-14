library(tidyverse)
library(sf)
library(raster)
library(fasterize)
library(whitebox)
library(osmdata)
library(elevatr)


### Collecting Data
basin = read_sf("https://labs.waterdata.usgs.gov/api/nldi/linked-data/nwissite/USGS-11119750/basin")

elev  = elevatr::get_elev_raster(basin, z = 13, units = "feet") %>%
  crop(basin) %>%
  mask(basin)

writeRaster(elev, "data/basinelev.tif", overwrite = TRUE)

elev_raster = raster("data/basinelev.tif")

bb = st_bbox(basin) %>%
  st_as_sfc() %>%
  st_transform(4326)

osm = osmdata::opq(bb) %>%
  add_osm_feature(key = 'building') %>%
  osmdata_sf()

building = osm$osm_polygons %>%
  st_intersection(basin) %>%
  st_transform(crs(basin)) %>%
  st_centroid()

rail = building %>%
  dplyr::filter(amenity == "railway")

osm2 = osmdata::opq(bb) %>%
  add_osm_feature(key = 'waterway', value = "stream") %>%
  osmdata_sf()

stream = osm2$osm_lines

stream = stream %>%
  st_transform(crs(basin)) %>%
  st_intersection(basin)

### Question 2: Terrain Analysis

wbt_hillshade("data/basinelev.tif", "data/basinhillshade.tif")

hill_raster = raster("data/basinelev.tif")

plot(hill_raster, axes = FALSE, box = FALSE, col = gray.colors(256, alpha = 0.5), main = "Hillshade", legend = FALSE)
plot(stream, add = TRUE, col = "blue")

stream_buffer = stream %>%
  st_transform(5070) %>%
  st_buffer(10) %>%
  st_transform(crs(elev_raster))

stream_raster = fasterize::fasterize(stream_buffer, elev_raster)

writeRaster(stream_raster, "data/streamelev.tif", overwrite = TRUE)

wbt_breach_depressions("data/basinelev.tif", "data/beachdepressions.tif")

wbt_elevation_above_stream("data/beachdepressions.tif", "data/streamelev.tif", "data/handraster.tif")

hand_raster = raster("data/handraster.tif")
hand_raster = hand_raster + 3.69
stream_raster = raster("data/streamelev.tif")

hand_raster[stream_raster == 1] = 0

writeRaster(hand_raster, "data/handoffset.tif", overwrite = TRUE)

### Question 3: 2017 Impact Assessment

hand_offset = raster("data/handoffset.tif")

hand_offset[hand_offset > 10.02] = NA

plot(hill_raster, col = gray.colors(256, alpha = .5), main = "Hillshade Basin and Flood", legend = FALSE, box =FALSE)
plot(hand_offset, add = TRUE, col = rev(blues9), legend = FALSE)
plot(basin, add = TRUE)
plot(railway, col = "green", cex = 1, pch = 16, add = TRUE)

# These maps look accurate










