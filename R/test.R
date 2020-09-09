library(raster)
library(tidyverse)
library(getlandsat)
library(sf)
library(mapview)
library(osmdata)


uscities = read_csv("data/uscities.csv")

bb = uscities %>%
  filter(city == "Palo") %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  st_transform(5070) %>%
  st_buffer(5000) %>%
  st_bbox() %>%
  st_as_sfc() %>%
  st_as_sf()

mapview(bb)

scenes = lsat_scenes()

bbwgs = bb %>% st_transform(4326)
bb = st_bbox(bbwgs)

down = scenes %>%
  filter(min_lat <= bb$ymin, max_lat >= bb$ymax,
         min_lon <= bb$xmin, max_lon >= bb$xmax,
         as.Date(acquisitionDate) == as.Date("2016-09-26"))

write.csv(down, file = "data/palo-flood-scene.csv", row.names = FALSE)

meta = read_csv("data/palo-flood-scene.csv")

files = lsat_scene_files(meta$download_url) %>%
  filter(grepl(paste0("B", 1:6, ".TIF$", collapse = "|"), file)) %>%
  arrange(file) %>%
  pull(file)

st = sapply(files, lsat_image)

s = stack(st) %>%
  setNames(c(paste0("band", 1:6)))

cropper = bbwgs %>%
  st_transform(crs(s))

r = crop(s, cropper)

par(mfrow = c(2, 2))
plotRGB(r, r = 4, g = 3, b = 2, stretch = "lin")
plotRGB(r, r = 5, g = 4, b = 3, stretch = "lin")
plotRGB(r, r = 5, g = 6, b = 4, stretch = "hist")
plotRGB(r, r = 6, g = 3, b = 2, stretch = "lin")

ndvi = (r$band5 - r$band4) / (r$band5 + r$band4)
plot(ndvi)

palette = colorRampPalette(c("blue", "white", "red"))
plot(ndvi, col = palette(256))

thresholding = function(x){ifelse(x <= 0, 1, NA)}

thresholding(100)
thresholding(-100)

flood = calc(ndvi, thresholding)
plot(flood, col = "blue")

mapview(flood)
