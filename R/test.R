library(tidyverse)
library(sf)
library(units)
library(USAboundaries)
library(gghighlight)
library(ggrepel)
library(rmapshaper)
library(readxl)


conus = USAboundaries::us_counties(resolution = "low") %>%
  filter(!state_name %in% c("Alaska", "Hawaii", "Puerto Rico")) %>%
  st_transform(5070) %>%
  st_centroid() %>%
  st_combine()

sf_conus = USAboundaries::us_counties(resolution = "low") %>%
  filter(!state_name %in% c("Alaska", "Hawaii", "Puerto Rico")) %>%
  st_transform(5070) %>%
  st_centroid() %>%
  mutate(id = 1:n())

usa = USAboundaries::us_counties(resolution = "low") %>%
  filter(!state_name %in% c("Alaska", "Hawaii", "Puerto Rico")) %>%
  st_transform(5070) %>%
  st_union() %>%
  ms_simplify(keep = 0.1)

v_conus = st_voronoi(conus) %>%
  st_sf() %>%
  st_cast() %>%
  mutate(id = 1:n()) %>%
  st_intersection(usa)

t_conus = st_triangulate(conus) %>%
  st_sf() %>%
  st_cast() %>%
  mutate(id = 1:n()) %>%
  st_intersection(usa)

gs_conus = st_make_grid(conus, n = 70, square = TRUE) %>%
  st_sf() %>%
  st_cast() %>%
  mutate(id = 1:n())

gh_conus = st_make_grid(conus, n = 70, square = FALSE) %>%
  st_sf() %>%
  st_cast() %>%
  mutate(id = 1:n())

dams <- read_excel("data/NID2019_U.xlsx") %>%
  filter(!is.na(LONGITUDE) & !is.na(LATITUDE)) %>%
  st_as_sf(coords = c("LONGITUDE", "LATITUDE"), crs = 4326) %>%
  st_transform(5070)

point_in_polygon(dams, v_conus, "id")
point_in_polygon(dams, t_conus, "id")
point_in_polygon(dams, gs_conus, "id")
point_in_polygon(dams, gh_conus, "id")
point_in_polygon(dams, sf_conus, "id")


st_join(sf_conus, dams) %>%
  st_drop_geometry() %>%
  count(get(name)) %>%
  rename(c("id" = "get(name)")) %>%
  left_join(polygon) %>%
  st_as_sf()

