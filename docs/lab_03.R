library(tidyverse)
library(sf)
library(units)

# Data
library(USAboundaries)
library(rnaturalearth)

# Visualization
library(gghighlight)
library(ggrepel)
library(knitr)

eqdc = '+proj=eqdc +lat_0=40 +lon_0=-96 +lat_1=20 +lat_2=60 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs'

conus = USAboundaries::us_states(resolution = "low") %>% 
  filter(!name %in% c("Alaska", "Hawaii", "District of Columbia", "Puerto Rico"))

conus <- st_transform(conus, eqdc)

world <- rnaturalearth::countries110 %>% 
  st_as_sf() %>% 
  filter(admin %in% c("United States of America", "Mexico", "Canada")) %>% 
  st_transform(eqdc)

library(readr)
cities <- read_csv("data/uscities.csv") %>% 
  right_join(conus, by = "state_name") %>% 
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>% 
  st_transform(eqdc)

conus_b = st_union(conus) %>%
  st_cast("MULTILINESTRING")

cities = cities %>%
  mutate(dist_to_border = st_distance(cities, conus_b),
         dist_to_border = units::set_units(dist_to_border,"km"),
         dist_to_border = units::drop_units(dist_to_border))

cities5 = cities %>% 
  slice_max(dist_to_border, n=5) %>% 
  as_tibble() %>% 
  select(city, state_name, dist_to_border)

knitr::kable(cities5, 
             caption = "Top 5 Greatest Distance to Border",
             col.names = c("City", "State", "Greatest Distance to Border"))

conus_s = st_combine(conus) %>%
  st_cast("MULTILINESTRING")

cities = cities %>%
  mutate(dist_to_stborder = st_distance(cities, conus_s),
         dist_to_stborder = units::set_units(dist_to_stborder,"km"),
         dist_to_stborder = units::drop_units(dist_to_stborder))

cities_st5 = cities %>% 
  slice_max(dist_to_stborder, n=5) %>% 
  as_tibble() %>% 
  select(city, state_name, dist_to_stborder)

knitr::kable(cities_st5, 
             caption = "Top 5 Greatest Distance to State Border",
             col.names = c("City", "State", "Greatest Distance to Border"))

mexico <- world %>% 
  filter(admin == "Mexico")

cities = cities %>%
  mutate(dist_to_mexico = st_distance(cities, mexico),
         dist_to_mexico = units::set_units(dist_to_mexico,"km"),
         dist_to_mexico = units::drop_units(dist_to_mexico))

cities_mx5 = cities %>% 
  slice_max(dist_to_mexico, n=5) %>% 
  as_tibble() %>% 
  select(city, state_name, dist_to_mexico)

knitr::kable(cities_mx5, 
             caption = "Top 5 Greatest Distance to Mexico Border",
             col.names = c("City", "State", "Greatest Distance to Border"))

canada <- world %>% 
  filter(admin == "Canada")

cities = cities %>%
  mutate(dist_to_canada = st_distance(cities, canada),
         dist_to_canada = units::set_units(dist_to_canada,"km"),
         dist_to_canada = units::drop_units(dist_to_canada))

cities_can5 = cities %>% 
  slice_max(dist_to_canada, n=5) %>% 
  as_tibble() %>% 
  select(city, state_name, dist_to_canada)

knitr::kable(cities_can5, 
             caption = "Top 5 Greatest Distance to Canada Border",
             col.names = c("City", "State", "Greatest Distance to Border"))
