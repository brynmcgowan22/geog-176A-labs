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

big_cities = cities %>%
  slice_max(population, n = 10)
  

ggplot() +
  geom_sf(data = world) +
  geom_sf(data = conus_s, linetype = "dotted") +
  geom_sf(data = conus_b, size = .75) +
  geom_sf(data = big_cities, col = "darkred", size = 1) +
  ggrepel::geom_label_repel(
    data = big_cities,
    aes(label = city, geometry = geometry),
    stat = "sf_coordinates",
    size = 3)

cities5sf = cities %>% 
  slice_max(dist_to_border, n=5)

ggplot() +
  geom_sf(data = cities, aes(color = dist_to_border)) +
  scale_color_gradient(high = "springgreen1", low = "slateblue") +
  geom_sf(data = cities5sf) +
  ggrepel::geom_label_repel(
    data = cities5sf,
    aes(label = city, geometry = geometry),
    stat = "sf_coordinates",
    size = 3)

colors()

cities_st5sf = cities %>% 
  slice_max(dist_to_stborder, n=5)

ggplot() +
  geom_sf(data = cities, aes(color = dist_to_stborder)) +
  scale_color_gradient(high = "darkred", low = "navy") +
  geom_sf(data = cities_st5sf) +
  ggrepel::geom_label_repel(
    data = cities_st5sf,
    aes(label = city, geometry = geometry),
    stat = "sf_coordinates",
    size = 3)

cities <- cities %>% 
  mutate(dist_to_mxcan = abs(dist_to_canada - dist_to_mexico))

cities_mxcan5 <- cities %>% 
  filter(dist_to_mxcan < 100) %>% 
  slice_max(population, n = 5)

ggplot() +
  geom_sf(data = cities, aes(color = dist_to_mxcan)) +
  scale_color_gradient(high = "darkred", low = "navy") +
  gghighlight(dist_to_mxcan < 100) +
  geom_sf(data = cities_mxcan5) +
  ggrepel::geom_label_repel(
    data = cities_mxcan5,
    aes(label = city, geometry = geometry),
    stat = "sf_coordinates",
    size = 3)

border_pop <- cities %>% 
  filter(dist_to_border < 160) %>% 
  as_tibble() %>% 
  summarise(count = n(), population = sum(population))

total_pop <- cities %>% 
  as_tibble() %>% 
  summarise(count = n(), population = sum(population))

percent = border_pop$population / total_pop$population * 100

knitr::kable(tibble(border_pop$population, total_pop$population, percent), 
             caption = "Percent of Population Less Than 100 Miles From Border",
             col.names = c("Border Population", "Total Population", "Percent"))

ggplot() +
  geom_sf(data = (border_pop <- cities %>% 
                    filter(dist_to_border < 160)), aes(color = dist_to_border)) +
  scale_color_gradient(high = "darkred", low = "orange") +
  gghighlight(dist_to_border < 160) +
  geom_sf(data = (border_pop <- cities %>% 
                    filter(dist_to_border < 160) %>% 
                    group_by(state_name) %>% 
                    slice_max(population, n = 1))) +
  ggrepel::geom_label_repel(
    data = (border_pop <- cities %>% 
      filter(dist_to_border < 160) %>% 
      group_by(state_name) %>% 
      slice_max(population, n = 1)),
    aes(label = city, geometry = geometry),
    stat = "sf_coordinates",
    size = 3)


