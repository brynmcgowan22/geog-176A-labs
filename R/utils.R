map_nearest_state = function(name, rows = 70) {

states = USAboundaries::us_states() %>%
  st_transform(5070)

state.of.interest = name

soi = filter(states, state_name == state.of.interest)

adjoining = st_filter(states, soi, .predicate = st_touches)

closest = st_make_grid(soi, n = rows, square = FALSE) %>%
  st_centroid() %>%
  st_sf() %>%
  st_join(adjoining, join = st_nearest_feature)

vor = closest %>%
  st_union() %>%
  st_voronoi() %>%
  st_cast() %>%
  st_sf() %>%
  st_join(closest) %>%
  group_by(state_name) %>%
  summarise() %>%
  st_intersection(soi)

leaflet() %>%
  addProviderTiles(providers$CartoDB) %>%
  addPolygons(data = st_transform(vor, 4326),
              fillColor = ~colorFactor("YlOrRd", state_name)(state_name), color = NA) %>%
  addPolygons(data = st_transform(soi, 4326),
              fillColor = "transparent", color = "black",
              group = "SOI") %>%
  addPolygons(data = st_transform(adjoining, 4326),
              fillColor = ~colorFactor("YlOrRd", state_name)(state_name), color = NA) %>%
  addLayersControl(overlayGroups = c("SOI"))
}

county_plot = function(plot_data, plot_title) {
  feat = plot_data %>%
    as_tibble() %>%
    select(id) %>%
    summarise(max(id))

  ggplot() +
    geom_sf(data = plot_data, color = "navy", size = 0.2) +
    aes(fillColor = "white") +
    labs(title = plot_title,
         caption = paste("Number of Features", feat)) +
    theme_void()
}

sum_tess = function(sf, desc){
  area = st_area(sf) %>%
      units::set_units("km^2") %>%
      units::drop_units()

    df = data.frame(description = desc,
                    count = nrow(sf),
                    mean_area = mean(area),
                    std_area = sd(area),
                    tot_area = sum(area))
    return(df)
}

point_in_polygon = function(points, polygon, c_name){
  st_join(polygon, points) %>%
    st_drop_geometry() %>%
    count(get(c_name)) %>%
    rename(c("id" = "get(c_name)")) %>%
    left_join(polygon) %>%
    st_as_sf()
}

pip_plot = function(plot_data, plot_title){
ggplot() +
  geom_sf(data = plot_data, aes(fill = log(n)), size = .2, col = NA) +
  scale_fill_viridis_c() +
  labs(title = plot_title,
       caption = paste(sum(plot_data$n))) +
  theme_void()
}

sum_tess2 = function(sf, desc) {
  area = st_area(sf) %>%
    units::set_units("km^2") %>%
    units::drop_units()

  df = data.frame(description = desc,
                  count = nrow(sf),
                  mean_area = mean(area),
                  std_area = sd(area),
                  tot_area = sum(area))
  return(df)
}

thresholding = function(x){ifelse(x <= 0, 1, NA)}
