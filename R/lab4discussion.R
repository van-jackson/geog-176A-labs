library(tidyverse)
library(sf)
library(leaflet)

states = USAboundaries::us_states()

tmp = states %>%
  filter(grepl("I", state_name))

plot(tmp$geometry, col = "red")




state.of.interest = "Pennsylvania"

soi = filter(states, state_name == state.of.interest)

adjoining = st_filter(states, soi, .predicate = st_touches)

closest = st_make_grid(soi, n = 70, square = TRUE) %>%
  st_centroid() %>%
  st_sf() %>%
  st_join(adjoining, join = st_nearest_feature)

unique(closest$state_name)

providers
leaflet() %>%
  addProviderTiles(providers$CartoDB) %>%
  addcircles(data = st_transform(closest, 4326), radius = 1)


