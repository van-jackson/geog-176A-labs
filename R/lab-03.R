#################################################
## Project: Lab 03
## Script purpose: Week 3
## Date: August 20

library(tidyverse)
library(sf)
library(units)

US_Cities_Data = readr::read_csv("data/uscities.csv")

region = data.frame(region = state.region, state_name = state.name)

south = right_join(USAboundaries::us_states(), region, by = "state_name") %>%
  filter(region == "South")

cities = readr::read_csv("data/uscities.csv") %>%
  st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
  st_filter(south, .predicate = st_intersects)

plot(south$geometry)
plot(cities$geometry, add = TRUE, pch = 16, cex = .1)

south_c = st_combine(south) %>%
  st_cast("MULTILINESTRING")

south_c = st_transform(south_c, 5070)

x = st_distance(cities, south_c, which = "Euclidean")
