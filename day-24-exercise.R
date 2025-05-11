# spatial data science
library(tidyverse)
library(sf)
library(units)

# Data
library(AOI)
library(rnaturalearthdata)
library(USAboundaries)
library(USAboundariesData)

# Visualization
library(gghighlight)
library(ggrepel)
library(knitr)
library(flextable)

eqdc <- '+proj=eqdc +lat_0=40 +lon_0=-96 +lat_1=20 +lat_2=60 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs'

# Cities Data
cities <- readr::read_csv("data/uscities.csv", show_col_types = FALSE) %>%
  filter(!state_id %in% c("PR")) %>% #filter out PR
  st_as_sf(coords = c("lng","lat"), crs = 4326) %>%
  st_transform(eqdc)

larimer <- aoi_get(state = "CO", county = "Larimer") %>% 
  st_transform(eqdc)

larimer_cities <- st_filter(cities, larimer, .predicate = st_within)

big3 <- larimer_cities %>% 
  select(city, population) %>% 
  slice_max(population, n = 3)

Biggest_Cities <- ggplot() +
  geom_sf(data = larimer) + 
  geom_sf(data = larimer_cities) +
  geom_sf(data = big3, color = "indianred", size = 4) +
  geom_label_repel(data = big3,
                   aes(geometry = geometry, label = city),
                   size = 3, box.padding = .2, min.segment.length = 0,
                   stat = "sf_coordinates") +
  theme_void()

ggsave(filename = "biggest_cities.png", plot = Biggest_Cities)
