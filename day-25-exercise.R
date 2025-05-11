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

mississippi <- read_sf("Data/majorrivers_0_0") %>% 
  filter(NAME == "Mississippi") %>% 
  st_transform(5070)

counties <- aoi_get(state = "conus", county = "all") %>% 
  st_transform(5070)

miss_cts <- st_filter(counties, mississippi, .predicate = st_intersects) %>% 
  rename(county_name = name) %>% 
  select(county_name, state_name)

ggplot() +
  geom_sf(data = miss_cts) + 
  geom_sf(data = mississippi) +
  theme_void()

cities <- readr::read_csv("data/uscities.csv", show_col_types = FALSE) %>%
  filter(!state_id %in% c("PR")) %>% #filter out PR
  st_as_sf(coords = c("lng","lat"), crs = 4326) %>% 
  st_transform(5070)

miss_city <- st_join(miss_cts, cities) %>% 
  rename(county_name = county_name.x) %>% 
  group_by(county_name) %>% 
  summarise(population = sum(population)) %>% 
  ungroup()
  
miss_p <- ggplot() +
  geom_sf(data = miss_city, aes(fill = population)) + 
  geom_sf(data = mississippi, color = "white") +
  theme_void() +
  scale_fill_viridis_c(option = "D") +
  labs(
    fill = "Population"
  ) +
  theme(
        text = element_text(color = "gray50"))

miss_p

ggsave("imgs/mississippi_pop.png", miss_p)
