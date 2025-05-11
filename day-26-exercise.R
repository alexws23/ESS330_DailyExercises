library(terra)
library(tidyverse)
library(sf)
library(AOI)
library(ggpubr)

url <- 'https://raw.githubusercontent.com/mikejohnson51/csu-ess-330/refs/heads/main/resources/foco-elev-cm.tif'

elev <- rast(glue::glue("/vsicurl/{url}"))

elev_ft <- elev*0.0328084

elev_df <- values(elev_ft, dataframe = TRUE)


den <- ggdensity(elev_df$dem,xlab = "Elevation (ft)", ylab = "Density", title = "Density Plot of Elevation in Fort Collins, CO")

ggsave("imgs/elev_density.png", den)
